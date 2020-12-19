use std::{collections::HashMap, convert::TryInto, ops::Deref, rc::Rc, unimplemented, vec};

use oxide::{vm::OpCode, OxFunction, OxString, Section, Value};

use crate::{
    analysis::{Entity, EntityInfo, EntityRef, FunctionInfo},
    ir::ast::BinaryOp,
    ir::hir::{
        Function, HirExpr, HirExprKind, HirFile, HirStmt, HirStmtKind, IfExprBranch, MirNode,
    },
    system::{FileId, FileMap},
    types::{Type, TypeKind},
    utils::{EntityPrinter, MirPrinter},
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct LocalId(pub usize);

#[derive(Debug, Clone)]
pub struct Context<'ctx> {
    files: &'ctx FileMap,
    file: &'ctx HirFile,
    global_map: HashMap<String, usize>,
    local_maps: Vec<HashMap<String, u8>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new(files: &'ctx FileMap, file: &'ctx HirFile) -> Self {
        Self {
            files,
            file,
            global_map: HashMap::new(),
            local_maps: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub enum GenError {}

type Result<T> = ::std::result::Result<T, GenError>;

/// facilitates the generation of byte code for a single file.
pub struct CodeGen<'ctx> {
    /// the stack of entities currently being processed.
    /// the top will be the current entity being processed.
    entity_stack: Vec<EntityRef>,
    ctx: Context<'ctx>,
    global_section: Section,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(ctx: Context<'ctx>) -> Self {
        Self {
            entity_stack: vec![],
            ctx,
            global_section: Section::new(),
        }
    }

    //                                             Gc<OxFunction>
    pub fn build(mut self) -> Result<Box<OxFunction>> {
        let value = self.build_string(
            self.ctx
                .files
                .get_path_by_id(&self.ctx.file.id())
                .map_or("<script>".to_string(), ToString::to_string),
        );

        self.prepopulate_globals();

        for entity in self.ctx.file.entities() {
            self.entity_stack.push(entity.clone());
            let borrow_ref = entity.borrow();
            self.gen_global(&borrow_ref)?;
            self.entity_stack.pop();
        }
        let globals = self.ctx.file.clone().globals().to_vec();
        for stmt in globals {
            Self::gen_stmt(stmt.as_ref(), &self.ctx, &mut self.global_section);
        }

        self.global_section.write_op(OpCode::Exit);
        Ok(Box::new(OxFunction::new(value, 0, self.global_section)))
    }

    fn build_string(&self, val: String) -> Box<OxString> {
        Box::new(OxString::new(val))
    }

    fn prepopulate_globals(&mut self) {
        for entity in self.ctx.file.entities() {
            let entity_borrow = entity.deref().borrow();
            let name = entity_borrow.name();
            let idx = self.global_section.add_global(Value::Unit);
            self.ctx
                .global_map
                .entry(name.to_owned())
                .or_insert(idx as usize);
        }
    }

    fn gen_global(&mut self, entity: &Entity) -> Result<()> {
        let value = match entity.kind() {
            EntityInfo::Function(function) => {
                let function = self.gen_function(entity.name(), function)?;
                Value::Function(function)
            }
            // EntityInfo::AssociatedFunction(_) => {}
            // EntityInfo::Variable(_) => {}
            // EntityInfo::Param(_) => {}
            // EntityInfo::SelfParam { mutable } => {}
            // EntityInfo::Field(_) => {}
            _ => {
                todo!()
            }
        };

        let idx = self.ctx.global_map[entity.name()];
        self.global_section.set_global(idx, value);
        Ok(())
    }

    fn gen_function_body(&mut self, body_expr: &HirExpr, section: &mut Section) {
        Self::gen_expr(body_expr, &mut self.ctx, section);
        section.write_op(OpCode::Return);
    }

    fn gen_stmt(stmt: &HirStmt, ctx: &Context, section: &mut Section) {
        match stmt.inner() {
            HirStmtKind::Expr(expr) => {
                let meta = expr.inner().meta();
                Self::gen_expr(expr, ctx, section);
                // we need to pop off the result of the return of the result isn't being used.
                if meta.is_call && !meta.uses_result {
                    section.write_op(OpCode::Pop);
                }
            }
            HirStmtKind::Assignment(_) => {}
            HirStmtKind::Item(_) => {}
        }
    }

    fn gen_expr(expr: &HirExpr, ctx: &Context, section: &mut Section) {
        let inner = expr.inner();
        let ty = expr.ty();
        match inner.kind() {
            HirExprKind::Integer(val) => {
                let (op, value) = match ty.kind() {
                    TypeKind::U8 => {
                        let val: u8 = (*val).try_into().unwrap();
                        (OpCode::LoadU8, Value::U8(val))
                    }
                    TypeKind::U16 => {
                        let val: u16 = (*val).try_into().unwrap();
                        (OpCode::LoadU16, Value::U16(val))
                    }
                    TypeKind::U32 => {
                        let val: u32 = (*val).try_into().unwrap();
                        (OpCode::LoadU32, Value::U32(val))
                    }
                    TypeKind::U64 => {
                        let val = *val as u64;
                        (OpCode::LoadU64, Value::U64(val))
                    }
                    TypeKind::I8 => {
                        let val: i8 = (*val).try_into().unwrap();
                        (OpCode::LoadI8, Value::I8(val))
                    }
                    TypeKind::I16 => {
                        let val: i16 = (*val).try_into().unwrap();
                        (OpCode::LoadI16, Value::I16(val))
                    }
                    TypeKind::Integer | TypeKind::I32 => {
                        let val: i32 = (*val).try_into().unwrap();
                        (OpCode::LoadI32, Value::I32(val))
                    }
                    TypeKind::I64 => (OpCode::LoadI64, Value::I64(*val)),
                    _ => panic!("Type Missmatch"),
                };

                let idx = section.add_constant(value);
                section.write_load(op, idx);
            }
            HirExprKind::Float(val) => {
                let (op, value) = match ty.kind() {
                    TypeKind::F32 => (OpCode::LoadF32, Value::F32(val.into_inner() as f32)),
                    TypeKind::F64 => (OpCode::LoadF64, Value::F64(val.into_inner())),
                    _ => panic!("Type Missmatch"),
                };

                let idx = section.add_constant(value);
                section.write_load(op, idx);
            }
            HirExprKind::String(val) => {}
            HirExprKind::Char(val) => {}
            HirExprKind::Bool(val) => {
                if *val {
                    section.write_op(OpCode::LoadTrue);
                } else {
                    section.write_op(OpCode::LoadFalse);
                }
            }
            HirExprKind::Name(val) => {
                let entity_borrow = val.deref().borrow();
                EntityPrinter::print(&entity_borrow);
                match entity_borrow.kind() {
                    EntityInfo::Function(_) => {
                        if let Some(indx) = ctx.global_map.get(entity_borrow.name()) {
                            section.write_load(OpCode::LoadGlobal, (*indx).try_into().unwrap());
                        } else {
                            unimplemented!()
                        }
                    }
                    EntityInfo::Variable(variable_info) => {
                        if variable_info.global {
                            if let Some(indx) = ctx.global_map.get(entity_borrow.name()) {
                                section.write_load(OpCode::LoadGlobal, (*indx).try_into().unwrap());
                            } else {
                                unimplemented!()
                            }
                        }
                    }
                    EntityInfo::Param(param) => {
                        section.write_load(OpCode::LoadLocal, param.index.try_into().unwrap())
                    }
                    // EntityInfo::AssociatedFunction(_) => {}
                    // EntityInfo::SelfParam { mutable } => {}
                    // EntityInfo::Field(_) => {}
                    // EntityInfo::Structure(_) => {}
                    _ => {}
                }
            }
            HirExprKind::Binary(binary_expr) => {
                Self::gen_expr(binary_expr.left.as_ref(), ctx, section);
                Self::gen_expr(binary_expr.right.as_ref(), ctx, section);
                let op = if binary_expr.op.is_cmp() {
                    Self::binary_op_for_type(binary_expr.op, binary_expr.left.ty())
                } else {
                    Self::binary_op_for_type(binary_expr.op, ty.clone())
                };

                section.write_op(op);
            }
            HirExprKind::Unary(_) => {}
            HirExprKind::Field(_) => {}
            HirExprKind::Index(_) => {}
            HirExprKind::FieldAccess(_) => {}
            HirExprKind::Call(call_info) => {
                // call_info.
                Self::gen_expr(call_info.operand.as_ref(), ctx, section);
                // let op = section.last_op();
                for actual in &call_info.actuals {
                    Self::gen_expr(actual.as_ref(), ctx, section);
                }

                section.write_load(OpCode::Call, call_info.actuals.len().try_into().unwrap());
            }
            HirExprKind::Method(_) => {}
            HirExprKind::AssociatedFunction(_) => {}
            HirExprKind::Block(block_expr) => {
                for stmt in block_expr.stmts.as_slice() {
                    Self::gen_stmt(stmt.as_ref(), ctx, section);
                }
            }
            HirExprKind::Tuple(_) => {}
            HirExprKind::Loop(_) => {}
            HirExprKind::While(_) => {}
            HirExprKind::For(_) => {}
            HirExprKind::If(if_expr) => {
                let mut offsets = vec![];
                for branch in if_expr.branches.as_slice() {
                    match branch {
                        IfExprBranch::Conditional { cond, body, first } => {
                            Self::gen_expr(cond.as_ref(), ctx, section);
                            let conditional_offset = section.write_jmp(OpCode::JmpFalse);
                            Self::gen_expr(body.as_ref(), ctx, section);
                            MirPrinter::print_expr(body.as_ref());
                            section.patch_jmp(conditional_offset);
                            offsets.push(section.write_jmp(OpCode::Jmp));
                        }
                        IfExprBranch::Unconditional { body } => {
                            Self::gen_expr(body.as_ref(), ctx, section);
                            MirPrinter::print_expr(body.as_ref());
                        }
                    }
                }
                offsets.iter().for_each(|offset| section.patch_jmp(*offset));
            }
            HirExprKind::StructExpr(_) => {}
            HirExprKind::SelfLit => {}
            HirExprKind::Break => {}
            HirExprKind::Continue => {}
            HirExprKind::Return(_) => {}
        }
    }

    fn gen_function(&mut self, name: &str, mir_function: &FunctionInfo) -> Result<Box<OxFunction>> {
        let mut section = Section::new();
        let name = self.build_string(name.to_owned());
        self.gen_function_body(mir_function.body.as_ref(), &mut section);
        Ok(Box::new(OxFunction::new(
            name,
            mir_function.params.elements().len().try_into().unwrap(),
            section,
        )))
    }

    fn binary_op_for_type(op: BinaryOp, ty: Rc<Type>) -> OpCode {
        match op {
            BinaryOp::Plus => match ty.kind() {
                TypeKind::U8 => OpCode::AddU8,
                TypeKind::U16 => OpCode::AddU16,
                TypeKind::U32 => OpCode::AddU32,
                TypeKind::U64 => OpCode::AddU64,
                TypeKind::I8 => OpCode::AddI8,
                TypeKind::I16 => OpCode::AddI16,
                TypeKind::Integer | TypeKind::I32 => OpCode::AddI32,
                TypeKind::I64 => OpCode::AddI64,
                TypeKind::Float | TypeKind::F32 => OpCode::AddF32,
                TypeKind::F64 => OpCode::AddF64,
                // TypeKind::Char => {}
                // TypeKind::String => {}
                _ => todo!("{}", ty),
            },
            BinaryOp::Minus => match ty.kind() {
                TypeKind::U8 => OpCode::SubU8,
                TypeKind::U16 => OpCode::SubU16,
                TypeKind::U32 => OpCode::SubU32,
                TypeKind::U64 => OpCode::SubU64,
                TypeKind::I8 => OpCode::SubI8,
                TypeKind::I16 => OpCode::SubI16,
                TypeKind::Integer | TypeKind::I32 => OpCode::SubI32,
                TypeKind::I64 => OpCode::SubI64,
                TypeKind::Float | TypeKind::F32 => OpCode::SubF32,
                TypeKind::F64 => OpCode::SubF64,
                // TypeKind::Char => {}
                // TypeKind::String => {}
                _ => todo!("{}", ty),
            },
            BinaryOp::Astrick => match ty.kind() {
                TypeKind::U8 => OpCode::MultU8,
                TypeKind::U16 => OpCode::MultU16,
                TypeKind::U32 => OpCode::MultU32,
                TypeKind::U64 => OpCode::MultU64,
                TypeKind::I8 => OpCode::MultI8,
                TypeKind::I16 => OpCode::MultI16,
                TypeKind::Integer | TypeKind::I32 => OpCode::MultI32,
                TypeKind::I64 => OpCode::MultI64,
                TypeKind::Float | TypeKind::F32 => OpCode::MultF32,
                TypeKind::F64 => OpCode::MultF64,
                _ => todo!("{}", ty),
            },
            BinaryOp::Slash => match ty.kind() {
                TypeKind::U8 => OpCode::DivU8,
                TypeKind::U16 => OpCode::DivU16,
                TypeKind::U32 => OpCode::DivU32,
                TypeKind::U64 => OpCode::DivU64,
                TypeKind::I8 => OpCode::DivI8,
                TypeKind::I16 => OpCode::DivI16,
                TypeKind::Integer | TypeKind::I32 => OpCode::DivI32,
                TypeKind::I64 => OpCode::DivI64,
                TypeKind::Float | TypeKind::F32 => OpCode::DivF32,
                TypeKind::F64 => OpCode::DivF64,
                _ => todo!("{}", ty),
            },
            BinaryOp::Less => match ty.kind() {
                TypeKind::U8 => OpCode::LessU8,
                TypeKind::U16 => OpCode::LessU16,
                TypeKind::U32 => OpCode::LessU32,
                TypeKind::U64 => OpCode::LessU64,
                TypeKind::I8 => OpCode::LessI8,
                TypeKind::I16 => OpCode::LessI16,
                TypeKind::Integer | TypeKind::I32 => OpCode::LessI32,
                TypeKind::I64 => OpCode::LessI64,
                TypeKind::Float | TypeKind::F32 => OpCode::LessF32,
                TypeKind::F64 => OpCode::LessF64,
                _ => todo!("{}", ty),
            },

            BinaryOp::Greater => match ty.kind() {
                TypeKind::U8 => OpCode::GreaterU8,
                TypeKind::U16 => OpCode::GreaterU16,
                TypeKind::U32 => OpCode::GreaterU32,
                TypeKind::U64 => OpCode::GreaterU64,
                TypeKind::I8 => OpCode::GreaterI8,
                TypeKind::I16 => OpCode::GreaterI16,
                TypeKind::Integer | TypeKind::I32 => OpCode::GreaterI32,
                TypeKind::I64 => OpCode::GreaterI64,
                TypeKind::Float | TypeKind::F32 => OpCode::GreaterF32,
                TypeKind::F64 => OpCode::GreaterF64,
                _ => todo!("{}", ty),
            },

            BinaryOp::LessEq => match ty.kind() {
                TypeKind::U8 => OpCode::LessEqU8,
                TypeKind::U16 => OpCode::LessEqU16,
                TypeKind::U32 => OpCode::LessEqU32,
                TypeKind::U64 => OpCode::LessEqU64,
                TypeKind::I8 => OpCode::LessEqI8,
                TypeKind::I16 => OpCode::LessEqI16,
                TypeKind::Integer | TypeKind::I32 => OpCode::LessEqI32,
                TypeKind::I64 => OpCode::LessEqI64,
                TypeKind::Float | TypeKind::F32 => OpCode::LessEqF32,
                TypeKind::F64 => OpCode::LessEqF64,
                _ => todo!("{}", ty),
            },

            BinaryOp::GreaterEq => match ty.kind() {
                TypeKind::U8 => OpCode::GreaterEqU8,
                TypeKind::U16 => OpCode::GreaterEqU16,
                TypeKind::U32 => OpCode::GreaterEqU32,
                TypeKind::U64 => OpCode::GreaterEqU64,
                TypeKind::I8 => OpCode::GreaterEqI8,
                TypeKind::I16 => OpCode::GreaterEqI16,
                TypeKind::Integer | TypeKind::I32 => OpCode::GreaterEqI32,
                TypeKind::I64 => OpCode::GreaterEqI64,
                TypeKind::Float | TypeKind::F32 => OpCode::GreaterEqF32,
                TypeKind::F64 => OpCode::GreaterEqF64,
                _ => todo!("{}", ty),
            },

            BinaryOp::Ampersand => match ty.kind() {
                // TypeKind::U8 => OpCode::BinaryAndU8,
                // TypeKind::U16 => OpCode::BinaryAndU16,
                // TypeKind::U32 => OpCode::BinaryAndU32,
                // TypeKind::U64 => OpCode::BinaryAndU64,
                // TypeKind::I8 => OpCode::BinaryAndI8,
                // TypeKind::I16 => OpCode::BinaryAndI16,
                // TypeKind::Integer | TypeKind::I32 => OpCode::BinaryAndI32,
                // TypeKind::I64 => OpCode::BinaryAndI64,
                _ => todo!(),
            },

            BinaryOp::Pipe => match ty.kind() {
                // TypeKind::U8 => OpCode::BinaryOrU8,
                // TypeKind::U16 => OpCode::BinaryOrU16,
                // TypeKind::U32 => OpCode::BinaryOrU32,
                // TypeKind::U64 => OpCode::BinaryOrU64,
                // TypeKind::I8 => OpCode::BinaryOrI8,
                // TypeKind::I16 => OpCode::BinaryOrI16,
                // TypeKind::Integer | TypeKind::I32 => OpCode::BinaryOrI32,
                // TypeKind::I64 => OpCode::BinaryOrI64,
                _ => todo!(),
            },

            BinaryOp::Percent => match ty.kind() {
                // TypeKind::U8 => OpCode::PercentU8,
                // TypeKind::U16 => OpCode::PercentU16,
                // TypeKind::U32 => OpCode::PercentU32,
                // TypeKind::U64 => OpCode::PercentU64,
                // TypeKind::I8 => OpCode::PercentI8,
                // TypeKind::I16 => OpCode::PercentI16,
                // TypeKind::Integer | TypeKind::I32 => OpCode::PercentI32,
                // TypeKind::I64 => OpCode::PercentI64,
                _ => todo!(),
            },

            BinaryOp::EqualEqual => match ty.kind() {
                // TypeKind::U8 => OpCode::EqEqU8,
                // TypeKind::U16 => OpCode::EqEqU16,
                // TypeKind::U32 => OpCode::EqEqU32,
                // TypeKind::U64 => OpCode::EqEqU64,
                // TypeKind::I8 => OpCode::EqEqI8,
                // TypeKind::I16 => OpCode::EqEqI16,
                // TypeKind::Integer | TypeKind::I32 => OpCode::EqEqI32,
                // TypeKind::I64 => OpCode::EqEqI64,
                // TypeKind::Float | TypeKind::F32 => OpCode::EqEqF32,
                // TypeKind::F64 => OpCode::EqEqF64,
                _ => todo!(),
            },

            BinaryOp::BangEqual => match ty.kind() {
                // TypeKind::U8 => OpCode::NotEqU8,
                // TypeKind::U16 => OpCode::NotEqU16,
                // TypeKind::U32 => OpCode::NotEqU32,
                // TypeKind::U64 => OpCode::NotEqU64,
                // TypeKind::I8 => OpCode::NotEqI8,
                // TypeKind::I16 => OpCode::NotEqI16,
                // TypeKind::Integer | TypeKind::I32 => OpCode::NotEqI32,
                // TypeKind::I64 => OpCode::NotEqI64,
                // TypeKind::Float | TypeKind::F32 => OpCode::NotEqF32,
                // TypeKind::F64 => OpCode::NotEqF64,
                _ => todo!(),
            },

            BinaryOp::LessLess => match ty.kind() {
                // TypeKind::U8 => OpCode::LessLessU8,
                // TypeKind::U16 => OpCode::LessLessU16,
                // TypeKind::U32 => OpCode::LessLessU32,
                // TypeKind::U64 => OpCode::LessLessU64,
                // TypeKind::I8 => OpCode::LessLessI8,
                // TypeKind::I16 => OpCode::LessLessI16,
                // TypeKind::Integer | TypeKind::I32 => OpCode::LessLessI32,
                // TypeKind::I64 => OpCode::LessLessI64,
                // TypeKind::Float | TypeKind::F32 => OpCode::LessLessF32,
                // TypeKind::F64 => OpCode::LessLessF64,
                _ => todo!(),
            },

            BinaryOp::GreaterGreater => match ty.kind() {
                // TypeKind::U8 => OpCode::GreaterGreaterU8,
                // TypeKind::U16 => OpCode::GreaterGreaterU16,
                // TypeKind::U32 => OpCode::GreaterGreaterU32,
                // TypeKind::U64 => OpCode::GreaterGreaterU64,
                // TypeKind::I8 => OpCode::GreaterGreaterI8,
                // TypeKind::I16 => OpCode::GreaterGreaterI16,
                // TypeKind::Integer | TypeKind::I32 => OpCode::GreaterGreaterI32,
                // TypeKind::I64 => OpCode::GreaterGreaterI64,
                // TypeKind::Float | TypeKind::F32 => OpCode::GreaterGreaterF32,
                // TypeKind::F64 => OpCode::GreaterF64,
                _ => todo!(),
            },
        }
    }
}
