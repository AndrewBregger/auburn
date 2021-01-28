use hir::HirExprKind;
use ir::hir;
use oxide::{Object, OxModule, OxFunction, Section, Value, Vm, gc::{Allocator, Gc}, vm::OpCode};
use crate::{analysis::{self, Entity, EntityInfo, FunctionInfo}, ir::{self, ast::BinaryOp, hir::{HirExpr, HirExprPtr, HirFile, HirItem, HirItemPtr, HirStmt, HirStmtKind, HirStmtPtr, MirNode}}, system::{FileMap, FileId}, types::{Type, TypeKind}};
use ordered_float::OrderedFloat;

use std::{cell::RefCell, collections::HashMap, convert::TryInto, ops::Deref, rc::Rc};

struct FileContext<'ctx> {
    /// the this context is for.
    file: &'ctx HirFile,
    /// all of the global names for this file
    globals: HashMap<String, u8>,
    // constants: HashMap<Value, u16>,
    /// section of code for the top level expressions.
    section: Section,
    /// stack of function sections that are being generated
    function_stack: Vec<Gc<OxFunction>>,
    /// list of objects for this file.
    objects: Vec<Object>
}

impl<'ctx> FileContext<'ctx> {
    fn new(file: &'ctx HirFile, vm: &Vm) -> Self {
        Self {
            file,
            globals: HashMap::new(),
            section: Section::new(vm),
            function_stack: vec![],
            objects: vec![],
        }
    }
}


#[derive(Debug)]
pub enum BuildError {}
pub struct CodeGen<'vm, 'ctx> {
    file_map: &'ctx FileMap,
    vm: &'vm mut Vm,
    file_stack: Vec<FileId>,
    file_context: HashMap<FileId, FileContext<'ctx>>,
}

impl<'vm, 'ctx> CodeGen<'vm, 'ctx> {
    pub fn build(
        file_map: &FileMap,
        hir_file: &HirFile,
        // hir_module: &HirModule, // not implemented yet
        vm: &mut Vm,
    ) -> Result<Gc<OxModule>, BuildError> {
        let mut code_gen = CodeGen {
            file_map,
            vm,
            file_stack: vec![],
            file_context: HashMap::new(),
        };
        
       code_gen.build_module(hir_file)
    }

}

// impl<'ctx> CodeGen<'ctx> {
impl<'vm, 'ctx> CodeGen<'vm, 'ctx> {
    fn push_context(&mut self, file: &'ctx HirFile) {
        let file_id = file.id();
        let context = FileContext::new(file, &self.vm);
        self.file_context.insert(file_id, context);
    }

    fn current_context(&self) -> &FileContext {
        assert!(!self.file_stack.is_empty());
        let last = self.file_stack.last().unwrap();
        self.file_context.get(last).unwrap()
    }    

    fn current_context_mut(&mut self) -> &mut FileContext<'ctx> {
        assert!(!self.file_stack.is_empty());
        let last = self.file_stack.last().unwrap();
        self.file_context.get_mut(last).unwrap()
    }

    fn split_stmts(top_level_stmts: &[HirStmtPtr]) -> (Vec<Rc<RefCell<Entity>>>, Vec<HirStmtPtr>) {
        let mut items = vec![];
        let mut stmts = vec![];

        for stmt in top_level_stmts {
            match stmt.inner() {
                HirStmtKind::Item(item) => items.push(item.clone()),
                _ => stmts.push(stmt.clone()),
            }
        }

        (items, stmts)
    }
    
    fn get_file(&self, id: &FileId) -> &str {
        self.file_map.find(id).map(|f| f.name()).expect("failed to find file")
    }

    fn get_stem(&self, id: &FileId) -> &str {
        self.file_map.find(id).map(|f| f.stem()).expect("failed to find file")
    }
    
    fn emit_op(&mut self, op: OpCode) {
        self.emit(&[op as u8]);
    }

    fn emit_op_u8(&mut self, op: OpCode, arg: u8) {
        self.emit(&[op as u8, arg]);
    }

    fn emit_op_u16(&mut self, op: OpCode, args: u16) {
        let bytes = args.to_be_bytes();
        self.emit(&[op as u8, bytes[0], bytes[1]]);
    }

    fn emit(&mut self, data: &[u8]) {
        let context = self.current_context_mut();
        context.section.write_bytes(data);
    }
}

//impl<'ctx> CodeGen<'ctx> {
impl<'vm, 'ctx> CodeGen<'vm, 'ctx> {
    fn build_module(&mut self, hir_file: &'ctx HirFile) -> Result<Gc<OxModule>, BuildError> {
        self.file_stack.push(hir_file.id());

        self.push_context(hir_file);

        self.build_file(hir_file)?;

        let stem = hir_file.stem();
        
        let name = self.vm.allocate_string(stem);

        let section = {
            let context = self.current_context();
            context.section.clone()
        };

        let objects = Vec::new_in(self.vm.allocator());

        let code = self.vm.allocate_function(name.clone(), 0, section);

        let module = self.vm.allocate_module(name, code, objects);
        
       Ok(module)
    }

    fn build_file(&mut self, hir_file: &HirFile) -> Result<(), BuildError> {
        assert!(!self.file_stack.is_empty(), "file stack is empty, currently not processing a file");

        let (items, stmts) = Self::split_stmts(hir_file.stmts());
            
        let context = self.current_context_mut();

        for item in items.iter() {
            let entity_borrow = item.borrow();
            let name = entity_borrow.name();
            let idx = context.section.add_global();
            context.globals.entry(name.to_owned()).or_insert(idx);
        }

        std::mem::drop(context);


        for entity in items.into_iter() {
            self.handle_entity(&entity.borrow())?;
        }

        for stmt in stmts.into_iter() {
            self.handle_stmt(stmt.as_ref())?;
        }
        // eof, exit from the file here.
        self.emit_op(OpCode::Return);
        Ok(())
    }

    fn build_function(&mut self, name: &str, mir_function: &FunctionInfo) -> Result<(), BuildError> {
        let name_string = self.vm.allocate_string(name);
        Ok(())    
    }
}


//impl<'ctx> CodeGen<'ctx> {
impl<'vm, 'ctx> CodeGen<'vm, 'ctx> {
    fn handle_entity(&mut self, entity: &Entity) -> Result<(), BuildError> {
        let name = entity.name();
        match entity.kind() {
            EntityInfo::Structure(_) => { todo!() }
            EntityInfo::Function(function_info) => self.build_function(name, function_info)?,
            EntityInfo::Variable(_) => {}
            EntityInfo::AssociatedFunction(_) => { todo!() }
            EntityInfo::Param(_) => { todo!() }
            EntityInfo::SelfParam { mutable } => { todo!() }
            EntityInfo::Field(_) => { todo!() }
            EntityInfo::Unresolved(_) => {}
            EntityInfo::Resolving => {}
            EntityInfo::Primitive => {}
        }

        Ok(())
    }

    //fn handle_top_level_stmt(&mut self, stmt: &HirStmt) -> Result<(), BuildError> {
    //}


    fn handle_stmt(&mut self, stmt: &HirStmt) -> Result<(), BuildError> {
        match stmt.inner() {
            HirStmtKind::Expr(expr) => self.handle_expr(expr.as_ref())?,
            HirStmtKind::Item(entity) => self.handle_entity(&entity.borrow())?,
            HirStmtKind::Assignment(_) => { unimplemented!() }
            HirStmtKind::Print(expr) => {
                self.handle_expr(expr.as_ref())?;
                self.emit_op(OpCode::Print);
            }
        }

        Ok(())
    }

    fn handle_expr(&mut self, expr: &HirExpr) -> Result<(), BuildError> {
        let inner = expr.inner();
        let ty = expr.ty();
        match inner.kind() {
            HirExprKind::Integer(val) => {
                self.load_integer(ty, *val)?;
            }
            HirExprKind::Float(val) => {
                self.load_float(ty, *val)?;
            }
            HirExprKind::String(val) => {
                let ox_string = self.vm.allocate_string_ptr(val);
                let value = Value::String(ox_string);

                let context = self.current_context_mut();
                let idx = context.section.add_constant(value);
                context.section.write_arg(OpCode::LoadStr, idx);
            }
            HirExprKind::Char(_) => {}
            HirExprKind::Bool(val) => {
                let op = if *val {
                    OpCode::LoadTrue
                } else {
                    OpCode::LoadFalse
                };
                self.emit_op(op);
            }
            HirExprKind::Name(val) => { todo!() }
            HirExprKind::Binary(bin_expr) => {
               self.handle_expr(bin_expr.left.as_ref())?;
               self.handle_expr(bin_expr.right.as_ref())?;

                let op = if bin_expr.op.is_cmp() {
                    Self::binary_op_for_type(bin_expr.op, bin_expr.left.ty())
                } else {
                    Self::binary_op_for_type(bin_expr.op, ty.clone())
                };
                
                self.emit_op(op);
            }
            HirExprKind::Unary(un_expr) => {}
            HirExprKind::Field(_) => {}
            HirExprKind::Index(_) => {}
            HirExprKind::FieldAccess(_) => {}
            HirExprKind::Call(call_expr) => {}
            HirExprKind::Method(_) => {}
            HirExprKind::AssociatedFunction(_) => {}
            HirExprKind::Block(_) => {}
            HirExprKind::Tuple(_) => {}
            HirExprKind::Loop(loop_expr) => {}
            HirExprKind::While(while_expr) => {}
            HirExprKind::For(for_expr) => {}
            HirExprKind::If(if_expr) => {}
            HirExprKind::StructExpr(struct_expr) => {}
            HirExprKind::SelfLit => {}
            HirExprKind::Break => {}
            HirExprKind::Continue => {}
            HirExprKind::Return(return_expr) => {
                self.handle_expr(return_expr.as_ref())?;
                self.emit_op(OpCode::Return);
            }
        }

        Ok(())
    }

    fn load_integer(&mut self, ty: Rc<Type>, val: i64) -> Result<(), BuildError> {
        let (op, value) = match ty.kind() {
            TypeKind::U8 => {
                let val: u8 = val.try_into().unwrap();
                (OpCode::LoadU8, Value::U8(val))
            }
            TypeKind::U16 => {
                let val: u16 = val.try_into().unwrap();
                (OpCode::LoadU16, Value::U16(val))
            }
            TypeKind::U32 => {
                let val: u32 = val.try_into().unwrap();
                (OpCode::LoadU32, Value::U32(val))
            }
            TypeKind::U64 => {
                let val = val as u64;
                (OpCode::LoadU64, Value::U64(val))
            }
            TypeKind::I8 => {
                let val: i8 = val.try_into().unwrap();
                (OpCode::LoadI8, Value::I8(val))
            }
            TypeKind::I16 => {
                let val: i16 = val.try_into().unwrap();
                (OpCode::LoadI16, Value::I16(val))
            }
            TypeKind::Integer | TypeKind::I32 => {
                let val: i32 = val.try_into().unwrap();
                (OpCode::LoadI32, Value::I32(val))
            }
            TypeKind::I64 => (OpCode::LoadI64, Value::I64(val)),
            _ => panic!("Type Missmatch"),
        };
    
        let context = self.current_context_mut();
        let idx = context.section.add_constant(value);
        context.section.write_arg(op, idx);
        Ok(())
    }

    fn load_float(&mut self, ty: Rc<Type>, val: OrderedFloat<f64>) -> Result<(), BuildError> {
        let (op, value) = match ty.kind() {
            TypeKind::F32 => (OpCode::LoadF32, Value::F32(val.into_inner() as f32)),
            TypeKind::F64 => (OpCode::LoadF64, Value::F64(val.into_inner())),
            _ => panic!("Type Missmatch"),
        };

        let context = self.current_context_mut();
        let idx = context.section.add_constant(value);
        context.section.write_arg(op, idx);
        Ok(())
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
