mod file_context;
mod type_helpers;

use crate::{
    analysis::{Entity, EntityInfo, FunctionInfo, StructureInfo, VariableInfo},
    ir::{
        self,
        ast::NodeType,
        hir::{
            Assignment, BlockExpr, FieldExpr, HirExpr, HirFile, HirStmt, HirStmtKind, HirStmtPtr,
            IfExpr, IfExprBranch, MirNode, StructExpr, WhileExpr,
        },
    },
    system::{FileId, FileMap},
    types::{Type, TypeKind},
};
use file_context::FileContext;
use hir::HirExprKind;
use ir::hir;
use oxide::{gc::Gc, vm::OpCode, OxFunction, OxModule, OxStruct, Section, Value, Vm};

use ordered_float::OrderedFloat;
use std::{cell::RefCell, collections::HashMap, convert::TryInto, ops::Deref, rc::Rc};

use self::file_context::GlobalInfo;

macro_rules! save_state {
    ($field:expr, $val:expr, $op:expr) => {
        let saved = $field;
        $field = $val;
        $op;
        $field = saved;
    };
}

#[derive(thiserror::Error, Debug)]
pub enum BuildError {
    #[error("{0}")]
    FeatureNotSupported(String),

    #[error("attempting to add a not function as a method")]
    SubEntityNotEntity,

    #[error("entry function for module not found")]
    ModuleEntryNotFound,

    #[error("unable to cast literal of type '{0}' to value")]
    InvalidLiteralCast(Rc<Type>),
}

pub struct CodeGen<'vm, 'ctx> {
    #[allow(dead_code)]
    file_map: &'ctx FileMap,
    vm: &'vm mut Vm,
    file_stack: Vec<FileId>,
    file_context: HashMap<FileId, FileContext<'ctx>>,
    scope_index: usize,
    handling_params: bool,
    result_used: bool,
    #[allow(dead_code)]
    is_function_scope: bool,
}

impl<'vm, 'ctx> CodeGen<'vm, 'ctx> {
    pub fn build(
        file_map: &FileMap,
        hir_file: &HirFile,
        // hir_module: &HirModule, // not implemented yet
        vm: &mut Vm,
    ) -> Result<Gc<OxModule>, BuildError> {
        // all gc objects that are allocated during code generation should never be deallocated.
        // Yes, this is a hack. Once a better solution is found this will be how it works.
        vm.force_no_collection(true);
        let mut code_gen = CodeGen {
            file_map,
            vm,
            file_stack: vec![],
            file_context: HashMap::new(),
            scope_index: 0,
            handling_params: false,
            result_used: false,
            is_function_scope: false,
        };

        let module = code_gen.build_module(hir_file);
        vm.force_no_collection(false);

        module
    }
}

// impl<'ctx> CodeGen<'ctx> {
impl<'vm, 'ctx> CodeGen<'vm, 'ctx> {
    fn push_context(&mut self, file: &'ctx HirFile) {
        let file_id = file.id();
        let context = FileContext::new(file, &mut self.vm);
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

    fn current_section(&self) -> &Section {
        let context = self.current_context();
        context.current_section()
    }

    fn current_section_mut(&mut self) -> &mut Section {
        let context = self.current_context_mut();
        context.current_section_mut()
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

    #[allow(dead_code)]
    fn get_file(&self, id: &FileId) -> &str {
        self.file_map
            .find(id)
            .map(|f| f.name())
            .expect("failed to find file")
    }

    #[allow(dead_code)]
    fn get_stem(&self, id: &FileId) -> &str {
        self.file_map
            .find(id)
            .map(|f| f.stem())
            .expect("failed to find file")
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
        let section = self.current_section_mut();
        section.write_bytes(data);
    }

    fn emit_jmp(&mut self, op: OpCode) -> usize {
        let section = self.current_section_mut();
        section.write_bytes(&[op as u8, 0xff, 0xff]);
        section.len() - 2
    }

    fn emit_patch(&mut self, offset: usize) {
        let section = self.current_section_mut();
        section.patch_jmp(offset);
    }

    fn push_scope(&mut self) {
        self.scope_index += 1;
    }

    fn pop_scope(&mut self) {
        self.scope_index -= 1;
    }
}

//impl<'ctx> CodeGen<'ctx> {
impl<'vm, 'ctx> CodeGen<'vm, 'ctx> {
    fn build_module(&mut self, hir_file: &'ctx HirFile) -> Result<Gc<OxModule>, BuildError> {
        self.file_stack.push(hir_file.id());

        self.push_context(hir_file);

        self.build_file(hir_file)?;

        let stem = hir_file.stem();
        let name_obj = self.vm.new_string_from_str(stem);

        let context = self
            .file_context
            .remove(&hir_file.id())
            .expect("unable to remove file context");

        // // book keeping for the next allocation. This makes sure that if there is a collection,
        // // the list of objects and name will not be collected.
        // self.vm.compiler_address.push(name_obj.buffer().ptr());
        // self.vm.compiler_address.push(context.values.ptr());

        let mut objects = self.vm.vec_with_capacity::<Value>(context.values.len());
        objects.extend_from_slice(context.values.as_slice());

        if let Some(entry) = hir_file.get_entry() {
            let borrow = entry.deref().borrow();
            let name = borrow.name();
            if let Some(global_info) = context.globals.get(name) {
                println!("building new module");
                // self.vm.force_no_collection(true);
                let module = self
                    .vm
                    .new_entry_module(name_obj, global_info.object_idx, objects);
                // self.vm.force_no_collection(false);
                Ok(module)
            } else {
                println!("failed to find global entity");
                Err(BuildError::ModuleEntryNotFound)
            }
        } else {
            Err(BuildError::ModuleEntryNotFound)
        }
    }

    fn build_file(&mut self, hir_file: &HirFile) -> Result<(), BuildError> {
        assert!(
            !self.file_stack.is_empty(),
            "file stack is empty, currently not processing a file"
        );

        self.push_scope();

        let (items, stmts) = Self::split_stmts(hir_file.stmts());

        let context = self.current_context_mut();
        for (idx, item) in items.iter().enumerate() {
            let entity_borrow = item.deref().borrow();
            let name = entity_borrow.name();
            context
                .globals
                .insert(name.to_owned(), GlobalInfo::new(name.to_owned(), idx));
        }

        std::mem::drop(context);

        for entity in items.into_iter() {
            match self.handle_entity(&entity.deref().borrow())? {
                Some(value) => {
                    let context = self.current_context_mut();
                    context.push_object(value);
                }
                None => {}
            }
        }

        for stmt in stmts.into_iter() {
            self.handle_stmt(stmt.as_ref())?;
        }

        self.push_scope();

        Ok(())
    }

    fn build_function(
        &mut self,
        name: &str,
        mir_function: &FunctionInfo,
    ) -> Result<Gc<OxFunction>, BuildError> {
        self.push_scope();

        let name_string = self.vm.new_string_from_str(name);
        let section = self.vm.new_section();
        let function = self
            .vm
            .new_function(name_string, mir_function.params.len() as u8, section);
        self.current_context_mut().push_function(function);

        self.handle_function_params(mir_function)?;

        self.handle_expr_inner(mir_function.body.as_ref(), true)?;
        self.emit_op(OpCode::Return);

        self.pop_scope();

        let context = self.current_context_mut();
        let function = if let Some(function_info) = context.current_function() {
            function_info.function.clone()
        } else {
            panic!("failed to get the current function")
        };
        Ok(function)
    }

    fn handle_function_params(&mut self, mir_function: &FunctionInfo) -> Result<(), BuildError> {
        self.handling_params = true;

        for (_, param_entity) in mir_function.params.elements() {
            self.handle_entity(&param_entity.deref().borrow())?;
        }

        self.handling_params = false;

        Ok(())
    }

    fn build_variable(
        &mut self,
        name: &str,
        variable_info: &VariableInfo,
    ) -> Result<(), BuildError> {
        if variable_info.global {
            // @todo: handle function calls for globals.
            let default = variable_info.default.as_ref().unwrap();
            if default.is_literal() {
                let value = self.literal_expr_to_value(default)?;
                self.current_context_mut().push_object(value);
            } else {
                panic!("non literals for globals is not supported by the vm")
            }
        } else {
            match (variable_info.spec.as_ref(), variable_info.default.as_ref()) {
                (Some(_spec), None) => {
                    todo!()
                }
                (_, Some(init)) => self.handle_expr(init.as_ref())?,
                (None, None) => {
                    unreachable!()
                }
            }
            self.build_local(name)?;
        }
        Ok(())
    }

    fn build_local(
        &mut self,
        name: &str, /* _variable_info: &VariableInfo */
    ) -> Result<(), BuildError> {
        let scope_index = self.scope_index;
        let context = self.current_context_mut();
        context.push_local(name, scope_index);
        if !self.handling_params {
            self.emit_op(OpCode::PushLocal);
        }
        Ok(())
    }

    fn build_struct(
        &mut self,
        name: &str,
        struct_info: &StructureInfo,
    ) -> Result<Gc<OxStruct>, BuildError> {
        // let mut methods = self.vm.new_vec::<Value>(struct_info.methods.len());
        let mut methods = self.vm.vec_with_capacity(struct_info.methods.len());

        for (_, element) in struct_info.methods.elements() {
            debug_assert!(element.deref().borrow().is_function());
            match self.handle_entity(&element.deref().borrow())? {
                Some(value) => match value {
                    Value::Function(inner) => {
                        methods.push(inner);
                    }
                    _ => return Err(BuildError::SubEntityNotEntity),
                },
                None => {}
            }
        }

        let name = self.vm.new_string_from_str(name);
        let structure = self.vm.new_struct(name, methods);
        Ok(structure)
    }
}

//impl<'ctx> CodeGen<'ctx> {
impl<'vm, 'ctx> CodeGen<'vm, 'ctx> {
    fn handle_entity(&mut self, entity: &Entity) -> Result<Option<Value>, BuildError> {
        let name = entity.name();
        match entity.kind() {
            EntityInfo::Structure(struct_info) => {
                let structure = self.build_struct(name, struct_info)?;
                Ok(Some(Value::from(structure)))
            }
            EntityInfo::Function(function_info) => {
                let function = self.build_function(name, function_info)?;
                Ok(Some(Value::from(function)))
            }
            EntityInfo::Variable(variable_info) => {
                self.build_variable(name, variable_info)?;
                Ok(None)
            }
            EntityInfo::AssociatedFunction(_) => {
                todo!()
            }
            EntityInfo::Param(_) => {
                self.build_local(name)?;
                Ok(None)
            }
            EntityInfo::SelfParam { mutable: _ } => {
                todo!()
            }
            EntityInfo::Field(_field_expr) => {
                todo!()
            }
            EntityInfo::Resolving => {
                panic!("Attempting to generate an entity still being resolved")
            }
            _ => {
                unreachable!()
            }
        }
    }

    //fn handle_top_level_stmt(&mut self, stmt: &HirStmt) -> Result<(), BuildError> {
    //}

    fn handle_stmt(&mut self, stmt: &HirStmt) -> Result<(), BuildError> {
        match stmt.inner() {
            HirStmtKind::Expr(expr) => self.handle_expr(expr.as_ref())?,
            HirStmtKind::Item(entity) => match self.handle_entity(&entity.deref().borrow())? {
                Some(_value) => {
                    let type_name = entity.deref().borrow().type_name();
                    return Err(BuildError::FeatureNotSupported(format!(
                        "local {} not supported",
                        type_name
                    )));
                }
                None => {}
            },
            HirStmtKind::Assignment(assignment) => self.handle_assignment(assignment)?,
            HirStmtKind::Echo(expr) => {
                self.handle_expr(expr.as_ref())?;
                self.emit_op(OpCode::Echo);
            }
        }

        Ok(())
    }

    fn handle_expr(&mut self, expr: &HirExpr) -> Result<(), BuildError> {
        self.handle_expr_inner(expr, false)
    }

    fn handle_expr_inner(&mut self, expr: &HirExpr, is_scope: bool) -> Result<(), BuildError> {
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
                let ox_string = self.vm.new_gc_string_from_str(val);
                let value = Value::String(ox_string);
                let context = self.current_context_mut();
                context.load_constant(OpCode::LoadStr, value);
            }
            HirExprKind::Char(ch) => {
                let value = Value::Char(*ch);
                let context = self.current_context_mut();
                context.load_constant(OpCode::LoadChar, value);
            }
            HirExprKind::Bool(val) => {
                let op = if *val {
                    OpCode::LoadTrue
                } else {
                    OpCode::LoadFalse
                };
                self.emit_op(op);
            }
            HirExprKind::Name(val) => self.handle_name(&val.deref().borrow())?,
            HirExprKind::Binary(bin_expr) => {
                save_state!(self.result_used, true, {
                    self.handle_expr(bin_expr.left.as_ref())?;
                    self.handle_expr(bin_expr.right.as_ref())?;
                });

                let op = if bin_expr.op.is_cmp() {
                    type_helpers::binary_op_for_type(bin_expr.op, bin_expr.left.ty())
                } else {
                    type_helpers::binary_op_for_type(bin_expr.op, ty.clone())
                };

                self.emit_op(op);
            }
            HirExprKind::Unary(_un_expr) => {}
            HirExprKind::Field(field_expr) => self.handle_field_expr(field_expr)?,
            HirExprKind::Index(_) => {}
            HirExprKind::FieldAccess(_) => {}
            HirExprKind::Call(call_expr) => {
                save_state!(self.result_used, true, {
                    self.handle_expr(call_expr.operand.as_ref())?;
                    for actual in call_expr.actuals.iter() {
                        self.handle_expr(actual.as_ref())?;
                    }
                });

                self.emit_op_u8(OpCode::Call, call_expr.actuals.len() as u8);
            }
            HirExprKind::Method(_) => {}
            HirExprKind::AssociatedFunction(_) => {}
            HirExprKind::Block(block_expr) => {
                if self.result_used {
                    self.handle_returning_block(block_expr, 2, is_scope)?;
                } else {
                    self.handle_block(block_expr, is_scope)?;
                }
            }
            HirExprKind::Tuple(tuple_expr) => {
                save_state!(self.result_used, true, {
                    for element in tuple_expr.elements.iter() {
                        self.handle_expr(element.as_ref())?;
                    }
                });

                self.emit_op_u16(OpCode::NewTuple, tuple_expr.elements.len() as u16);
            }
            HirExprKind::TupleIndex(tuple_index) => {
                self.handle_expr(tuple_index.tuple.as_ref())?;
                let index = tuple_index.field as u16;
                self.emit_op_u16(OpCode::TupleAttr, index);
            }
            HirExprKind::Loop(_loop_expr) => {}
            HirExprKind::While(while_expr) => self.handle_while(while_expr)?,
            HirExprKind::For(_for_expr) => {}
            HirExprKind::If(if_expr) => self.handle_if(if_expr)?,
            HirExprKind::StructExpr(struct_expr) => self.handle_struct_expr(struct_expr)?,
            HirExprKind::SelfLit => {}
            HirExprKind::Break => {}
            HirExprKind::Continue => {}
            HirExprKind::Return(return_expr) => {
                save_state!(
                    self.result_used,
                    true,
                    self.handle_expr(return_expr.as_ref())?
                );
                self.emit_op(OpCode::Return);
            }
        }

        Ok(())
    }

    fn handle_name(&mut self, name: &Entity) -> Result<(), BuildError> {
        let global = match name.kind() {
            EntityInfo::Variable(variable_info) => variable_info.global,
            EntityInfo::Param(_) => false,
            _ => true,
        };

        if global {
            let context = self.current_context_mut();
            let global_idx = context.load_global_in_function(name.name());
            self.emit_op_u8(OpCode::LoadGlobal, global_idx);
        } else {
            let context = self.current_context_mut();

            let stack_idx = context
                .current_function()
                .and_then(|funct| funct.look_up_local(name.name()))
                .expect("failed to find local info")
                .stack_idx as u8;

            context
                .current_section_mut()
                .write_arg(OpCode::LoadLocal, stack_idx);
        }

        Ok(())
    }

    fn handle_block(&mut self, block_expr: &BlockExpr, is_scope: bool) -> Result<(), BuildError> {
        self.push_scope();
        for stmt in block_expr.stmts.iter() {
            self.handle_stmt(stmt.as_ref())?;
        }

        // only output the scope cleanup if
        if !is_scope {
            self.cleanup_top_scope();
        }

        // pop locals
        self.pop_scope();

        Ok(())
    }

    fn handle_returning_block(
        &mut self,
        _block_expr: &BlockExpr,
        _dst: usize,
        _is_scope: bool,
    ) -> Result<(), BuildError> {
        todo!()
    }

    fn handle_lvalue(&mut self, lvalue: &HirExpr) -> Result<(), BuildError> {
        let inner = lvalue.inner();
        match inner.kind() {
            HirExprKind::Name(entity) => self.handle_name(&entity.deref().borrow())?,
            HirExprKind::Field(field_expr) => {
                self.handle_expr(field_expr.operand.as_ref())?;
            }
            HirExprKind::Index(index) => {
                self.handle_expr(index.operand.as_ref())?;
                self.handle_expr(index.index.as_ref())?;
            }
            //HirExprKind::TupleIndex(tuple_index) => {}
            _ => {
                panic!(
                    "Compiler Error: unexpected expression '{}' when handling expected lvalues",
                    inner.name()
                );
            }
        }
        Ok(())
    }

    fn handle_assignment(&mut self, assignment: &Assignment) -> Result<(), BuildError> {
        save_state!(
            self.result_used,
            true,
            self.handle_expr(assignment.rhs.as_ref())?
        );
        self.handle_lvalue(assignment.lvalue.as_ref())?;
        self.handle_expr(assignment.rhs.as_ref())?;

        match assignment.lvalue.inner().kind() {
            HirExprKind::Name(entity) => match entity.deref().borrow().kind() {
                EntityInfo::Variable(variable_info) => {
                    self.handle_variable(entity.deref().borrow().name(), variable_info, true)?;
                }
                _ => {}
            },
            HirExprKind::Field(field_expr) => match field_expr.field.deref().borrow().kind() {
                EntityInfo::Field(local_info) => {
                    self.emit_op_u8(OpCode::SetAttr, local_info.index as u8);
                }
                _ => {}
            },
            HirExprKind::Index(_index) => {}
            _ => {}
        }
        Ok(())
    }

    fn handle_if(&mut self, if_expr: &IfExpr) -> Result<(), BuildError> {
        let mut offsets = vec![];
        for branch in if_expr.branches.as_slice() {
            match branch {
                IfExprBranch::Conditional { cond, body, .. } => {
                    self.handle_expr(cond.as_ref())?;
                    let conditional_offset = self.emit_jmp(OpCode::JmpFalse);
                    self.handle_expr(body.as_ref())?;
                    offsets.push(self.emit_jmp(OpCode::Jmp));
                    self.emit_patch(conditional_offset);
                }
                IfExprBranch::Unconditional { body } => {
                    self.handle_expr(body.as_ref())?;
                }
            }
        }
        let section = self.current_section_mut();
        offsets.iter().for_each(|offset| section.patch_jmp(*offset));
        Ok(())
    }

    fn handle_while(&mut self, while_expr: &WhileExpr) -> Result<(), BuildError> {
        let ip = self.current_section().len();
        self.handle_expr(while_expr.cond.as_ref())?;
        let exit_jmp = self.emit_jmp(OpCode::JmpFalse);
        self.handle_expr(while_expr.body.as_ref())?;
        let section = self.current_section_mut();
        section.write_loop(ip);
        section.patch_jmp(exit_jmp);
        Ok(())
    }

    fn handle_struct_expr(&mut self, struct_expr: &StructExpr) -> Result<(), BuildError> {
        let struct_type = struct_expr.struct_type.clone();
        // println!("struct_type: {}", struct_type);
        debug_assert!(struct_type.is_struct());

        if let TypeKind::Struct { entity } = struct_type.kind() {
            let entity_borrow = entity.deref().borrow();
            let name = entity_borrow.name();
            let context = self.current_context_mut();
            let global_idx = context.load_global_in_function(name);
            self.emit_op_u8(OpCode::LoadGlobal, global_idx);
        } else {
            unreachable!()
        };

        for (_, field) in struct_expr.fields.iter() {
            self.handle_expr(field.as_ref())?;
        }

        self.emit_op_u16(OpCode::NewInstance, struct_expr.fields.len() as u16);
        Ok(())
    }

    fn handle_field_expr(&mut self, field_expr: &FieldExpr) -> Result<(), BuildError> {
        save_state!(
            self.result_used,
            true,
            self.handle_expr(field_expr.operand.as_ref())?
        );
        let field_borrow = field_expr.field.deref().borrow();
        match field_borrow.kind() {
            EntityInfo::Field(local_info) => {
                self.emit_op_u16(OpCode::InstanceAttr, local_info.index as u16);
            }
            _ => panic!(
                "field access of invalid entity type: {}",
                field_borrow.type_name()
            ),
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
        context.load_constant(op, value);
        Ok(())
    }

    fn load_float(&mut self, ty: Rc<Type>, val: OrderedFloat<f64>) -> Result<(), BuildError> {
        let (op, value) = match ty.kind() {
            TypeKind::F32 => (OpCode::LoadF32, Value::F32(val.into_inner() as f32)),
            TypeKind::F64 => (OpCode::LoadF64, Value::F64(val.into_inner())),
            _ => panic!("Type Missmatch"),
        };

        let context = self.current_context_mut();
        context.load_constant(op, value);
        Ok(())
    }

    fn handle_variable(
        &mut self,
        name: &str,
        variable_info: &VariableInfo,
        set_op: bool,
    ) -> Result<(), BuildError> {
        println!("handle_variable: {}", name);
        let context = self.current_context_mut();
        if variable_info.global {
            let op = if set_op {
                OpCode::SetGlobal
            } else {
                OpCode::LoadGlobal
            };

            let global_idx = context.load_global_in_function(name);
            let section = context.current_section_mut();
            section.write_arg(op, global_idx);
        } else {
            let op = if set_op {
                OpCode::SetLocal
            } else {
                OpCode::LoadLocal
            };
            let stack_idx = {
                let function = self
                    .current_context_mut()
                    .current_function_mut()
                    .expect("failed to get current function");
                function
                    .look_up_local(name)
                    .expect("failed to find local")
                    .stack_idx as u8
            };

            self.current_section_mut().write_arg(op, stack_idx);
        }

        Ok(())
    }

    fn literal_expr_to_value(&mut self, expr: &HirExpr) -> Result<Value, BuildError> {
        let inner = expr.inner();

        match inner.kind() {
            HirExprKind::Integer(val) => match expr.ty().kind() {
                TypeKind::U8 => Ok(Value::from(*val as u8)),
                TypeKind::U16 => Ok(Value::from(*val as u16)),
                TypeKind::U32 => Ok(Value::from(*val as u32)),
                TypeKind::U64 => Ok(Value::from(*val as u64)),
                TypeKind::I8 => Ok(Value::from(*val as i8)),
                TypeKind::I16 => Ok(Value::from(*val as i16)),
                TypeKind::Integer | TypeKind::I32 => Ok(Value::from(*val as i32)),
                TypeKind::I64 => Ok(Value::from(*val)),
                _ => Err(BuildError::InvalidLiteralCast(expr.ty())),
            },
            HirExprKind::Float(val) => match expr.ty().kind() {
                TypeKind::Float | TypeKind::F32 => Ok(Value::from(val.into_inner() as f32)),
                TypeKind::F64 => Ok(Value::from(val.into_inner())),
                _ => Err(BuildError::InvalidLiteralCast(expr.ty())),
            },
            HirExprKind::String(val) => {
                let ox_string = self.vm.new_gc_string_from_str(val.as_str());
                Ok(Value::from(ox_string))
            }
            HirExprKind::Char(val) => Ok(Value::from(*val)),
            HirExprKind::Bool(val) => Ok(Value::from(*val)),
            _ => Err(BuildError::InvalidLiteralCast(expr.ty())),
        }
    }

    fn cleanup_top_scope(&mut self) {
        let current_level = self.scope_index;
        let locals = self
            .current_context()
            .current_function()
            .expect("invalid current function")
            .locals
            .as_slice();
        let mut lost_locals = 0;
        for local in locals.iter().rev() {
            if local.scope_level < current_level {
                break;
            }
            lost_locals += 1;
        }
        std::mem::drop(locals);

        (0..lost_locals).for_each(|_| self.emit_op(OpCode::Pop));

        let function = self
            .current_context_mut()
            .current_function_mut()
            .expect("invalid current function");
        let len = function.locals.len();
        let start = len - lost_locals;
        function.locals.drain(start..len);
        assert_eq!(function.locals.len(), start);
    }
}
