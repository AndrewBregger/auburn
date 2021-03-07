mod file_context;
mod type_helpers;

use crate::{analysis::{self, Entity, EntityInfo, FunctionInfo, StructureInfo, VariableInfo}, ir::{self, ast::BinaryOp, hir::{Assignment, BlockExpr, FieldExpr, HirExpr, HirExprPtr, HirFile, HirItem, HirItemPtr, HirStmt, HirStmtKind, HirStmtPtr, IfExpr, IfExprBranch, MirNode, StructExpr, WhileExpr}}, system::{FileId, FileMap}, types::{Type, TypeKind}};
use file_context::FileContext;
use hir::HirExprKind;
use ir::hir;
use oxide::{OxFunction, OxModule, OxStruct, Section, Value, Vm, gc::{Allocator, Gc}, vm::OpCode};

use ordered_float::OrderedFloat;
use std::{cell::RefCell, collections::HashMap, convert::TryInto, ops::Deref, rc::Rc};

use self::file_context::GlobalInfo;

macro_rules! save_state {
    ($field:expr, $val:expr, $op:expr) => {
        let saved = $field;
        $field = $val;
        $op;
        $field = saved;
    }
}


#[derive(thiserror::Error, Debug)]
pub enum BuildError {
    #[error("{0}")]
    FeatureNotSupported(String),
}

pub struct CodeGen<'vm, 'ctx> {
    file_map: &'ctx FileMap,
    vm: &'vm mut Vm,
    file_stack: Vec<FileId>,
    file_context: HashMap<FileId, FileContext<'ctx>>,
    scope_index: usize,
    handling_params: bool,
    result_used: bool,
    is_function_scope: bool,
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
            scope_index: 0,
            handling_params: false,
            result_used: false,
            is_function_scope: false,
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

    fn get_file(&self, id: &FileId) -> &str {
        self.file_map
            .find(id)
            .map(|f| f.name())
            .expect("failed to find file")
    }

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
        let name = self.vm.allocate_string(stem);

        let mut context = self
            .file_context
            .remove(&hir_file.id())
            .expect("unable to remove file context");

        let mut values = self.vm.allocate_vec(context.values.len());
        values.extend(context.values.drain(..).into_iter());
        let mut module = self.vm.allocate_module(name, values);


        if let Some(entry) = hir_file.get_entry() {
            if let Some(global_info) = context.globals.get(entry.borrow().name()) {
                let module_mut = module.as_ref_mut();
                module_mut.set_entry(global_info.object_idx);
            }
        }

        for funct in context.function_stack.iter() {
            println!("Globals: {:#?}", funct.global_map);
            println!("Locals: {:#?}", funct.locals);
        }

        Ok(module)
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
            let entity_borrow = item.borrow();
            let name = entity_borrow.name();
            context
                .globals
                .insert(name.to_owned(), GlobalInfo::new(name.to_owned(), idx));
        }

        std::mem::drop(context);

        for entity in items.into_iter() {
            match self.handle_entity(&entity.borrow())? {
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

        let name_string = self.vm.allocate_string(name);
        let function = self.vm.allocate_function(
            name_string,
            mir_function.params.len() as u8,
            Section::new(&self.vm),
        );
        self.current_context_mut().push_function(function);
        
        self.handle_function_params(mir_function)?;

        self.handle_expr_inner(mir_function.body.as_ref(), true)?;
        self.emit_op(OpCode::Return);

        self.pop_scope();

        let context = self.current_context_mut();
        let function = if let Some(function_info) = context.current_function() {
            function_info.function.clone()
        } else { panic!("failed to get the current function") };
        Ok(function)
    }

    fn handle_function_params(&mut self,  mir_function: &FunctionInfo) -> Result<(), BuildError> {
        self.handling_params = true;
        
        for (_, param_entity) in mir_function.params.elements() {
            self.handle_entity(&param_entity.borrow())?;
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
                let inner = default.inner();
                let value = match inner.kind() {
                    HirExprKind::Integer(val) => {
                        match default.ty().kind() {
                            TypeKind::U8 => Value::from(*val as u8),
                            TypeKind::U16 => Value::from(*val as u16),
                            TypeKind::U32 => Value::from(*val as u32),
                            TypeKind::U64 => Value::from(*val as u64),
                            TypeKind::I8 => Value::from(*val as i8),
                            TypeKind::I16 => Value::from(*val as i16),
                            TypeKind::Integer | TypeKind::I32 => Value::from(*val as i32),
                            TypeKind::I64 => Value::from(*val),
                            _ => unreachable!(),
                        }
                    }
                    HirExprKind::Float(val) => match default.ty().kind() {
                        TypeKind::Float | TypeKind::F32 => Value::from(val.into_inner() as f32),
                        TypeKind::F64 => Value::from(val.into_inner()),
                        _ => unreachable!(),
                    }
                    HirExprKind::String(val) => {
                        let ox_string = self.vm.allocate_string_ptr(val.as_str());
                        Value::from(ox_string)
                    }
                    HirExprKind::Char(val) => Value::from(*val),
                    HirExprKind::Bool(val) => Value::from(*val),
                    _ => unreachable!(),
                };

                let context = self.current_context_mut();
                context.push_object(value);
            }
            else {
                panic!("non literals for globals is not supported by the vm")
            }
        }
        else {
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

    fn build_local(&mut self, name: &str /* _variable_info: &VariableInfo */) -> Result<(), BuildError> {
        let scope_index = self.scope_index;
        let context = self.current_context_mut();
        context.push_local(name, scope_index);
        if !self.handling_params {
            self.emit_op(OpCode::PushLocal);
        }
        Ok(())
    }

    
    fn build_struct(&mut self, name: &str, struct_info: &StructureInfo) -> Result<Gc<OxStruct>, BuildError> {
        let mut methods = self.vm.allocate_vec::<Value>(struct_info.methods.len());

        for (_, element) in struct_info.methods.elements() {
            debug_assert!(element.borrow().is_function());
            match self.handle_entity(&element.borrow())? {
                Some(value) => {
                    methods.push(value);
                }
                None => {}
            }
        }
        
        let name = self.vm.allocate_string(name);
        let structure = self.vm.allocate_struct(name, methods);
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
            EntityInfo::SelfParam { mutable } => {
                todo!()
            }
            EntityInfo::Field(field_expr) => {
                todo!()
            }
            EntityInfo::Resolving => {
                panic!("Attempting to generate an entity still being resolved")
            }
            _ => { unreachable!() }
        }
    }

    //fn handle_top_level_stmt(&mut self, stmt: &HirStmt) -> Result<(), BuildError> {
    //}

    fn handle_stmt(&mut self, stmt: &HirStmt) -> Result<(), BuildError> {
        match stmt.inner() {
            HirStmtKind::Expr(expr) => self.handle_expr(expr.as_ref())?,
            HirStmtKind::Item(entity) => {
                match self.handle_entity(&entity.borrow())? {
                    Some(_value) => {
                        return Err(BuildError::FeatureNotSupported(format!("local {} not supported", entity.borrow().type_name())));
                    }
                    None => {}
                }
            }
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
                let ox_string = self.vm.allocate_string_ptr(val);
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
            HirExprKind::Name(val) => self.handle_name(&val.borrow())?,
            HirExprKind::Binary(bin_expr) => {
                save_state!(self.result_used, true, 
                    {
                        self.handle_expr(bin_expr.left.as_ref())?;
                        self.handle_expr(bin_expr.right.as_ref())?;
                    }
                );

                let op = if bin_expr.op.is_cmp() {
                    type_helpers::binary_op_for_type(bin_expr.op, bin_expr.left.ty())
                } else {
                    type_helpers::binary_op_for_type(bin_expr.op, ty.clone())
                };

                self.emit_op(op);
            }
            HirExprKind::Unary(un_expr) => {}
            HirExprKind::Field(field_expr) =>  self.handle_field_expr(field_expr)?,
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
                }
                else {
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
            HirExprKind::Loop(loop_expr) => {}
            HirExprKind::While(while_expr) => self.handle_while(while_expr)?,
            HirExprKind::For(for_expr) => {}
            HirExprKind::If(if_expr) => self.handle_if(if_expr)?,
            HirExprKind::StructExpr(struct_expr) => self.handle_struct_expr(struct_expr)?,
            HirExprKind::SelfLit => {}
            HirExprKind::Break => {}
            HirExprKind::Continue => {}
            HirExprKind::Return(return_expr) => {
                save_state!(self.result_used, true,
                self.handle_expr(return_expr.as_ref())?);
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
        
        println!("Variable Global: {} {}", name.name(), global);
        if global {
            let context = self.current_context_mut();
            let global_idx = context.load_global_in_function(name.name());
            self.emit_op_u8(OpCode::LoadGlobal, global_idx);
        } else {
            let context = self.current_context_mut();

            let stack_idx = context
                .current_function()
                .and_then(|funct| funct.look_up_local(name.name()))
                .expect("failed to find local info").stack_idx as u8;

            context.current_section_mut().write_arg(OpCode::LoadLocal, stack_idx);
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

    fn handle_returning_block(&mut self, block_expr: &BlockExpr, dst: usize, is_scope: bool) -> Result<(), BuildError> {
        todo!()
    }

    fn handle_assignment(&mut self, assignment: &Assignment) -> Result<(), BuildError> {
        save_state!(self.result_used, true,  self.handle_expr(assignment.rhs.as_ref())?);
        let lvalue_borrow = assignment.lvalue.borrow();
        if let EntityInfo::Variable(variable_info) = lvalue_borrow.kind() {
            self.handle_variable(lvalue_borrow.name(), variable_info, true)?;
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
        println!("struct_type: {}", struct_type);
		debug_assert!(struct_type.is_struct()); 

        if let TypeKind::Struct { entity } = struct_type.kind() {
            let entity_borrow = entity.borrow();
            let name = entity_borrow.name();
            let context = self.current_context_mut();
            let global_idx = context.load_global_in_function(name);
            self.emit_op_u8(OpCode::LoadGlobal, global_idx);
        }
        else { unreachable!() };
        

        for (_, field) in struct_expr.fields.iter() {
            self.handle_expr(field.as_ref())?;
        }

        self.emit_op_u16(OpCode::NewInstance, struct_expr.fields.len() as u16);
        Ok(())
	}

    fn handle_field_expr(&mut self, field_expr: &FieldExpr) -> Result<(), BuildError> {
        save_state!(self.result_used, true, self.handle_expr(field_expr.operand.as_ref())?);
        let field_borrow = field_expr.field.borrow();
        match field_borrow.kind() {
            EntityInfo::Field(local_info) => {
                self.emit_op_u16(OpCode::InstanceAttr, local_info.index as u16);
            }
            _ => panic!("field access of invalid entity type: {}", field_borrow.type_name()),
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
                let function = self.current_context_mut().current_function_mut().expect("failed to get current function");
                function.look_up_local(name).expect("failed to find local").stack_idx as u8
            };

            self.current_section_mut().write_arg(op, stack_idx);
        }

        Ok(())
    }

    fn cleanup_top_scope(&mut self) {
        let current_level = self.scope_index;
        let locals = self.current_context().current_function().expect("invalid current function").locals.as_slice();
        let mut lost_locals = 0;
        for local in locals.iter().rev() {
            if local.scope_level < current_level {
                break;
            }
            lost_locals += 1;
        }
        std::mem::drop(locals);

        (0..lost_locals).for_each(|_| self.emit_op(OpCode::Pop));

        let function = self.current_context_mut().current_function_mut().expect("invalid current function");
        let len = function.locals.len();
        let start = len - lost_locals;
        function.locals.drain(start..len);
        assert_eq!(function.locals.len(), start);
    }
}
