use std::cell::RefCell;
use std::rc::Rc;

use crate::analysis::scope::{Scope, ScopeKind, ScopeRef};
use crate::analysis::{Entity, EntityInfo, EntityRef};
use crate::error::Error;
use crate::mir::{
    BinaryExpr, BlockExpr, MirExpr, MirExprKind, MirExprPtr, MirFile, MirNode, MirSpec,
    MirSpecKind, MirStmt, MirStmtKind, StructExpr, UnaryExpr,
};
use crate::syntax::ast::{
    BinaryOp, Expr, ExprKind, FunctionBody, Identifier, Item, ItemKind, Node, NodeType, Spec,
    SpecKind, Stmt, StmtKind, StructExprField, UnaryOp, Visibility,
};
use crate::syntax::{ParsedFile, Position};
use crate::types::{Type, TypeKind, TypeMap};
use crate::utils::{new_ptr, Ptr};
use crate::{
    analysis::entity::{FunctionInfo, LocalInfo, StructureInfo, VariableInfo},
    mir::Structure,
};
use std::borrow::Borrow;
use std::ops::Deref;

type State = u64;

const DEFAULT: State = 0;
const BLOCK: State = 1 << 0;
const EXPR_RESULT_USED: State = 1 << 1;
const STRUCT: State = 1 << 2;
const FUNCTION: State = 1 << 3;
const FUNCTION_PARAM: State = 1 << 4;
const FUNCTION_BODY: State = 1 << 5;

macro_rules! with_state {
    ($typer:expr, $state:expr, $body:tt) => {{
        let old_state = $typer.state;
        $typer.state |= $state;
        let res = $body;
        $typer.state = old_state;
        res
    }};
}

pub(crate) struct Typer<'a> {
    type_map: &'a mut TypeMap,
    item_stack: &'a mut Vec<Box<Item>>,
    scope_stack: &'a mut Vec<Scope>,
    entities: Vec<EntityRef>,
    state: State,
}

impl<'a> Typer<'a> {
    pub fn new(
        type_map: &'a mut TypeMap,
        item_stack: &'a mut Vec<Box<Item>>,
        scope_stack: &'a mut Vec<Scope>,
    ) -> Self {
        Self {
            type_map,
            item_stack,
            scope_stack,
            entities: vec![],
            state: DEFAULT,
        }
    }

    fn check_state(&self, state: State) -> bool {
        (self.state & state) == state
    }

    fn push_scope(&mut self, kind: ScopeKind) {
        let new_scope = Scope::new(kind, None);
        self.scope_stack.push(new_scope);
    }

    fn insert_entity(&mut self, name: &str, entity: EntityRef) {
        self.current_scope_mut().add_element(name, entity);
    }

    fn pop_scope(&mut self) -> ScopeRef {
        let mut last_scope = self.scope_stack.pop().unwrap();
        let last_scope = Rc::new(last_scope);
        self.current_scope_mut().add_child(last_scope.clone());
        last_scope
    }

    fn insert_type(&mut self, kind: TypeKind) -> Rc<Type> {
        self.type_map.insert_type(kind)
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scope_stack.last_mut().expect("Scope stack is emtpy")
    }

    fn current_scope(&self) -> &Scope {
        self.scope_stack.last().expect("Scope stack is emtpy")
    }

    fn deep_lookup(&self, name: &str) -> Option<EntityRef> {
        for item in self.scope_stack.iter().rev() {
            if let Some(entity) = item.shallow_lookup(name) {
                return Some(entity);
            }
        }
        None
    }

    pub fn shallow_lookup(&self, name: &str) -> Option<EntityRef> {
        self.current_scope().shallow_lookup(name)
    }

    pub fn resolve_file(mut self, parsed_file: ParsedFile) -> Result<MirFile, Error> {
        self.push_scope(ScopeKind::File(parsed_file.file_id));
        println!("Stmts: {}", parsed_file.stmts.len());
        let mut global_expression = vec![];
        let mut items = vec![];

        for stmt in &parsed_file.stmts {
            match stmt.kind() {
                StmtKind::Item(item) => match item.kind() {
                    ItemKind::Variable { name, vis, .. }
                    | ItemKind::Struct { name, vis, .. }
                    | ItemKind::Function { name, vis, .. } => {
                        let entity = Rc::new(RefCell::new(Entity::unresolved(
                            *vis,
                            name.kind().value.clone(),
                            item.clone(),
                            self.type_map.get_invalid(),
                        )));
                        self.insert_entity(name.kind().value.as_str(), entity);
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        for stmt in &parsed_file.stmts {
            match stmt.kind() {
                StmtKind::Expr(expr) => {
                    let expr = self.resolve_expr(expr.as_ref(), None)?;
                    global_expression.push(expr);
                }
                StmtKind::Item(item) => {
                    let item = self.resolve_top_level_item(item.as_ref())?;
                    items.push(item);
                }
                _ => unreachable!(),
            }
        }

        let entities = self
            .current_scope()
            .elements()
            .values()
            .map(|entity| entity.clone())
            .collect();

        self.pop_scope();

        Ok(MirFile::new(
            parsed_file.file_id,
            global_expression,
            entities,
        ))
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<Rc<MirStmt>, Error> {
        println!("Resolving Stmt {}", stmt.kind().name());

        match stmt.kind() {
            StmtKind::Expr(expr) => {
                let old_state = self.state;
                self.state &= !EXPR_RESULT_USED;
                let expr = self.resolve_expr(expr.as_ref(), None)?;
                self.state = old_state;
                let position = expr.position();
                let ty = expr.ty();
                Ok(Rc::new(MirStmt::new(MirStmtKind::Expr(expr), position, ty)))
            }
            StmtKind::Item(item) => {
                self.item_stack.push(item.clone());
                let entity = self.resolve_item(item.as_ref())?;
                self.item_stack.pop();

                let position = item.position();
                let ty = entity.deref().borrow().ty();
                Ok(Rc::new(MirStmt::new(
                    MirStmtKind::Item(entity),
                    position,
                    ty,
                )))
            }
            StmtKind::Empty => unreachable!(),
        }
    }

    fn resolve_expr(
        &mut self,
        expr: &Expr,
        expected_type: Option<Rc<Type>>,
    ) -> Result<Rc<MirExpr>, Error> {
        println!("Resolving Expr: {}", expr.kind().name());
        let expr = match expr.kind() {
            ExprKind::Integer(val) => {
                let ty = self.type_map.get_u32();
                Rc::new(MirExpr::new(
                    MirExprKind::Integer(*val),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Float(val) => {
                let ty = self.type_map.get_f32();
                Rc::new(MirExpr::new(MirExprKind::Float(*val), expr.position(), ty))
            }
            ExprKind::String(val) => {
                let ty = self.type_map.get_string();
                Rc::new(MirExpr::new(
                    MirExprKind::String(val.clone()),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Char(val) => {
                let ty = self.type_map.get_char();
                Rc::new(MirExpr::new(MirExprKind::Char(*val), expr.position(), ty))
            }
            ExprKind::Name(ident) => {
                let name = self.resolve_ident(ident)?;
                let ty = name.deref().borrow().ty();
                Rc::new(MirExpr::new(MirExprKind::Name(name), expr.position(), ty))
            }
            ExprKind::Binary(op, left, right) => self.resolve_binary(
                *op,
                left.as_ref(),
                right.as_ref(),
                expected_type.clone(),
                expr.position(),
            )?,
            ExprKind::Unary(op, expr) => {
                self.resolve_unary(*op, expr.as_ref(), expected_type.clone(), expr.position())?
            }
            ExprKind::Block(stmts) => {
                self.push_scope(ScopeKind::Block);
                let stmts = with_state!(self, BLOCK, {
                    let mut mir_stmts = vec![];
                    for stmt in stmts {
                        let mir_stmt = self.resolve_stmt(stmt.as_ref())?;
                        mir_stmts.push(mir_stmt);
                    }

                    Ok(mir_stmts)
                })?;

                self.pop_scope();

                let return_type = stmts
                    .last()
                    .map_or(self.type_map.get_unit(), |stmt| stmt.ty());

                let block_expr = BlockExpr {
                    stmts,
                    return_used: self.check_state(EXPR_RESULT_USED),
                };

                Rc::new(MirExpr::new(
                    MirExprKind::Block(block_expr),
                    expr.position(),
                    return_type,
                ))
            }
            ExprKind::StructExpr { name, fields } => {
                let ty = self.resolve_type_expr(name.as_ref())?;
                let entity_borrow = ty.deref().borrow();
                let struct_type = entity_borrow.ty();
                println!("StructExpr: {}", struct_type);
                match entity_borrow.kind() {
                    EntityInfo::Structure(structure) => {
                        self.resolve_struct_expr(struct_type, fields, structure, expr.position())?
                    }
                    _ => {
                        let err = Error::expected_struct_type(struct_type.as_ref())
                            .with_position(name.position());
                        return Err(err);
                    }
                }
            }
            /*
            ExprKind::Field(_, _) => {}
            ExprKind::Call { .. } => {}
            ExprKind::Method { .. } => {}
            ExprKind::Tuple(_) => {}
            ExprKind::Loop(_) => {}
            ExprKind::While(_, _) => {}
            ExprKind::For { .. } => {}
            ExprKind::If { .. } => {}
            ExprKind::SelfLit => {}
            ExprKind::SelfType => {}
            */
            _ => unimplemented!(),
        };

        if let Some(expected_type) = &expected_type {
            println!(
                "Has Expected Type: {}, Found Type: {}",
                expected_type,
                expr.ty()
            );
            if *expected_type != expr.ty() {
                return Err(
                    Error::incompatible_types(expected_type.as_ref(), expr.ty().as_ref())
                        .with_position(expr.position()),
                );
            }
        }

        Ok(expr)
    }

    fn resolve_struct_expr(
        &mut self,
        struct_type: Rc<Type>,
        fields: &[StructExprField],
        structure: &StructureInfo,
        position: Position,
    ) -> Result<MirExprPtr, Error> {
        let mut mir_fields = vec![];
        for field in fields {
            match field {
                StructExprField::Bind(name, expr) => {
                    let field_name = name.kind().value.as_str();
                    if let Some(field) = structure.field.elements().get(field_name) {
                        let mir_field =
                            self.resolve_expr(expr.as_ref(), Some(field.deref().borrow().ty()))?;
                        if let EntityInfo::Param(local_info) = field.deref().borrow().kind() {
                            mir_fields.push((local_info.index, mir_field));
                        } else {
                            panic!("Compiler Error: Invalid entity for struct field");
                        }
                    } else {
                        let err = Error::undeclared_field_in_struct_binding(
                            field_name,
                            struct_type.as_ref(),
                        );
                        return Err(err.with_position(name.position()));
                    }
                }
                StructExprField::Field(expr) => match expr.kind() {
                    ExprKind::Name(name) => {
                        let field_name = name.kind().value.as_str();
                        if let Some(field) = structure.field.elements().get(field_name) {
                            let mir_field = self
                                .resolve_expr(expr.as_ref(), Some(field.deref().borrow().ty()))?;
                            if let EntityInfo::Field(local_info) = field.deref().borrow().kind() {
                                mir_fields.push((local_info.index, mir_field));
                            } else {
                                panic!("Compiler Error: Invalid entity for struct field");
                            }
                        } else {
                            let err = Error::undeclared_field_in_struct_binding(
                                field_name,
                                struct_type.as_ref(),
                            );
                            return Err(err.with_position(name.position()));
                        }
                    }
                    _ => {}
                },
            }
        }
        let struct_expr = StructExpr {
            struct_type: struct_type.clone(),
            fields: mir_fields,
        };
        let mir_expr = MirExprKind::StructExpr(struct_expr);
        Ok(Rc::new(MirExpr::new(mir_expr, position, struct_type)))
    }

    fn resolve_top_level_item(&mut self, item: &Item) -> Result<EntityRef, Error> {
        if let Some(name) = item.get_name() {
            if let Some(entity) = self.shallow_lookup(name.kind().value.as_str()) {
                entity.deref().borrow_mut().to_resolving();
                self.resolve_item_impl(item, entity, true)
            } else {
                let err = Error::other(
                    "Compiler Error: attempting to resolve top level item but failed to find entity"
                        .to_string(),
                );
                return Err(err.with_position(name.position()));
            }
        } else {
            panic!("Compiler Error: Invalid top level item found in analysis. This should have been caught by the parser.");
        }
    }

    fn resolve_item(&mut self, item: &Item) -> Result<EntityRef, Error> {
        if let Some(name) = item.get_name() {
            let vis = item.get_visibility();
            self.check_duplicate_item_name(name)?;
            let entity = Ptr::new(RefCell::new(Entity::resolving(
                vis,
                name.kind().value.clone(),
                self.type_map.get_invalid(),
            )));
            self.resolve_item_impl(item, entity, false)
        } else {
            panic!("Compiler Error: attempting to resolving a field, param, or self as an item. These should be resolved locally");
        }
    }

    fn resolve_item_impl(
        &mut self,
        item: &Item,
        entity: EntityRef,
        declared: bool,
    ) -> Result<EntityRef, Error> {
        match item.kind() {
            ItemKind::Variable {
                vis,
                mutable,
                name,
                init,
                spec,
            } => self.resolve_variable(
                entity,
                *vis,
                *mutable,
                name.clone(),
                init.as_ref(),
                spec.as_ref(),
                item.position(),
                declared,
            ),
            ItemKind::Struct { vis, name, fields } => self.resolve_struct(
                entity,
                *vis,
                name,
                fields.as_slice(),
                item.position(),
                declared,
            ),
            ItemKind::Function {
                vis,
                name,
                params,
                ret,
                body,
            } => with_state!(self, FUNCTION, {
                self.resolve_function(
                    entity,
                    *vis,
                    name,
                    params.as_slice(),
                    ret.as_ref(),
                    body,
                    item.position(),
                    declared,
                )
            }),
            ItemKind::Param { .. } | ItemKind::SelfParam { .. } | ItemKind::Field { .. } => todo!(),
        }
    }

    fn check_duplicate_item_name(&self, name: &Identifier) -> Result<(), Error> {
        if let Some(entity) = self.shallow_lookup(name.kind().value.as_str()) {
            let err =
                Error::duplicate_name(name.kind().value.clone()).with_position(name.position());
            Err(err)
        } else {
            Ok(())
        }
    }

    fn resolve_variable(
        &mut self,
        entity: EntityRef,
        vis: Visibility,
        mutable: bool,
        name: Identifier,
        init: Option<&Box<Expr>>,
        spec: Option<&Box<Spec>>,
        position: Position,
        declared: bool,
    ) -> Result<EntityRef, Error> {
        let (spec, init, result_type) = self.resolve_local(spec, init, position)?;

        let variable_info = VariableInfo {
            spec,
            default: init.clone(),
        };

        entity
            .borrow_mut()
            .resolve(result_type, EntityInfo::Variable(variable_info));

        {
            let borrow = entity.deref().borrow();
            println!("Variable Entity Ptr: {:?}", (&borrow) as *const _);
        }

        if !declared {
            self.insert_entity(name.kind().value.as_str(), entity.clone());
        }

        Ok(entity)
    }

    fn resolve_struct(
        &mut self,
        entity: EntityRef,
        vis: Visibility,
        name: &Identifier,
        fields: &[Box<Item>],
        position: Position,
        declared: bool,
    ) -> Result<EntityRef, Error> {
        let (fields, methods) = Self::split_struct_members(fields);
        let fields_scope = with_state!(self, STRUCT, {
            self.push_scope(ScopeKind::Struct(name.kind().value.clone()));

            let mut field_index = 0;
            for field in fields {
                if let ItemKind::Field {
                    vis,
                    names,
                    spec,
                    init,
                } = field.kind()
                {
                    self.resolve_field(
                        *vis,
                        names.as_slice(),
                        spec.as_ref(),
                        init.as_ref(),
                        field_index,
                        field.position(),
                    )?;
                    field_index += names.len();
                }
            }

            self.pop_scope()
        });
        todo!()
    }

    fn resolve_local(
        &mut self,
        spec: Option<&Box<Spec>>,
        init: Option<&Box<Expr>>,
        position: Position,
    ) -> Result<(Option<Rc<MirSpec>>, Option<Rc<MirExpr>>, Rc<Type>), Error> {
        let spec = match spec {
            Some(spec) => Some(self.resolve_spec(spec.as_ref())?),
            None => None,
        };

        let init = match init {
            Some(init) => Some(self.resolve_expr(init.as_ref(), None)?),
            None => None,
        };

        let result_type = match (&spec, &init) {
            (Some(spec), Some(init)) => {
                let spec_type = spec.ty();
                let init_type = init.ty();

                if *spec_type == *init_type {
                    spec_type
                } else {
                    return Err(
                        Error::incompatible_types(spec_type.as_ref(), init_type.as_ref())
                            .with_position(init.position()),
                    );
                }
            }
            (Some(spec), None) => spec.ty(),
            (None, Some(init)) => init.ty(),
            (None, None) => return Err(Error::invalid_local_item().with_position(position)),
        };

        Ok((spec, init, result_type))
    }

    fn resolve_field(
        &mut self,
        vis: Visibility,
        names: &[Identifier],
        spec: Option<&Box<Spec>>,
        init: Option<&Box<Expr>>,
        start_index: usize,
        position: Position,
    ) -> Result<Vec<EntityRef>, Error> {
        let mut fields = vec![];
        let (spec, init, ty) = self.resolve_local(spec, init, position)?;

        names.iter().enumerate().for_each(|(idx, name)| {
            let index = start_index + idx;
            let local_info = LocalInfo {
                index,
                spec: spec.clone(),
                default: init.clone(),
            };
            let info = EntityInfo::Field(local_info);
            let entity = new_ptr(Entity::new(
                vis,
                name.kind().value.clone(),
                ty.clone(),
                info,
            ));
            self.insert_entity(name.kind().value.as_str(), entity.clone());
            fields.push(entity);
        });
        Ok(fields)
    }

    fn split_struct_members(members: &[Box<Item>]) -> (Vec<&Box<Item>>, Vec<&Box<Item>>) {
        let mut fields = vec![];
        let mut methods = vec![];

        for item in members {
            match item.kind() {
                ItemKind::Field { .. } => fields.push(item),
                ItemKind::Function { .. } => methods.push(item),
                _ => unreachable!(),
            }
        }
        (fields, methods)
    }

    fn resolve_function(
        &mut self,
        entity: EntityRef,
        vis: Visibility,
        name: &Identifier,
        params: &[Box<Item>],
        return_spec: &Spec,
        body: &FunctionBody,
        position: Position,
        declared: bool,
    ) -> Result<EntityRef, Error> {
        let mut function_params = Vec::with_capacity(params.len());
        let mir_items = with_state!(self, FUNCTION_PARAM, {
            let mut mir_items = vec![];
            self.push_scope(ScopeKind::Param(name.kind().value.clone()));
            let mut start_index = 0;
            for param in params {
                if let ItemKind::Param { names, spec, init } = param.kind() {
                    let params = self.resolve_param(
                        names,
                        spec.as_ref(),
                        init.as_ref(),
                        start_index,
                        param.position(),
                    )?;
                    start_index += names.len();

                    function_params.extend(params.iter().map(|param| param.deref().borrow().ty()));
                    mir_items.extend(params);
                }
            }
            Ok(mir_items)
        })?;

        let (return_type, mir_spec, mir_expr) =
            with_state!(self, FUNCTION_BODY | EXPR_RESULT_USED, {
                match body {
                    FunctionBody::Block(expr) => {
                        let mir_spec = if return_spec.is_infer() {
                            Rc::new(MirSpec::new(
                                MirSpecKind::Infer,
                                position,
                                self.type_map.get_unit(),
                            ))
                        } else {
                            self.resolve_spec(return_spec)?
                        };

                        let mir_expr = self.resolve_expr(expr.as_ref(), Some(mir_spec.ty()))?;
                        Ok((mir_spec.ty(), mir_spec, mir_expr))
                    }
                    FunctionBody::Expression(expr) => {
                        let mir_spec = if return_spec.is_infer() {
                            None
                        } else {
                            Some(self.resolve_spec(return_spec)?)
                        };

                        let mir_expr = self
                            .resolve_expr(expr.as_ref(), mir_spec.as_ref().map(|spec| spec.ty()))?;

                        let return_spec = match mir_spec {
                            Some(ty) => ty,
                            None => {
                                Rc::new(MirSpec::new(MirSpecKind::Infer, position, mir_expr.ty()))
                            }
                        };

                        Ok((return_spec.ty(), return_spec, mir_expr))
                    }
                }
            })?;

        let params_scope = self.pop_scope();

        let function_kind = TypeKind::Function {
            params: function_params,
            return_type: return_type.clone(),
        };

        let function_type = self.insert_type(function_kind);

        let body_scope = params_scope.children().first().map(Rc::clone);

        let function_info = FunctionInfo {
            receiver: None,
            params: params_scope,
            body_scope,
            body: mir_expr,
        };

        {
            let borrow = entity.deref().borrow();
            println!(
                "Resolved Function: {} Entity {:?}",
                function_type,
                (&borrow) as *const _
            );
        }
        entity
            .borrow_mut()
            .resolve(function_type, EntityInfo::Function(function_info));

        if !declared {
            self.insert_entity(name.kind().value.as_str(), entity.clone());
        }

        Ok(entity)
    }

    fn resolve_param(
        &mut self,
        names: &[Identifier],
        spec: Option<&Box<Spec>>,
        init: Option<&Box<Expr>>,
        start_index: usize,
        position: Position,
    ) -> Result<Vec<EntityRef>, Error> {
        let mut fields = vec![];
        let (spec, init, ty) = self.resolve_local(spec, init, position)?;
        names.iter().enumerate().for_each(|(idx, name)| {
            let index = start_index + idx;
            let local_info = LocalInfo {
                index,
                spec: spec.clone(),
                default: init.clone(),
            };

            let info = EntityInfo::Param(local_info);
            let entity = new_ptr(Entity::new(
                Visibility::Private,
                name.kind().value.clone(),
                ty.clone(),
                info,
            ));

            self.insert_entity(name.kind().value.as_str(), entity.clone());
            fields.push(entity);
        });

        Ok(fields)
    }

    fn resolve_spec(&mut self, spec: &Spec) -> Result<Rc<MirSpec>, Error> {
        match spec.kind() {
            SpecKind::Named(expr) => {
                let entity = self.resolve_type_expr(expr.as_ref())?;
                if entity.deref().borrow().is_type() {
                    let ty = entity.deref().borrow().ty();
                    Ok(Rc::new(MirSpec::new(
                        MirSpecKind::Named,
                        spec.position(),
                        ty,
                    )))
                } else {
                    panic!()
                }
            }
            SpecKind::SelfType => todo!(),
            SpecKind::Tuple(_) | SpecKind::Unit | SpecKind::Infer => todo!(),
        }
    }

    fn resolve_type_expr(&mut self, expr: &Expr) -> Result<EntityRef, Error> {
        match expr.kind() {
            ExprKind::Name(name) => self.resolve_ident(name),
            // ExprKind::Field(operand, name) => {}
            // ExprKind::Call { operand, actual } => {
            //     let operand_entity = self.resolve_type_expr(operand.as_ref())?;
            // }
            // ExprKind::Method {
            //     operand,
            //     name,
            //     actual,
            // } => {}
            // ExprKind::SelfType => {}
            _ => unreachable!("Unreachable at: {}", expr.name()),
        }
    }

    fn resolve_ident(&mut self, ident: &Identifier) -> Result<EntityRef, Error> {
        if let Some(entity) = self.deep_lookup(ident.kind().value.as_str()) {
            println!("Resolving Name: {}", ident.kind().value);
            let entity_borrow = entity.deref().borrow();
            if entity_borrow.is_resolved() {
                println!("\tName Resolves is resolved");
                std::mem::drop(entity_borrow);
                Ok(entity)
            } else if entity_borrow.is_resolving() {
                let err =
                    Error::other("Cyclic references are not supported at the moment".to_string())
                        .with_position(ident.position());
                Err(err)
            } else {
                println!("\tEntity is unresolved, resolving");
                if let EntityInfo::Unresolved(item) = entity_borrow.kind().clone() {
                    std::mem::drop(entity_borrow);
                    let entity = self.resolve_item_impl(item.as_ref(), entity, true)?;
                    Ok(entity)
                } else {
                    unreachable!("Compiler Error")
                }
            }
        } else {
            Err(Error::undeclared_identifier(ident.kind().value.clone())
                .with_position(ident.position()))
        }
    }

    fn resolve_binary(
        &mut self,
        op: BinaryOp,
        lhs: &Expr,
        rhs: &Expr,
        expected_type: Option<Rc<Type>>,
        position: Position,
    ) -> Result<Rc<MirExpr>, Error> {
        let left = self.resolve_expr(lhs, expected_type.clone())?;
        let right = self.resolve_expr(rhs, Some(left.ty()))?;
        let left_type = left.ty();
        let right_type = right.ty();
        let result_type = match op {
            BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Astrick | BinaryOp::Slash => {
                if left_type.is_primitive() && right_type.is_primitive() {
                    if *left_type == *right_type {
                        left_type
                    } else if left_type.is_signed() && right.is_literal() {
                        left_type
                    } else if left.is_literal() && right_type.is_signed() {
                        right_type
                    } else {
                        return Err(Error::incompatible_operands_for_binary_op(
                            op,
                            left_type.as_ref(),
                            right_type.as_ref(),
                        )
                        .with_position(position));
                    }
                } else if left_type.is_struct() || right_type.is_struct() {
                    todo!()
                } else {
                    todo!()
                }
            }
            BinaryOp::Percent
            | BinaryOp::Less
            | BinaryOp::Greater
            | BinaryOp::LessEq
            | BinaryOp::GreaterEq
            | BinaryOp::EqualEqual
            | BinaryOp::BangEqual => {
                if left_type.is_primitive() && right_type.is_primitive() {
                    if *left_type == *right_type {
                        self.type_map.get_bool()
                    } else {
                        return Err(Error::incompatible_operands_for_binary_op(
                            op,
                            left_type.as_ref(),
                            right_type.as_ref(),
                        )
                        .with_position(position));
                    }
                } else if left_type.is_struct() || right_type.is_struct() {
                    todo!()
                } else {
                    todo!()
                }
            }
            BinaryOp::Pipe
            | BinaryOp::Ampersand
            | BinaryOp::LessLess
            | BinaryOp::GreaterGreater => {
                if left_type.is_integer() && right_type.is_integer() {
                    left_type
                } else {
                    todo!()
                }
            }
        };

        Ok(Rc::new(MirExpr::new(
            MirExprKind::Binary(BinaryExpr { op, left, right }),
            position,
            result_type,
        )))
    }

    fn resolve_unary(
        &mut self,
        op: UnaryOp,
        expr: &Expr,
        expected_type: Option<Rc<Type>>,
        position: Position,
    ) -> Result<Rc<MirExpr>, Error> {
        let operand = self.resolve_expr(expr, expected_type.clone())?;
        let ty = operand.ty();
        let result_type = match op {
            UnaryOp::Bang => {
                if ty.is_bool() || ty.is_integer() {
                    ty
                } else {
                    return Err(Error::incompatible_operands_for_unary_op(op, ty.as_ref())
                        .with_position(position));
                }
            }
            UnaryOp::Minus => {
                if ty.is_primitive() {
                    if ty.is_signed() {
                        ty
                    } else {
                        if operand.is_literal() {
                            self.type_map.get_i32()
                        } else {
                            return Err(Error::incompatible_operands_for_unary_op(op, ty.as_ref())
                                .with_position(position));
                        }
                    }
                } else {
                    todo!()
                }
            }
            UnaryOp::Ampersand => todo!("what is an & supposed to do."),
        };

        Ok(Rc::new(MirExpr::new(
            MirExprKind::Unary(UnaryExpr { op, operand }),
            expr.position(),
            result_type,
        )))
    }
}
