use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use itertools::Itertools;

use crate::analysis::entity::{FunctionInfo, LocalInfo, StructureInfo, VariableInfo};
use crate::analysis::{Entity, EntityInfo, EntityRef};
use crate::error::Error;
use crate::mir::{
    AddressMode, Assignment, AssociatedFunctionExpr, BinaryExpr, BlockExpr, CallExpr, LoopExpr,
    MethodExpr, MirExpr, MirExprKind, MirExprPtr, MirFile, MirNode, MirSpec, MirSpecKind, MirStmt,
    MirStmtKind, MutabilityInfo, StructExpr, TupleExpr, UnaryExpr, WhileExpr,
};
use crate::syntax::ast::{
    AssignmentOp, BinaryOp, Expr, ExprKind, FunctionBody, Identifier, Item, ItemKind, Node,
    NodeType, Spec, SpecKind, Stmt, StmtKind, StructExprField, UnaryOp, Visibility,
};
use crate::syntax::{ParsedFile, Position};
use crate::types::{Type, TypeKind, TypeMap};
use crate::utils::{new_ptr, Ptr};
use crate::{
    analysis::scope::{Scope, ScopeKind, ScopeRef},
    mir::{FieldExpr, MirExprInner},
};

use super::entity::{AssociatedFunctionInfo, Path};

type State = u64;

const DEFAULT: State = 0;
const BLOCK: State = 1 << 0;
const EXPR_RESULT_USED: State = 1 << 1;
const STRUCT: State = 1 << 2;
const FUNCTION: State = 1 << 3;
const FUNCTION_PARAM: State = 1 << 4;
const FUNCTION_BODY: State = 1 << 5;
const ASSOCIATIVE_FUNCTION: State = 1 << 6;
const ALLOW_CONTROL_FLOW_EXPRESSIONS: State = 1 << 7;
const SELF_PARAM_IDENT: &'static str = "__self__";

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
    scope_stack: &'a mut Vec<Scope>,
    state: State,
    self_entity: Option<EntityRef>,
}

impl<'a> Typer<'a> {
    pub fn new(type_map: &'a mut TypeMap, scope_stack: &'a mut Vec<Scope>) -> Self {
        Self {
            type_map,
            scope_stack,
            state: DEFAULT,
            self_entity: None,
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

    fn set_self(&mut self, entity: EntityRef) {
        self.self_entity = Some(entity);
    }

    fn unset_self(&mut self) {
        self.self_entity = None;
    }

    fn pop_scope(&mut self) -> ScopeRef {
        let last_scope = self.scope_stack.pop().unwrap();
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

    fn compare_types(&self, expected: Rc<Type>, found: Rc<Type>, ignore_mutable: bool) -> bool {
        let expected = if ignore_mutable {
            Type::inner(expected)
        } else {
            expected
        };

        let found = if ignore_mutable {
            Type::inner(found)
        } else {
            found
        };

        expected.as_ref() != found.as_ref()
    }

    fn current_path_from_root(&self) -> Path {
        let mut path = Path::empty();
        for scope in self.scope_stack.iter() {
            match scope.kind() {
                ScopeKind::File {
                    file_id: _,
                    file_name,
                } => {
                    path.push_path(file_name.as_str());
                }
                ScopeKind::Struct(name) => path.push_path(name.as_str()),
                ScopeKind::StructMethods(name) => path.push_path(name.as_str()),
                _ => {}
            }
        }
        path
    }

    pub fn resolve_file(mut self, parsed_file: ParsedFile) -> Result<MirFile, Error> {
        self.push_scope(ScopeKind::File {
            file_id: parsed_file.file_id,
            file_name: parsed_file.file_name.clone(),
        });
        println!("Stmts: {}", parsed_file.stmts.len());
        let mut globals = vec![];

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
            let stmt = self.resolve_stmt_inner(stmt.as_ref(), true)?;
            match stmt.inner() {
                MirStmtKind::Expr(..) | MirStmtKind::Assignment(..) => globals.push(stmt.clone()),
                _ => {}
            }
        }

        let entities = self
            .current_scope()
            .elements()
            .values()
            .map(|entity| entity.clone())
            .collect();

        self.pop_scope();

        Ok(MirFile::new(parsed_file.file_id, globals, entities))
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<Rc<MirStmt>, Error> {
        self.resolve_stmt_inner(stmt, false)
    }

    fn resolve_stmt_inner(&mut self, stmt: &Stmt, top_level: bool) -> Result<Rc<MirStmt>, Error> {
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
                let entity = if top_level {
                    self.resolve_top_level_item(item.as_ref())?
                } else {
                    self.resolve_item(item.as_ref())?
                };

                let position = item.position();
                // let ty = entity.deref().borrow().ty();
                Ok(Rc::new(MirStmt::new(
                    MirStmtKind::Item(entity),
                    position,
                    self.type_map.get_unit(),
                )))
            }
            StmtKind::Assignment { op, lvalue, rhs } => {
                let (entity, mir_lvalue) = self.resolve_expr_to_entity(lvalue.as_ref())?;
                // let lvalue_type = mir_lvalue.ty();
                let mutability = mir_lvalue.inner().mutable();
                if !mutability.mutable {
                    let err = Error::immutable_entity(entity.deref().borrow().name());
                    return Err(err.with_position(lvalue.position()));
                }
                match op {
                    AssignmentOp::Assign => {
                        let lvalue_type = mir_lvalue.ty();
                        let rhs = self.resolve_expr(rhs.as_ref(), Some(lvalue_type))?;
                        let assignment = Assignment {
                            op: *op,
                            lvalue: entity,
                            rhs,
                        };

                        Ok(Rc::new(MirStmt::new(
                            MirStmtKind::Assignment(assignment),
                            stmt.position(),
                            self.type_map.get_unit(),
                        )))
                    }
                    _ => todo!("Assignment operator {} is not implemented", op),
                }
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
                    MirExprInner::new(
                        AddressMode::Value,
                        MutabilityInfo::literal(),
                        MirExprKind::Integer(*val),
                    ),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Float(val) => {
                let ty = self.type_map.get_f32();
                Rc::new(MirExpr::new(
                    MirExprInner::new(
                        AddressMode::Value,
                        MutabilityInfo::literal(),
                        MirExprKind::Float(*val),
                    ),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::String(val) => {
                let ty = self.type_map.get_string();
                Rc::new(MirExpr::new(
                    MirExprInner::new(
                        AddressMode::Address,
                        MutabilityInfo::literal(),
                        MirExprKind::String(val.clone()),
                    ),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Char(val) => {
                let ty = self.type_map.get_char();
                Rc::new(MirExpr::new(
                    MirExprInner::new(
                        AddressMode::Value,
                        MutabilityInfo::literal(),
                        MirExprKind::Char(*val),
                    ),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Name(ident) => {
                let name = self.resolve_ident(ident)?;
                let ty = name.deref().borrow().ty();
                let mutable = match name.deref().borrow().kind() {
                    EntityInfo::Variable(variable) => {
                        MutabilityInfo::new(variable.mutable, false, ty.is_mutable(), false, false)
                    }
                    EntityInfo::Structure(_structure) => {
                        MutabilityInfo::new(false, false, false, false, true)
                    }
                    EntityInfo::Param(_local_info) => {
                        MutabilityInfo::new(false, false, ty.is_mutable(), false, false)
                    }
                    _ => MutabilityInfo::new(false, false, false, false, false),
                };
                Rc::new(MirExpr::new(
                    MirExprInner::new(AddressMode::Address, mutable, MirExprKind::Name(name)),
                    expr.position(),
                    ty,
                ))
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

                // the result of the block is the result of the last expression in the block, this includes the addressing, type, and mutability
                let (address_mode, return_type, mutable) = stmts.last().map_or(
                    (
                        AddressMode::Value,
                        self.type_map.get_unit(),
                        MutabilityInfo::literal(),
                    ),
                    |stmt| match stmt.inner() {
                        MirStmtKind::Expr(expr) => {
                            let inner = expr.inner();
                            (inner.address_mode(), expr.ty(), inner.mutable())
                        }
                        _ => (
                            AddressMode::Value,
                            stmt.ty(),
                            MutabilityInfo::new(false, false, false, false, false),
                        ),
                    },
                );

                let block_expr = BlockExpr {
                    stmts,
                    return_used: self.check_state(EXPR_RESULT_USED),
                };

                Rc::new(MirExpr::new(
                    MirExprInner::new(address_mode, mutable, MirExprKind::Block(block_expr)),
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
            ExprKind::SelfLit => {
                if self.check_state(ASSOCIATIVE_FUNCTION) {
                    let self_entity = match self.deep_lookup(SELF_PARAM_IDENT) {
                        Some(entity) => entity,
                        None => {
                            let err =
                                Error::invalid_self_in_function().with_position(expr.position());
                            return Err(err);
                        }
                    };

                    let self_borrow = self_entity.deref().borrow();
                    if let EntityInfo::SelfParam { mutable } = self_borrow.kind() {
                        let mir_inner = MirExprInner::new(
                            AddressMode::Address,
                            MutabilityInfo::new(*mutable, false, false, false, false),
                            MirExprKind::SelfLit,
                        );
                        Rc::new(MirExpr::new(
                            mir_inner,
                            expr.position(),
                            self_entity.deref().borrow().ty(),
                        ))
                    } else {
                        let err = Error::other(
                            "Compiler Error: 'self' entity is not a Self Entity type".to_owned(),
                        );
                        return Err(err.with_position(expr.position()));
                    }
                } else {
                    let err = Error::invalid_self_expression().with_position(expr.position());
                    return Err(err);
                }
            }
            ExprKind::Field(operand, field) => {
                self.resolve_field_access(operand.as_ref(), field.as_ref())?
            }
            ExprKind::Call { operand, actual } => {
                self.resolve_call(operand.as_ref(), actual.as_slice())?
            }
            ExprKind::Method { name, actual } => {
                self.resolve_method_call(name.as_ref(), actual, expr.position())?
            }
            ExprKind::Tuple(elements) => {
                let mut mir_elements = vec![];
                for element in elements {
                    let mir_expr = self.resolve_expr(element.as_ref(), None)?;
                    mir_elements.push(mir_expr);
                }

                let elements = mir_elements
                    .iter()
                    .map(|element| element.ty())
                    .collect_vec();

                let tuple_expr = TupleExpr {
                    elements: mir_elements,
                };

                let tuple_type = self.insert_type(TypeKind::Tuple { elements });
                let mir_expr_inner = MirExprInner::new(
                    AddressMode::Value,
                    MutabilityInfo::new(false, false, false, true, false),
                    MirExprKind::Tuple(tuple_expr),
                );
                Rc::new(MirExpr::new(mir_expr_inner, expr.position(), tuple_type))
            }
            ExprKind::If {
                cond,
                body,
                else_if,
            } => self.resolve_if(
                cond.as_ref(),
                body.as_ref(),
                else_if.as_ref().map(AsRef::as_ref),
                expr.position(),
            )?,
            ExprKind::Loop(body) => self.resolve_loop(body.as_ref(), expr.position())?,
            ExprKind::While(cond, body) => {
                self.resolve_while(cond.as_ref(), body.as_ref(), expr.position())?
            }
            ExprKind::Break => {
                if self.check_state(ALLOW_CONTROL_FLOW_EXPRESSIONS) {
                    let inner = MirExprInner::new(
                        AddressMode::Value,
                        MutabilityInfo::default(),
                        MirExprKind::Break,
                    );
                    Rc::new(MirExpr::new(
                        inner,
                        expr.position(),
                        self.type_map.get_unit(),
                    ))
                } else {
                    let err =
                        Error::invalid_control_in_loop("break").with_position(expr.position());
                    return Err(err);
                }
            }
            ExprKind::Continue => {
                if self.check_state(ALLOW_CONTROL_FLOW_EXPRESSIONS) {
                    let inner = MirExprInner::new(
                        AddressMode::Value,
                        MutabilityInfo::default(),
                        MirExprKind::Continue,
                    );
                    Rc::new(MirExpr::new(
                        inner,
                        expr.position(),
                        self.type_map.get_unit(),
                    ))
                } else {
                    let err =
                        Error::invalid_control_in_loop("continue").with_position(expr.position());
                    return Err(err);
                }
            }
            ExprKind::Return(expr) => {
                if self.check_state(FUNCTION | FUNCTION_BODY)
                    || self.check_state(ASSOCIATIVE_FUNCTION | FUNCTION_BODY)
                {
                    let mir_expr = self.resolve_expr(expr, None)?;
                    let ty = mir_expr.ty();
                    let mutable = MutabilityInfo::new(false, false, ty.is_mutable(), false, false);
                    let inner = MirExprInner::new(
                        mir_expr.inner().address_mode(),
                        mutable,
                        MirExprKind::Return(mir_expr),
                    );
                    Rc::new(MirExpr::new(inner, expr.position(), ty))
                } else {
                    let err = Error::invalid_return().with_position(expr.position());
                    return Err(err);
                }
            }
            /*
            ExprKind::For {
                element,
                expr,
                body,
            } => {}
            */
            _ => todo!(),
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

    fn resolve_expr_to_entity(&mut self, expr: &Expr) -> Result<(EntityRef, Rc<MirExpr>), Error> {
        let mir_expr = self.resolve_expr(expr, None)?;
        let entity = match mir_expr.inner().kind() {
            MirExprKind::Field(field_expr) => field_expr.field.clone(),
            MirExprKind::Name(entity) => entity.clone(),
            _ => {
                let err = Error::invalid_lvalue();
                return Err(err.with_position(expr.position()));
            }
        };

        Ok((entity, mir_expr))
    }

    fn resolve_if(
        &mut self,
        cond: &Expr,
        body: &Expr,
        else_if: Option<&Expr>,
        position: Position,
    ) -> Result<MirExprPtr, Error> {
        todo!()
    }

    fn resolve_while(
        &mut self,
        cond: &Expr,
        body: &Expr,
        position: Position,
    ) -> Result<MirExprPtr, Error> {
        let mir_cond = self.resolve_expr(cond, Some(self.type_map.get_bool()))?;

        let mir_body = with_state!(self, ALLOW_CONTROL_FLOW_EXPRESSIONS, {
            self.resolve_expr(body, None)?
        });

        let while_expr = WhileExpr {
            cond: mir_cond,
            body: mir_body,
        };

        let inner = MirExprInner::new(
            AddressMode::Value,
            MutabilityInfo::default(),
            MirExprKind::While(while_expr),
        );

        Ok(Rc::new(MirExpr::new(
            inner,
            position,
            self.type_map.get_unit(),
        )))
    }

    fn resolve_loop(&mut self, body: &Expr, position: Position) -> Result<MirExprPtr, Error> {
        let mir_body = with_state!(self, ALLOW_CONTROL_FLOW_EXPRESSIONS, {
            self.resolve_expr(body, None)?
        });

        let loop_expr = LoopExpr { body: mir_body };

        let inner = MirExprInner::new(
            AddressMode::Value,
            MutabilityInfo::default(),
            MirExprKind::Loop(loop_expr),
        );

        Ok(Rc::new(MirExpr::new(
            inner,
            position,
            self.type_map.get_unit(),
        )))
    }

    fn resolve_field_access(
        &mut self,
        operand: &Expr,
        field: &Identifier,
    ) -> Result<Rc<MirExpr>, Error> {
        let operand = self.resolve_expr(operand, None)?;
        // get any inner type when appropriate (ie. mut T -> T)
        let operand_type = Type::inner(operand.ty());
        match operand_type.kind() {
            TypeKind::Struct { entity } => {
                // we know it is a structure at this point.
                let entity_borrow = entity.deref().borrow();
                let structure_info = entity_borrow.as_struct();
                if let Some(field_entity) = structure_info
                    .fields
                    .elements()
                    .get(field.kind().value.as_str())
                {
                    let field_borrow = field_entity.deref().borrow();
                    if !operand.inner().kind().is_self() {
                        // otherwise, we need to check if this field can be access in this context.
                        match field_borrow.visibility() {
                            Visibility::Private => {
                                let err = Error::inaccessible_subentity(
                                    "field",
                                    operand_type.as_ref(),
                                    field.kind().value.clone(),
                                )
                                .with_position(field.position());
                                return Err(err);
                            }
                            Visibility::Public => { /* Continue */ }
                        }
                    }

                    // if the parent is a 'self' then we do not care what the visibility of the field is
                    match field_borrow.kind() {
                        EntityInfo::Field(_field_info) => {
                            let position = field.position();
                            let ty = field_borrow.ty();

                            let parent_mutability_info = operand.inner().mutable();
                            let field_expr = FieldExpr {
                                operand,
                                field: field_entity.clone(),
                            };

                            let mutable = MutabilityInfo::new(
                                parent_mutability_info.mutable,
                                parent_mutability_info.inherited || true,
                                parent_mutability_info.mutable_type,
                                false,
                                field_borrow.is_type(),
                            );

                            std::mem::drop(field_borrow);
                            Ok(Rc::new(MirExpr::new(
                                MirExprInner::new(
                                    AddressMode::Address,
                                    mutable,
                                    MirExprKind::Field(field_expr),
                                ),
                                position,
                                ty,
                            )))
                        }
                        _ => todo!(),
                    }
                } else {
                    let err = Error::unknown_subfield(
                        "field",
                        operand_type.as_ref(),
                        field.kind().value.clone(),
                    )
                    .with_position(field.position());
                    return Err(err);
                }
            }
            _ => todo!("Operand Type: {}", operand_type),
        }
    }

    fn resolve_call(
        &mut self,
        operand: &Expr,
        actuals: &[Box<Expr>],
    ) -> Result<Rc<MirExpr>, Error> {
        let mir_operand = self.resolve_expr(operand, None)?;
        println!("Function Type: {}", mir_operand.ty());
        let function_type = mir_operand.ty();
        match function_type.kind() {
            TypeKind::Function {
                params,
                return_type,
            } => {
                if params.len() != actuals.len() {
                    let err = Error::invalid_actuals(params.len(), actuals.len())
                        .with_position(operand.position());
                    return Err(err);
                }
                let mut mir_actuals = vec![];
                for (act, param) in actuals.iter().zip(params) {
                    let mir_actual = self.resolve_expr(act, Some(param.clone()))?;
                    mir_actuals.push(mir_actual);
                }
                let call_expr = CallExpr {
                    operand: mir_operand,
                    function_type: function_type.clone(),
                    actuals: mir_actuals,
                };

                let mutable =
                    MutabilityInfo::new(false, false, return_type.is_mutable(), false, false);
                let inner =
                    MirExprInner::new(AddressMode::Value, mutable, MirExprKind::Call(call_expr));
                Ok(Rc::new(MirExpr::new(
                    inner,
                    operand.position(),
                    return_type.clone(),
                )))
            }
            _ => {
                let err = Error::invalid_call_on_type(mir_operand.ty().as_ref());
                return Err(err.with_position(operand.position()));
            }
        }
    }

    fn resolve_method_call(
        &mut self,
        name: &Identifier,
        actuals: &[Box<Expr>],
        position: Position,
    ) -> Result<Rc<MirExpr>, Error> {
        assert!(actuals.len() >= 1);
        let receiver_expr = actuals.first().unwrap();
        let mir_expr = self.resolve_expr(receiver_expr.as_ref(), None)?;
        let struct_type = mir_expr.ty();

        let mir_entity = match mir_expr.inner().kind() {
            MirExprKind::Field(field_expr) => field_expr.field.clone(),
            MirExprKind::Name(entity) => entity.clone(),
            _ => {
                let err = Error::invalid_lvalue();
                return Err(err.with_position(mir_expr.position()));
            }
        };

        let name_str = name.kind().value.as_str();
        match struct_type.kind() {
            TypeKind::Struct { entity } => {
                if let EntityInfo::Structure(structure_info) = entity.deref().borrow().kind() {
                    if let Some(method) = structure_info.methods.get(name_str) {
                        self.resolve_method_from_entity(
                            mir_entity.clone(),
                            method.clone(),
                            mir_expr,
                            actuals,
                            name,
                            position,
                        )
                    } else if let Some(_field) = structure_info.fields.get(name_str) {
                        panic!()
                    } else {
                        return Err(Error::unknown_subentity(
                            "associated function",
                            name_str,
                            struct_type.as_ref(),
                        )
                        .with_position(name.position()));
                    }
                } else {
                    panic!("Compiler Error: structure type entity is not a struct entity")
                }
            }
            _ => panic!(),
        }
    }

    fn resolve_method_from_entity(
        &mut self,
        associated_type: EntityRef,
        method_entity: EntityRef,
        receiver: Rc<MirExpr>,
        actuals: &[Box<Expr>],
        name: &Identifier,
        position: Position,
    ) -> Result<Rc<MirExpr>, Error> {
        let method_borrow = method_entity.deref().borrow();
        match method_borrow.kind() {
            EntityInfo::AssociatedFunction(associated_function_info) => {
                let method_type = method_borrow.ty();
                if associated_type.deref().borrow().is_type() {
                    if associated_function_info.entity.deref().borrow().id()
                        != associated_type.deref().borrow().id()
                    {
                        panic!("Compiler Error: invalid expected receiver type with found receiver: {} -> {}", associated_function_info.entity.deref().borrow().full_name(), associated_type.deref().borrow().full_name());
                    }
                    if associated_function_info.takes_self {
                        let err =
                            Error::invalid_associated_function_receiver(name.kind().value.as_str());
                        return Err(err.with_position(name.position()));
                    }

                    match method_type.kind() {
                        TypeKind::Function {
                            params,
                            return_type,
                        } => {
                            if params.len() != actuals.len() - 1 {
                                let err = Error::invalid_actuals(params.len(), actuals.len())
                                    .with_position(name.position());
                                return Err(err);
                            }
                            let mut mir_actuals = vec![receiver.clone()];
                            for (act, param) in actuals.iter().skip(1).zip(params) {
                                let mir_actual = self.resolve_expr(act, Some(param.clone()))?;
                                mir_actuals.push(mir_actual);
                            }
                            let associated_function_expr = AssociatedFunctionExpr {
                                function_type: method_type.clone(),
                                name: name.kind().value.clone(),
                                actuals: mir_actuals,
                            };

                            let mutable = MutabilityInfo::new(
                                false,
                                false,
                                return_type.is_mutable(),
                                false,
                                false,
                            );
                            // the address_mode should be determined by the address mode of the returned expression
                            let inner = MirExprInner::new(
                                AddressMode::Address,
                                mutable,
                                MirExprKind::AssociatedFunction(associated_function_expr),
                            );
                            Ok(Rc::new(MirExpr::new(inner, position, return_type.clone())))
                        }
                        _ => {
                            let err = Error::invalid_call_on_type(method_borrow.ty().as_ref());
                            return Err(err.with_position(name.position()));
                        }
                    }
                } else if associated_type.deref().borrow().is_instance() {
                    if !associated_function_info.takes_self {
                        let err =
                            Error::associated_function_invalid_receiver(name.kind().value.as_str());
                        return Err(err.with_position(name.position()));
                    }

                    match method_type.kind() {
                        TypeKind::Function {
                            params,
                            return_type,
                        } => {
                            if params.len() != actuals.len() {
                                let err = Error::invalid_actuals(params.len(), actuals.len())
                                    .with_position(name.position());
                                return Err(err);
                            }

                            let receiver_mutablility = receiver.inner().mutable();
                            let expected_receiver_type = params.first().unwrap();

                            if self.compare_types(
                                expected_receiver_type.clone(),
                                receiver.ty(),
                                receiver_mutablility.mutable,
                            ) {
                                let err = Error::incompatible_types(
                                    expected_receiver_type.as_ref(),
                                    receiver.ty().as_ref(),
                                );
                                return Err(err.with_position(name.position()));
                            }

                            let mut mir_actuals = vec![receiver.clone()];
                            for (act, param) in actuals.iter().zip(params).skip(1) {
                                let mir_actual = self.resolve_expr(act, Some(param.clone()))?;
                                mir_actuals.push(mir_actual);
                            }

                            let method_expr = MethodExpr {
                                function_type: method_type.clone(),
                                name: name.kind().value.clone(),
                                actuals: mir_actuals,
                            };

                            let mutable = MutabilityInfo::new(
                                false,
                                false,
                                return_type.is_mutable(),
                                false,
                                false,
                            );
                            // the address_mode should be determined by the address mode of the returned expression
                            let inner = MirExprInner::new(
                                AddressMode::Address,
                                mutable,
                                MirExprKind::Method(method_expr),
                            );
                            Ok(Rc::new(MirExpr::new(inner, position, return_type.clone())))
                        }
                        _ => {
                            let err = Error::invalid_call_on_type(method_borrow.ty().as_ref());
                            return Err(err.with_position(name.position()));
                        }
                    }
                } else {
                    unimplemented!()
                }
            }
            _ => {
                let err = Error::invalid_call_on_type(method_borrow.ty().as_ref())
                    .with_position(name.position());
                Err(err)
            }
        }
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
                    if let Some(field) = structure.fields.elements().get(field_name) {
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
                        if let Some(field) = structure.fields.elements().get(field_name) {
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
        let mutable = MutabilityInfo::new(false, false, false, true, false);
        let mir_inner = MirExprInner::new(AddressMode::Value, mutable, mir_expr);
        Ok(Rc::new(MirExpr::new(mir_inner, position, struct_type)))
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
        if let Some(_) = self.shallow_lookup(name.kind().value.as_str()) {
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
        _vis: Visibility,
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
            mutable,
            default: init.clone(),
        };

        entity.borrow_mut().resolve(
            result_type,
            EntityInfo::Variable(variable_info),
            Path::empty(),
        );

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
        _vis: Visibility,
        name: &Identifier,
        fields: &[Box<Item>],
        _position: Position,
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

        let ty = self.insert_type(TypeKind::Struct {
            entity: entity.clone(),
        });

        let structure_info = StructureInfo {
            fields: fields_scope,
            methods: Scope::new_ref(ScopeKind::Invalid, None),
        };

        let path = self.current_path_from_root();

        entity
            .borrow_mut()
            .resolve(ty, EntityInfo::Structure(structure_info), path);

        with_state!(self, ASSOCIATIVE_FUNCTION, {
            let mut entities = vec![];

            self.set_self(entity.clone());
            self.push_scope(ScopeKind::StructMethods(name.kind().value.clone()));

            for method in methods.iter() {
                if let ItemKind::Function { vis, name, .. } = method.kind() {
                    let entity = new_ptr(Entity::unresolved(
                        *vis,
                        name.kind().value.clone(),
                        method.deref().clone(),
                        self.type_map.get_invalid(),
                    ));
                    entities.push(entity.clone());
                    self.insert_entity(name.kind().value.as_str(), entity);
                }
            }

            for (&method, entity) in methods.iter().zip(entities) {
                match method.kind() {
                    ItemKind::Function {
                        vis,
                        name,
                        params,
                        ret,
                        body,
                    } => {
                        self.resolve_function(
                            entity,
                            *vis,
                            name,
                            params,
                            ret,
                            body,
                            method.position(),
                            true,
                        )?;
                    }
                    _ => todo!(),
                }
            }

            let method_scope = self.pop_scope();
            self.unset_self();
            entity.deref().borrow_mut().as_struct_mut().methods = method_scope;
        });

        if !declared {
            self.insert_entity(name.kind().value.as_str(), entity.clone());
        }

        Ok(entity)
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
                self.current_path_from_root(),
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
        _vis: Visibility,
        name: &Identifier,
        params: &[Box<Item>],
        return_spec: &Spec,
        body: &FunctionBody,
        position: Position,
        declared: bool,
    ) -> Result<EntityRef, Error> {
        let mut function_params = Vec::with_capacity(params.len());
        let mut takes_self = false;
        let _mir_items = with_state!(self, FUNCTION_PARAM, {
            let mut mir_items = vec![];
            self.push_scope(ScopeKind::Param(name.kind().value.clone()));
            let mut start_index = 0;
            for param in params {
                match param.kind() {
                    ItemKind::Param { names, spec, init } => {
                        let params = self.resolve_param(
                            names,
                            spec.as_ref(),
                            init.as_ref(),
                            start_index,
                            param.position(),
                        )?;
                        start_index += names.len();

                        function_params
                            .extend(params.iter().map(|param| param.deref().borrow().ty()));
                        mir_items.extend(params);
                    }
                    ItemKind::SelfParam { mutable } => {
                        if !self.check_state(ASSOCIATIVE_FUNCTION) {
                            let err = Error::invalid_self_declared_in_function()
                                .with_position(param.position());
                            return Err(err);
                        }
                        if start_index != 0 {
                            let err =
                                Error::unexpected_self_parameter().with_position(param.position());
                            return Err(err);
                        }
                        takes_self = true;
                        if let Some(entity) = self.self_entity.as_ref() {
                            let ty = entity.deref().borrow().ty();
                            std::mem::drop(entity);

                            let result_type = if *mutable {
                                self.insert_type(TypeKind::Mutable { inner: ty })
                            } else {
                                ty
                            };

                            let param = new_ptr(Entity::new(
                                Visibility::Private,
                                "self".to_string(),
                                result_type.clone(),
                                EntityInfo::SelfParam { mutable: *mutable },
                                Path::empty(),
                            ));
                            start_index += 1;

                            self.insert_entity(SELF_PARAM_IDENT, param.clone());
                            function_params.push(result_type);
                            mir_items.push(param);
                        }
                    }
                    _ => panic!("Compiler Error: unexpected param item"),
                }
            }
            Ok(mir_items)
        })?;

        let (return_type, _mir_spec, mir_expr) =
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

        let path = self.current_path_from_root();
        if self.check_state(ASSOCIATIVE_FUNCTION) {
            let associated_function_info = AssociatedFunctionInfo {
                entity: self.self_entity.as_ref().map(|e| e.clone()).unwrap(),
                params: params_scope,
                body_scope,
                body: mir_expr,
                takes_self,
            };

            entity.borrow_mut().resolve(
                function_type,
                EntityInfo::AssociatedFunction(associated_function_info),
                path,
            );
        } else {
            let function_info = FunctionInfo {
                params: params_scope,
                body_scope,
                body: mir_expr,
            };

            entity
                .borrow_mut()
                .resolve(function_type, EntityInfo::Function(function_info), path);
        }

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
                Path::empty(),
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
            ExprKind::SelfType => {
                if let Some(entity) = self.self_entity.as_ref() {
                    Ok(entity.clone())
                } else {
                    if !self.check_state(ASSOCIATIVE_FUNCTION) {
                        Err(Error::invalid_self_type_in_context().with_position(expr.position()))
                    } else {
                        panic!("Compiler Error: Self not set in associative function")
                    }
                }
            }
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
        let (address_mode, result_type) = match op {
            BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Astrick | BinaryOp::Slash => {
                if left_type.is_primitive() && right_type.is_primitive() {
                    if *left_type == *right_type {
                        (AddressMode::Value, left_type)
                    } else if left_type.is_signed() && right.is_literal() {
                        (AddressMode::Value, left_type)
                    } else if left.is_literal() && right_type.is_signed() {
                        (AddressMode::Value, right_type)
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
                        (AddressMode::Value, self.type_map.get_bool())
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
                    (AddressMode::Value, left_type)
                } else {
                    todo!()
                }
            }
        };

        let mutable = MutabilityInfo::new(false, false, false, true, false);
        Ok(Rc::new(MirExpr::new(
            MirExprInner::new(
                address_mode,
                mutable,
                MirExprKind::Binary(BinaryExpr { op, left, right }),
            ),
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
        let (address_mode, result_type) = match op {
            UnaryOp::Bang => {
                if ty.is_bool() || ty.is_integer() {
                    (AddressMode::Value, ty)
                } else {
                    return Err(Error::incompatible_operands_for_unary_op(op, ty.as_ref())
                        .with_position(position));
                }
            }
            UnaryOp::Minus => {
                if ty.is_primitive() {
                    if ty.is_signed() {
                        (AddressMode::Value, ty)
                    } else {
                        if operand.is_literal() {
                            (AddressMode::Value, self.type_map.get_i32())
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

        let mutable = MutabilityInfo::new(false, false, false, true, false);
        Ok(Rc::new(MirExpr::new(
            MirExprInner::new(
                address_mode,
                mutable,
                MirExprKind::Unary(UnaryExpr { op, operand }),
            ),
            expr.position(),
            result_type,
        )))
    }
}
