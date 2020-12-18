use crate::analysis::scope::ScopeKind;
use crate::analysis::typer::{
    Typer, ALLOW_CONTROL_FLOW_EXPRESSIONS, ASSOCIATIVE_FUNCTION, BLOCK, EXPR_RESULT_USED, FUNCTION,
    FUNCTION_BODY, SELF_PARAM_IDENT,
};
use crate::analysis::{EntityInfo, EntityRef};
use crate::error::Error;
use crate::ir::ast::{
    BinaryOp, Expr, ExprKind, Identifier, Node, NodeType, StructExprField, UnaryOp, Visibility,
};
use crate::ir::hir::{
    AddressMode, AssociatedFunctionExpr, BinaryExpr, BlockExpr, CallExpr, FieldExpr, HirExpr,
    HirExprInner, HirExprKind, HirExprPtr, HirStmtKind, IfExpr, IfExprBranch, IndexExpr, LoopExpr,
    MethodExpr, MirNode, ResultMeta, StructExpr, TupleExpr, UnaryExpr, WhileExpr,
};
use crate::syntax::Position;
use crate::types::{Type, TypeKind};
use crate::{analysis::entity::StructureInfo, utils::EntityPrinter};

use itertools::Itertools;
use std::ops::Deref;
use std::rc::Rc;

macro_rules! with_state {
    ($typer:expr, $state:expr, $body:tt) => {{
        let old_state = $typer.state;
        $typer.state |= $state;
        let res = $body;
        $typer.state = old_state;
        res
    }};
}

impl<'src> Typer<'src> {
    pub(crate) fn resolve_expr(
        &mut self,
        expr: &Expr,
        expected_type: Option<Rc<Type>>,
    ) -> Result<Rc<HirExpr>, Error> {
        println!("Resolving Expr: {}", expr.kind().name());
        let expr = match expr.kind() {
            ExprKind::Integer(val) => {
                let ty = self.type_map.get_i32();
                Rc::new(HirExpr::new(
                    HirExprInner::new(
                        AddressMode::Value,
                        ResultMeta::literal(),
                        HirExprKind::Integer(*val as i64),
                    ),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Float(val) => {
                let ty = self.type_map.get_f32();
                Rc::new(HirExpr::new(
                    HirExprInner::new(
                        AddressMode::Value,
                        ResultMeta::literal(),
                        HirExprKind::Float(*val),
                    ),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::String(val) => {
                let ty = self.type_map.get_string();
                Rc::new(HirExpr::new(
                    HirExprInner::new(
                        AddressMode::Address,
                        ResultMeta::literal(),
                        HirExprKind::String(val.clone()),
                    ),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Char(val) => {
                let ty = self.type_map.get_char();
                Rc::new(HirExpr::new(
                    HirExprInner::new(
                        AddressMode::Value,
                        ResultMeta::literal(),
                        HirExprKind::Char(*val),
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
                        ResultMeta::new(variable.mutable, false, ty.is_mutable(), false, false)
                    }
                    EntityInfo::Structure(_structure) => {
                        ResultMeta::new(false, false, false, false, true)
                    }
                    EntityInfo::Param(_local_info) => {
                        ResultMeta::new(false, false, ty.is_mutable(), false, false)
                    }
                    _ => ResultMeta::new(false, false, false, false, false),
                };

                Rc::new(HirExpr::new(
                    HirExprInner::new(AddressMode::Address, mutable, HirExprKind::Name(name)),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Bool(val) => {
                let ty = self.type_map.get_bool();
                Rc::new(HirExpr::new(
                    HirExprInner::new(
                        ty.address_mode(),
                        ResultMeta::literal(),
                        HirExprKind::Bool(*val),
                    ),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Binary(op, left, right) => self.resolve_binary(
                *op,
                left.as_ref(),
                right.as_ref(),
                // expected_type.clone(),
                None,
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
                        ResultMeta::literal(),
                    ),
                    |stmt| match stmt.inner() {
                        HirStmtKind::Expr(expr) => {
                            let inner = expr.inner();
                            (inner.address_mode(), expr.ty(), inner.meta())
                        }
                        _ => (
                            AddressMode::Value,
                            stmt.ty(),
                            ResultMeta::new(false, false, false, false, false),
                        ),
                    },
                );

                let block_expr = BlockExpr {
                    stmts,
                    return_used: self.check_state(EXPR_RESULT_USED),
                };

                Rc::new(HirExpr::new(
                    HirExprInner::new(address_mode, mutable, HirExprKind::Block(block_expr)),
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
                        let mir_inner = HirExprInner::new(
                            AddressMode::Address,
                            ResultMeta::new(*mutable, false, false, false, false),
                            HirExprKind::SelfLit,
                        );
                        Rc::new(HirExpr::new(
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
                let mir_expr_inner = HirExprInner::new(
                    AddressMode::Value,
                    ResultMeta::new(false, false, false, true, false),
                    HirExprKind::Tuple(tuple_expr),
                );
                Rc::new(HirExpr::new(mir_expr_inner, expr.position(), tuple_type))
            }
            ExprKind::If { .. } => self.resolve_if(expr, None, expr.position())?,
            ExprKind::Loop(body) => self.resolve_loop(body.as_ref(), expr.position())?,
            ExprKind::While(cond, body) => {
                self.resolve_while(cond.as_ref(), body.as_ref(), expr.position())?
            }
            ExprKind::Break => {
                if self.check_state(ALLOW_CONTROL_FLOW_EXPRESSIONS) {
                    let inner = HirExprInner::new(
                        AddressMode::Value,
                        ResultMeta::default(),
                        HirExprKind::Break,
                    );
                    Rc::new(HirExpr::new(
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
                    let inner = HirExprInner::new(
                        AddressMode::Value,
                        ResultMeta::default(),
                        HirExprKind::Continue,
                    );
                    Rc::new(HirExpr::new(
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
                    let mutable = ResultMeta::new(false, false, ty.is_mutable(), false, false);
                    let inner = HirExprInner::new(
                        mir_expr.inner().address_mode(),
                        mutable,
                        HirExprKind::Return(mir_expr),
                    );
                    Rc::new(HirExpr::new(inner, expr.position(), ty))
                } else {
                    let err = Error::invalid_return().with_position(expr.position());
                    return Err(err);
                }
            }
            ExprKind::Index { operand, index } => {
                self.resolve_index(operand, index, expr.position())?
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
                let position = expr
                    .returned_expression()
                    .map(|expr| expr.position())
                    .unwrap_or_else(|| expr.position());

                return Err(
                    Error::incompatible_types(expected_type.as_ref(), expr.ty().as_ref())
                        .with_position(position),
                );
            }
        }

        Ok(expr)
    }

    pub(crate) fn resolve_index(
        &mut self,
        operand: &Expr,
        index: &Expr,
        position: Position,
    ) -> Result<HirExprPtr, Error> {
        let mir_operand = self.resolve_expr(operand, None)?;
        let operand_meta = mir_operand.inner().meta();
        if operand_meta.is_type {
            let err = Error::expecting_instance_type(mir_operand.ty().as_ref());
            return Err(err.with_position(operand.position()));
        }

        let operand_type = mir_operand.ty();
        match Type::inner(operand_type.clone()).kind() {
            TypeKind::Array { element_type, .. } | TypeKind::Slice { element_type } => {
                let mir_index = self.resolve_expr(index, None)?;
                let index_meta = mir_index.inner().meta();
                if index_meta.is_type {
                    let err = Error::expecting_instance_type(mir_index.ty().as_ref());
                    return Err(err.with_position(operand.position()));
                }

                let mir_index_type = mir_index.ty();
                if mir_index_type.is_integer() {
                    let parent_mutable = mir_index.inner().meta();
                    let meta = ResultMeta::new(
                        parent_mutable.mutable,
                        true,
                        element_type.is_mutable(),
                        false,
                        false,
                    );
                    Ok(Rc::new(HirExpr::new(
                        HirExprInner::new(
                            element_type.address_mode(),
                            meta,
                            HirExprKind::Index(IndexExpr {
                                operand: mir_operand,
                                index: mir_index,
                            }),
                        ),
                        operand.position(),
                        element_type.clone(),
                    )))
                } else {
                    let err = Error::incompatible_types(&Type::integer(), mir_index_type.as_ref());
                    Err(err.with_position(index.position()))
                }
            }
            _ => {
                let err = Error::invalid_index_type(operand_type.as_ref());
                Err(err.with_position(position))
            }
        }
    }

    pub(crate) fn resolve_expr_to_entity(
        &mut self,
        expr: &Expr,
    ) -> Result<(EntityRef, Rc<HirExpr>), Error> {
        let mir_expr = self.resolve_expr(expr, None)?;
        let entity = match mir_expr.inner().kind() {
            HirExprKind::Field(field_expr) => field_expr.field.clone(),
            HirExprKind::Name(entity) => entity.clone(),
            _ => {
                let err = Error::invalid_lvalue();
                return Err(err.with_position(expr.position()));
            }
        };

        Ok((entity, mir_expr))
    }

    pub(crate) fn resolve_if(
        &mut self,
        expr: &Expr,
        mut expected_type: Option<Rc<Type>>,
        _position: Position,
    ) -> Result<HirExprPtr, Error> {
        let mut branches = vec![];
        let mut curr_expr = expr;
        let mut first = true;

        loop {
            match curr_expr.kind() {
                ExprKind::If {
                    cond,
                    body,
                    else_if,
                } => {
                    let mir_expr = self.resolve_expr(cond, Some(self.type_map.get_bool()))?;
                    let body = self.resolve_expr(body, expected_type.clone())?;

                    if expected_type.is_none() {
                        expected_type = Some(body.ty());
                    }

                    let if_expr_branch = IfExprBranch::Conditional {
                        cond: mir_expr,
                        body,
                        first,
                    };

                    first = false;

                    branches.push(if_expr_branch);

                    if let Some(expr) = else_if.as_ref() {
                        curr_expr = expr;
                    } else {
                        break;
                    }
                }
                _ => {
                    if first {
                        panic!("Compiler Error: if: first is {}", first);
                    }

                    let mir_expr = self.resolve_expr(curr_expr, expected_type.clone())?;

                    let if_expr_branch = IfExprBranch::Unconditional { body: mir_expr };

                    branches.push(if_expr_branch);
                    break;
                }
            }
        }

        let if_expr = IfExpr { branches };

        if let Some(ty) = expected_type.as_ref() {
            let mutable = ResultMeta::new(false, ty.is_mutable(), false, false, false);
            let inner = HirExprInner::new(ty.address_mode(), mutable, HirExprKind::If(if_expr));
            Ok(Rc::new(HirExpr::new(inner, expr.position(), ty.clone())))
        } else {
            panic!("Compiler Error: Failed to determine the result type of if expression");
        }
    }

    pub(crate) fn resolve_while(
        &mut self,
        cond: &Expr,
        body: &Expr,
        position: Position,
    ) -> Result<HirExprPtr, Error> {
        let mir_cond = self.resolve_expr(cond, Some(self.type_map.get_bool()))?;

        let mir_body = with_state!(self, ALLOW_CONTROL_FLOW_EXPRESSIONS, {
            self.resolve_expr(body, None)?
        });

        let while_expr = WhileExpr {
            cond: mir_cond,
            body: mir_body,
        };

        let inner = HirExprInner::new(
            AddressMode::Value,
            ResultMeta::default(),
            HirExprKind::While(while_expr),
        );

        Ok(Rc::new(HirExpr::new(
            inner,
            position,
            self.type_map.get_unit(),
        )))
    }

    pub(crate) fn resolve_loop(
        &mut self,
        body: &Expr,
        position: Position,
    ) -> Result<HirExprPtr, Error> {
        let mir_body = with_state!(self, ALLOW_CONTROL_FLOW_EXPRESSIONS, {
            self.resolve_expr(body, None)?
        });

        let loop_expr = LoopExpr { body: mir_body };

        let inner = HirExprInner::new(
            AddressMode::Value,
            ResultMeta::default(),
            HirExprKind::Loop(loop_expr),
        );

        Ok(Rc::new(HirExpr::new(
            inner,
            position,
            self.type_map.get_unit(),
        )))
    }

    pub(crate) fn resolve_field_access(
        &mut self,
        operand: &Expr,
        field: &Identifier,
    ) -> Result<Rc<HirExpr>, Error> {
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

                            let parent_mutability_info = operand.inner().meta();
                            let field_expr = FieldExpr {
                                operand,
                                field: field_entity.clone(),
                            };

                            let mutable = ResultMeta::new(
                                parent_mutability_info.mutable,
                                parent_mutability_info.inherited || true,
                                parent_mutability_info.mutable_type,
                                false,
                                field_borrow.is_type(),
                            );

                            std::mem::drop(field_borrow);
                            Ok(Rc::new(HirExpr::new(
                                HirExprInner::new(
                                    AddressMode::Address,
                                    mutable,
                                    HirExprKind::Field(field_expr),
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

    pub(crate) fn resolve_call(
        &mut self,
        operand: &Expr,
        actuals: &[Box<Expr>],
    ) -> Result<Rc<HirExpr>, Error> {
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
                let result_used = self.check_state(EXPR_RESULT_USED);
                let mutable = ResultMeta::funct(
                    false,
                    false,
                    return_type.is_mutable(),
                    false,
                    false,
                    true,
                    result_used,
                );
                let inner =
                    HirExprInner::new(AddressMode::Value, mutable, HirExprKind::Call(call_expr));
                Ok(Rc::new(HirExpr::new(
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

    pub(crate) fn resolve_method_call(
        &mut self,
        name: &Identifier,
        actuals: &[Box<Expr>],
        position: Position,
    ) -> Result<Rc<HirExpr>, Error> {
        assert!(actuals.len() >= 1);
        let receiver_expr = actuals.first().unwrap();
        let mir_expr = self.resolve_expr(receiver_expr.as_ref(), None)?;
        let struct_type = mir_expr.ty();

        let mir_entity = match mir_expr.inner().kind() {
            HirExprKind::Field(field_expr) => field_expr.field.clone(),
            HirExprKind::Name(entity) => entity.clone(),
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

    pub(crate) fn resolve_method_from_entity(
        &mut self,
        associated_type: EntityRef,
        method_entity: EntityRef,
        receiver: Rc<HirExpr>,
        actuals: &[Box<Expr>],
        name: &Identifier,
        position: Position,
    ) -> Result<Rc<HirExpr>, Error> {
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

                            let mutable = ResultMeta::funct(
                                false,
                                false,
                                return_type.is_mutable(),
                                false,
                                false,
                                true,
                                self.check_state(EXPR_RESULT_USED),
                            );
                            // the address_mode should be determined by the address mode of the returned expression
                            let inner = HirExprInner::new(
                                AddressMode::Address,
                                mutable,
                                HirExprKind::AssociatedFunction(associated_function_expr),
                            );
                            Ok(Rc::new(HirExpr::new(inner, position, return_type.clone())))
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

                            let receiver_mutablility = receiver.inner().meta();
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

                            let mutable = ResultMeta::new(
                                false,
                                false,
                                return_type.is_mutable(),
                                false,
                                false,
                            );
                            // the address_mode should be determined by the address mode of the returned expression
                            let inner = HirExprInner::new(
                                AddressMode::Address,
                                mutable,
                                HirExprKind::Method(method_expr),
                            );
                            Ok(Rc::new(HirExpr::new(inner, position, return_type.clone())))
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

    pub(crate) fn resolve_struct_expr(
        &mut self,
        struct_type: Rc<Type>,
        fields: &[StructExprField],
        structure: &StructureInfo,
        position: Position,
    ) -> Result<HirExprPtr, Error> {
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
        let mir_expr = HirExprKind::StructExpr(struct_expr);
        let mutable = ResultMeta::new(false, false, false, true, false);
        let mir_inner = HirExprInner::new(AddressMode::Value, mutable, mir_expr);
        Ok(Rc::new(HirExpr::new(mir_inner, position, struct_type)))
    }

    pub(crate) fn resolve_type_expr(&mut self, expr: &Expr) -> Result<EntityRef, Error> {
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

    pub(crate) fn resolve_binary(
        &mut self,
        op: BinaryOp,
        lhs: &Expr,
        rhs: &Expr,
        expected_type: Option<Rc<Type>>,
        position: Position,
    ) -> Result<Rc<HirExpr>, Error> {
        let left = self.resolve_expr(lhs, expected_type.clone())?;
        let right = self.resolve_expr(rhs, Some(left.ty()))?;
        let left_type = Type::inner(left.ty());
        let right_type = Type::inner(right.ty());
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
            BinaryOp::Less
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
            BinaryOp::Percent
            | BinaryOp::Pipe
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

        let mutable = ResultMeta::new(false, false, false, true, false);
        Ok(Rc::new(HirExpr::new(
            HirExprInner::new(
                address_mode,
                mutable,
                HirExprKind::Binary(BinaryExpr { op, left, right }),
            ),
            position,
            result_type,
        )))
    }

    pub(crate) fn resolve_unary(
        &mut self,
        op: UnaryOp,
        expr: &Expr,
        expected_type: Option<Rc<Type>>,
        position: Position,
    ) -> Result<Rc<HirExpr>, Error> {
        let operand = self.resolve_expr(expr, expected_type.clone())?;
        let ty = Type::inner(operand.ty());
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

        let mutable = ResultMeta::new(false, false, false, true, false);
        Ok(Rc::new(HirExpr::new(
            HirExprInner::new(
                address_mode,
                mutable,
                HirExprKind::Unary(UnaryExpr { op, operand }),
            ),
            expr.position(),
            result_type,
        )))
    }
}
