use crate::analysis::entity::{EntityInfo, EntityRef};
use crate::analysis::scope::{Scope, ScopeKind, ScopeRef};
use crate::analysis::Entity;
use crate::error::Error;
use crate::mir::{
    BinaryExpr, Function, MirExpr, MirExprKind, MirFile, MirItem, MirItemKind, MirNode,
    MirNodeBase, MirParam, MirSpec, MirSpecKind, MirStmt, MirStmtKind, Param, UnaryExpr, Variable,
};
use crate::syntax::ast::{
    BinaryOp, Expr, ExprKind, FunctionBody, Identifier, Item, ItemKind, Node, NodeType, Spec,
    SpecKind, Stmt, StmtKind, UnaryOp, Visibility,
};
use crate::syntax::{ParsedFile, Position};
use crate::types::{Type, TypeKind, TypeMap};
use std::ops::Deref;
use std::rc::Rc;

pub(crate) struct Typer<'a> {
    type_map: &'a mut TypeMap,
    item_stack: &'a mut Vec<Box<Item>>,
    scope_stack: &'a mut Vec<Scope>,
    entities: Vec<EntityRef>,
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
        }
    }

    fn push_scope(&mut self, kind: ScopeKind) {
        let new_scope = Scope::new(kind, None);
        self.scope_stack.push(new_scope);
    }

    fn insert_entity(&mut self, name: &str, entity: EntityRef) {
        self.current_scope_mut().add_element(name, entity);
    }

    fn pop_scope(&mut self) {
        let mut last_scope = self.scope_stack.pop().unwrap();
        self.current_scope_mut().add_child(Rc::new(last_scope));
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
        let mut stmts = vec![];

        for stmt in &parsed_file.stmts {
            let stmt = self.resolve_stmt(stmt.as_ref())?;
            stmts.push(stmt);
        }

        self.pop_scope();

        Ok(MirFile::new(parsed_file.file_id, stmts, self.entities))
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<Box<MirStmt>, Error> {
        println!("Resolving Stmt {}", stmt.kind().name());

        match stmt.kind() {
            StmtKind::Expr(expr) => {
                let expr = self.resolve_expr(expr.as_ref(), None)?;
                let position = expr.position();
                let ty = expr.ty();
                Ok(Box::new(MirStmt::new(
                    MirStmtKind::Expr(expr),
                    position,
                    ty,
                )))
            }
            StmtKind::Item(item) => {
                let item = self.resolve_item(item.as_ref())?;
                let position = item.position();
                let ty = item.ty();
                Ok(Box::new(MirStmt::new(
                    MirStmtKind::Item(item),
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
    ) -> Result<Box<MirExpr>, Error> {
        println!("Resolving Expr: {}", expr.kind().name());
        let expr = match expr.kind() {
            ExprKind::Integer(val) => {
                let ty = self.type_map.get_u32();
                Box::new(MirExpr::new(
                    MirExprKind::Integer(*val),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Float(val) => {
                let ty = self.type_map.get_f32();
                Box::new(MirExpr::new(MirExprKind::Float(*val), expr.position(), ty))
            }
            ExprKind::String(val) => {
                let ty = self.type_map.get_string();
                Box::new(MirExpr::new(
                    MirExprKind::String(val.clone()),
                    expr.position(),
                    ty,
                ))
            }
            ExprKind::Char(val) => {
                let ty = self.type_map.get_char();
                Box::new(MirExpr::new(MirExprKind::Char(*val), expr.position(), ty))
            }
            ExprKind::Name(ident) => {
                let name = self.resolve_ident(ident)?;
                let ty = name.borrow().ty();
                Box::new(MirExpr::new(MirExprKind::Name(name), expr.position(), ty))
            }
            ExprKind::Binary(op, left, right) => self.resolve_binary(
                *op,
                left.as_ref(),
                right.as_ref(),
                expected_type,
                expr.position(),
            )?,
            ExprKind::Unary(op, expr) => {
                self.resolve_unary(*op, expr.as_ref(), expected_type, expr.position())?
            }
            _ => unimplemented!(), /*            ExprKind::Field(_, _) => {}
                                               ExprKind::Call { .. } => {}
                                               ExprKind::Method { .. } => {}
                                               ExprKind::Block(_) => {}
                                               ExprKind::Tuple(_) => {}
                                               ExprKind::Loop(_) => {}
                                               ExprKind::While(_, _) => {}
                                               ExprKind::For { .. } => {}
                                               ExprKind::If { .. } => {}
                                               ExprKind::StructExpr { .. } => {}
                                               ExprKind::SelfLit => {}
                                               ExprKind::SelfType => {}
                                   */
        };

        // expr.as_ref().ty()
        Ok(expr)
    }

    fn resolve_item(&mut self, item: &Item) -> Result<Box<MirItem>, Error> {
        match item.kind() {
            ItemKind::Variable {
                vis,
                mutable,
                name,
                init,
                spec,
            } => self.resolve_variable(
                *vis,
                *mutable,
                name.clone(),
                init.as_ref(),
                spec.as_ref(),
                item.position(),
            ),
            ItemKind::Struct { vis, name, fields } => {
                self.resolve_struct(*vis, name, fields.as_slice(), item.position())
            }
            ItemKind::Function {
                vis,
                name,
                params,
                ret,
                body,
            } => self.resolve_function(
                *vis,
                name,
                params.as_slice(),
                ret.as_ref(),
                body,
                item.position(),
            ),
            ItemKind::Param { .. } | ItemKind::Field { .. } | _ => todo!(),
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
        vis: Visibility,
        mutable: bool,
        name: Identifier,
        init: Option<&Box<Expr>>,
        spec: Option<&Box<Spec>>,
        position: Position,
    ) -> Result<Box<MirItem>, Error> {
        self.check_duplicate_item_name(&name)?;

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
            (None, None) => {
                return Err(Error::invalid_variable_item().with_position(name.position()))
            }
        };

        let entity = Entity::new_ref(
            name.kind().value.clone(),
            result_type.clone(),
            EntityInfo::Variable {
                default: init.clone(),
            },
        );

        self.insert_entity(name.kind().value.as_str(), entity);

        let variable = Variable {
            vis,
            mutable,
            name,
            init,
            spec,
        };

        Ok(Box::new(MirItem::new(
            MirItemKind::Variable(variable),
            position,
            result_type,
        )))
    }

    fn resolve_struct(
        &mut self,
        vis: Visibility,
        name: &Identifier,
        fields: &[Box<Item>],
        position: Position,
    ) -> Result<Box<MirItem>, Error> {
        unimplemented!()
    }

    fn resolve_function(
        &mut self,
        vis: Visibility,
        name: &Identifier,
        params: &[Box<Item>],
        return_spec: &Spec,
        body: &FunctionBody,
        position: Position,
    ) -> Result<Box<MirItem>, Error> {
        self.check_duplicate_item_name(name)?;

        let mut mir_items = vec![];
        self.push_scope(ScopeKind::Param(name.kind().value.clone()));
        for param in params {
            if let ItemKind::Param { names, spec, init } = param.kind() {
                let param =
                    self.resolve_param(names, spec.as_ref(), init.as_ref(), param.position())?;
                mir_items.extend(param);
            }
        }

        let function_params: Vec<Rc<Type>> = mir_items.iter().map(|item| item.ty()).collect();

        let (return_type, mir_expr) = match body {
            FunctionBody::Block(expr) => {
                let mir_spec = if return_spec.is_infer() {
                    Box::new(MirSpec::new(
                        MirSpecKind::Infer,
                        position,
                        self.type_map.get_unit(),
                    ))
                } else {
                    self.resolve_spec(spec)?
                };

                let mir_expr = self.resolve_expr(expr.as_ref(), Some(mir_spec.ty()))?;
                (expected_type, mir_expr)
            }
            FunctionBody::Expression(expr) => {
                let mir_spec = if return_spec.is_infer() {
                    None
                } else {
                    Some(self.resolve_spec(spec)?)
                };

                let mir_expr =
                    self.resolve_expr(expr.as_ref(), mir_spec.as_ref().map(|spec| spec.ty()))?;

                let return_type = match mir_spec {
                    Some(ty) => ty,
                    None => mir_expr.ty(),
                };

                (return_type, mir_expr)
            }
        };

        self.pop_scope();

        let function_kind = TypeKind::Function(function_params, return_type);
        let function_type = self.insert_type(function_kind);

        let function = Function {
            vis,
            name: name.clone(),
            params: mir_items,
            ret: return_type,
            body: mir_expr,
        };

        unimplemented!()
    }

    fn resolve_param(
        &mut self,
        names: &[Identifier],
        spec: Option<&Box<Spec>>,
        init: Option<&Box<Expr>>,
        position: Position,
    ) -> Result<Vec<Box<MirParam>>, Error> {
        unreachable!()
    }

    fn resolve_spec(&mut self, spec: &Spec) -> Result<Box<MirSpec>, Error> {
        match spec.kind() {
            SpecKind::Named(expr) => {
                // let expr = self.resolve_expr(expr.as_ref(), None)?;
                let entity = self.resolve_type_expr(expr.as_ref())?;
                if entity.borrow().is_type() {
                    let ty = entity.borrow().ty();
                    Ok(Box::new(MirSpec::new(
                        MirSpecKind::Named,
                        spec.position(),
                        ty,
                    )))
                } else {
                    panic!()
                }
            }
            SpecKind::Tuple(_) | SpecKind::Unit | SpecKind::Infer | SpecKind::SelfType => todo!(),
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
            _ => unreachable!(),
        }
    }

    fn resolve_ident(&self, ident: &Identifier) -> Result<EntityRef, Error> {
        if let Some(entity) = self.deep_lookup(ident.kind().value.as_str()) {
            Ok(entity)
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
    ) -> Result<Box<MirExpr>, Error> {
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

        Ok(Box::new(MirExpr::new(
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
    ) -> Result<Box<MirExpr>, Error> {
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

        Ok(Box::new(MirExpr::new(
            MirExprKind::Unary(UnaryExpr { op, operand }),
            expr.position(),
            result_type,
        )))
    }
}
