use crate::analysis::entity::EntityRef;
use crate::analysis::scope::{Scope, ScopeKind, ScopeRef};
use crate::error::Error;
use crate::mir::{
    BinaryExpr, MirExpr, MirExprKind, MirFile, MirItem, MirNode, MirSpec, MirStmt, MirStmtKind,
    UnaryExpr,
};
use crate::syntax::ast::{
    BinaryOp, Expr, ExprKind, Identifier, Item, ItemKind, Node, NodeType, Spec, SpecKind, Stmt,
    StmtKind, UnaryOp,
};
use crate::syntax::{ParsedFile, Position};
use crate::types::{Type, TypeMap};
use std::rc::Rc;

pub(crate) struct Typer<'a> {
    type_map: &'a mut TypeMap,
    item_stack: &'a mut Vec<Box<Item>>,
    scope_stack: &'a mut Vec<ScopeRef>,
    entities: Vec<EntityRef>,
    prelude: ScopeRef,
    current: ScopeRef,
}

impl<'a> Typer<'a> {
    pub fn new(
        type_map: &'a mut TypeMap,
        item_stack: &'a mut Vec<Box<Item>>,
        scope_stack: &'a mut Vec<ScopeRef>,
    ) -> Self {
        let prelude = Scope::new_ref(ScopeKind::Prelude, None);
        let current = prelude.clone();
        Self {
            type_map,
            item_stack,
            scope_stack,
            entities: vec![],
            prelude,
            current,
        }
    }

    fn push_scope(&mut self, kind: ScopeKind) -> ScopeRef {
        let new_scope = Scope::new_ref(kind, Some(self.current.clone()));
        self.current = new_scope;
        self.current.clone()
    }

    pub fn pop_scope(&mut self) {
        let parent = self.current.borrow().parent();
        if let Some(parent) = parent {
            self.current = parent;
        } else {
            panic!("Compiler Error: Popping the prelude scope");
        }
    }

    pub fn deep_lookup(&self, name: &str) -> Option<EntityRef> {
        let mut current = self.current.clone();
        loop {
            let borrow = current.borrow();
            if let Some(entity) = borrow.shallow_lookup(name) {
                return Some(entity);
            } else {
                if let Some(parent) = borrow.parent() {
                    std::mem::forget(borrow);
                    current = parent;
                } else {
                    break;
                }
            }
        }
        None
    }

    pub fn shallow_lookup(&self, name: &str) -> Option<EntityRef> {
        self.current.borrow().shallow_lookup(name)
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

        assert_eq!(*self.current.borrow().kind(), ScopeKind::Prelude);

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
            ItemKind::Variable { .. } => {}
            ItemKind::Struct { .. } => {}
            ItemKind::Function { .. } => {}
            ItemKind::Param { .. } => {}
            ItemKind::Field { .. } => {}
        }
        unimplemented!()
    }

    fn resolve_spec(&mut self, spec: &Spec) -> Result<Box<MirSpec>, Error> {
        match spec.kind() {
            SpecKind::Named(_) => {}
            SpecKind::Tuple(_) => {}
            SpecKind::Unit => {}
            SpecKind::Infer => {}
            SpecKind::SelfType => {}
        }
        unimplemented!()
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
            UnaryOp::Minus | UnaryOp::Ampersand => todo!("what is an & supposed to do."),
        };

        Ok(Box::new(MirExpr::new(
            MirExprKind::Unary(UnaryExpr { op, operand }),
            expr.position(),
            result_type,
        )))
    }
}
