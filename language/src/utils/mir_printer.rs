use crate::mir::{
    MirExpr, MirExprKind, MirField, MirItem, MirItemKind, MirNode, MirNodeBase, MirParam, MirSpec,
    MirStmt, MirStmtKind,
};
use crate::syntax::ast::NodeType;
use crate::utils::EntityPrinter;
use std::ops::Deref;

pub struct MirPrinter;

impl MirPrinter {
    fn indent(indent: usize) -> String {
        (0..indent).map(|_| '\t').collect()
    }

    pub fn print_stmt(stmt: &MirStmt) {
        Self::print_stmt_inner(stmt, 0);
    }

    fn print_header<N: NodeType>(base: &MirNodeBase<N>, indent: usize) {
        println!(
            "{}{} - {}",
            Self::indent(indent),
            base.inner().name(),
            base.ty()
        );
    }

    fn print_stmt_inner(stmt: &MirStmt, indent: usize) {
        Self::print_header(stmt, indent);
        match stmt.inner() {
            MirStmtKind::Expr(expr) => Self::print_expr_inner(expr.as_ref(), indent + 1),
            MirStmtKind::Item(item) => {
                EntityPrinter::print_impl(&item.deref().borrow(), indent + 1);
            }
        }
    }

    pub fn print_expr(expr: &MirExpr) {
        Self::print_expr_inner(expr, 0);
    }

    pub(crate) fn print_expr_inner(expr: &MirExpr, indent: usize) {
        Self::print_header(expr, indent);
        match expr.inner().kind() {
            MirExprKind::Name(_) => {}
            MirExprKind::Binary(binary_expr) => {
                Self::print_expr_inner(binary_expr.left.as_ref(), indent + 1);
                Self::print_expr_inner(binary_expr.right.as_ref(), indent + 1);
            }
            MirExprKind::Unary(unary_expr) => {
                Self::print_expr_inner(unary_expr.operand.as_ref(), indent + 1);
            }
            MirExprKind::Field(field_expr) => {
                Self::print_expr_inner(field_expr.operand.as_ref(), indent + 1);
                EntityPrinter::print_impl(&field_expr.field.borrow(), indent + 1);
            }
            MirExprKind::FieldAccess(..) => {}
            MirExprKind::Call(call_expr) => {
                Self::print_expr_inner(call_expr.operand.as_ref(), indent + 1);
                for actual in &call_expr.actuals {
                    Self::print_expr_inner(actual.as_ref(), indent + 1);
                }
            }
            MirExprKind::Block(block_expr) => {
                block_expr
                    .stmts
                    .iter()
                    .for_each(|stmt| Self::print_stmt_inner(stmt, indent + 1));
            }
            MirExprKind::Method(_method_expr) => {}
            MirExprKind::Tuple(_tuple_expr) => {}
            MirExprKind::Loop(_loop_expr) => {}
            MirExprKind::While(_while_expr) => {}
            MirExprKind::For(_for_expr) => {}
            MirExprKind::If(_if_expr) => {}
            MirExprKind::StructExpr(struct_expr) => {
                struct_expr.fields.iter().for_each(|(index, mir)| {
                    println!("{}Index: {}", Self::indent(indent + 1), index);
                    Self::print_expr_inner(mir.as_ref(), indent + 1);
                })
            }
            MirExprKind::SelfLit => {}
            MirExprKind::Integer(_)
            | MirExprKind::Float(_)
            | MirExprKind::String(_)
            | MirExprKind::Char(_) => {}
        }
    }

    pub fn print_spec(stmt: &MirSpec) {
        Self::print_spec_inner(stmt, 0);
    }

    fn print_spec_inner(spec: &MirSpec, indent: usize) {
        Self::print_header(spec, indent);
        // match spec.inner() {
        //     MirSpecKind::Named => {}
        //     MirSpecKind::Tuple => {}
        //     MirSpecKind::Unit => {}
        //     MirSpecKind::SelfType => {}
        // }
    }

    pub fn print_item(stmt: &MirItem) {
        Self::print_item_inner(stmt, 0);
    }

    fn print_item_inner(item: &MirItem, indent: usize) {
        Self::print_header(item, indent);
        match item.inner() {
            MirItemKind::Variable(variable) => {
                variable
                    .spec
                    .as_ref()
                    .map(|spec| Self::print_spec_inner(spec.as_ref(), indent + 1));

                variable
                    .init
                    .as_ref()
                    .map(|init| Self::print_expr_inner(init.as_ref(), indent + 1));
            }
            MirItemKind::Struct(structure) => {
                structure
                    .fields
                    .iter()
                    .for_each(|field| Self::print_field_inner(field.as_ref(), indent + 1));

                structure
                    .methods
                    .iter()
                    .for_each(|method| Self::print_item_inner(method.as_ref(), indent + 1));
            }
            MirItemKind::Function(function) => {
                function
                    .params
                    .iter()
                    .for_each(|param| Self::print_param_inner(param.as_ref(), indent + 1));

                Self::print_spec_inner(function.ret.as_ref(), indent + 1);
                Self::print_expr_inner(function.body.as_ref(), indent + 1);
            }
        }
    }

    fn print_param_inner(param: &MirParam, indent: usize) {
        let param_inner = param.inner();

        param_inner
            .spec
            .as_ref()
            .map(|spec| Self::print_spec_inner(spec.as_ref(), indent + 1));

        param_inner
            .init
            .as_ref()
            .map(|init| Self::print_expr_inner(init.as_ref(), indent + 1));
    }

    fn print_field_inner(field: &MirField, indent: usize) {
        let field_inner = field.inner();
        field_inner
            .spec
            .as_ref()
            .map(|spec| Self::print_spec_inner(spec.as_ref(), indent + 1));

        field_inner
            .init
            .as_ref()
            .map(|init| Self::print_expr_inner(init.as_ref(), indent + 1));
    }
}
