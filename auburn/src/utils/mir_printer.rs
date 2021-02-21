use std::ops::Deref;

use crate::ir::ast::NodeType;
use crate::ir::hir::{
    HirExpr, HirExprKind, HirField, HirFile, HirItem, HirItemKind, HirNodeBase, HirParam, HirSpec,
    HirStmt, HirStmtKind, IfExprBranch, MirNode,
};
use crate::utils::EntityPrinter;

pub struct MirPrinter;

impl MirPrinter {
    fn indent(indent: usize) -> String {
        (0..indent).map(|_| '\t').collect()
    }

    pub fn print_file(file: &HirFile) {
        println!("Stmts:");
        for stmt in file.stmts() {
            Self::print_stmt_inner(stmt.as_ref(), 1);
        }
    }

    pub fn print_stmt(stmt: &HirStmt) {
        Self::print_stmt_inner(stmt, 0);
    }

    fn print_header<N: NodeType>(base: &HirNodeBase<N>, indent: usize) {
        println!(
            "{}{} - {}",
            Self::indent(indent),
            base.inner().name(),
            base.ty()
        );
    }

    pub(crate) fn print_stmt_inner(stmt: &HirStmt, indent: usize) {
        Self::print_header(stmt, indent);
        match stmt.inner() {
            HirStmtKind::Expr(expr) => Self::print_expr_inner(expr.as_ref(), indent + 1),
            HirStmtKind::Item(item) => {
                EntityPrinter::print_impl(&item.deref().borrow(), indent + 1);
            }
            HirStmtKind::Assignment(assignment) => {
                EntityPrinter::print_impl(&assignment.lvalue.borrow(), indent + 1);
                Self::print_expr_inner(assignment.rhs.as_ref(), indent + 1);
            }
            HirStmtKind::Echo(expr) => {
                Self::print_expr_inner(expr.as_ref(), indent + 1);
            }
        }
    }

    pub fn print_expr(expr: &HirExpr) {
        Self::print_expr_inner(expr, 0);
    }

    pub(crate) fn print_expr_inner(expr: &HirExpr, indent: usize) {
        Self::print_header(expr, indent);
        match expr.inner().kind() {
            HirExprKind::Name(_) => {}
            HirExprKind::Binary(binary_expr) => {
                println!("{}Op: {}", Self::indent(indent + 1), binary_expr.op);
                Self::print_expr_inner(binary_expr.left.as_ref(), indent + 1);
                Self::print_expr_inner(binary_expr.right.as_ref(), indent + 1);
            }
            HirExprKind::Unary(unary_expr) => {
                println!("{}Op: {}", Self::indent(indent + 1), unary_expr.op);
                Self::print_expr_inner(unary_expr.operand.as_ref(), indent + 1);
            }
            HirExprKind::Field(field_expr) => {
                Self::print_expr_inner(field_expr.operand.as_ref(), indent + 1);
                EntityPrinter::print_impl(&field_expr.field.borrow(), indent + 1);
            }
            HirExprKind::FieldAccess(..) => {}
            HirExprKind::Call(call_expr) => {
                Self::print_expr_inner(call_expr.operand.as_ref(), indent + 1);
                for actual in &call_expr.actuals {
                    Self::print_expr_inner(actual.as_ref(), indent + 1);
                }
            }
            HirExprKind::Block(block_expr) => {
                block_expr
                    .stmts
                    .iter()
                    .for_each(|stmt| Self::print_stmt_inner(stmt, indent + 1));
            }
            HirExprKind::Method(method_expr) => {
                println!(
                    "{}Type: {}",
                    Self::indent(indent + 1),
                    method_expr.function_type
                );
                for actual in &method_expr.actuals {
                    Self::print_expr_inner(actual.as_ref(), indent + 1);
                }
            }
            HirExprKind::AssociatedFunction(associated_function_expr) => {
                println!(
                    "{}Type: {}",
                    Self::indent(indent + 1),
                    associated_function_expr.function_type
                );
                for actual in &associated_function_expr.actuals {
                    Self::print_expr_inner(actual.as_ref(), indent + 1);
                }
            }
            HirExprKind::Tuple(tuple_expr) => {
                println!("{}Elements:", Self::indent(indent));
                for element in tuple_expr.elements.iter() {
                    Self::print_expr_inner(element.as_ref(), indent + 1);
                }
            }
            HirExprKind::TupleIndex(tuple_index) => {
                println!("{}Operand:", Self::indent(indent));
                Self::print_expr_inner(tuple_index.tuple.as_ref(), indent + 1);
                println!("{}Field: {}", Self::indent(indent), tuple_index.field);
            }
            HirExprKind::Loop(loop_expr) => {
                println!("{}Body:", Self::indent(indent));
                Self::print_expr_inner(loop_expr.body.as_ref(), indent + 1);
            }
            HirExprKind::While(while_expr) => {
                println!("{}Cond:", Self::indent(indent));
                Self::print_expr_inner(while_expr.cond.as_ref(), indent + 1);
                println!("{}Body:", Self::indent(indent));
                Self::print_expr_inner(while_expr.body.as_ref(), indent + 1);
            }
            HirExprKind::For(_for_expr) => {}
            HirExprKind::If(if_expr) => {
                for branch in if_expr.branches.as_slice() {
                    match branch {
                        IfExprBranch::Conditional { cond, body, first } => {
                            println!("{}Cond:", Self::indent(indent));
                            Self::print_expr_inner(cond, indent + 1);
                            println!("{}Body:", Self::indent(indent));
                            Self::print_expr_inner(body, indent + 1);
                            println!("{}First: {}", Self::indent(indent), first);
                        }
                        IfExprBranch::Unconditional { body } => {
                            println!("{}Body:", Self::indent(indent));
                            Self::print_expr_inner(body, indent + 1);
                        }
                    }
                }
            }
            HirExprKind::StructExpr(struct_expr) => {
                struct_expr.fields.iter().for_each(|(index, mir)| {
                    println!("{}Index: {}", Self::indent(indent + 1), index);
                    Self::print_expr_inner(mir.as_ref(), indent + 1);
                })
            }
            HirExprKind::Return(expr) => {
                Self::print_expr_inner(expr, indent + 1);
            }
            HirExprKind::Index(index_expr) => {
                println!("{}Operand:", Self::indent(indent));
                Self::print_expr_inner(index_expr.operand.as_ref(), indent + 1);
                println!("{}Index:", Self::indent(indent));
                Self::print_expr_inner(index_expr.index.as_ref(), indent + 1);
            }
            HirExprKind::SelfLit => {}
            HirExprKind::Integer(_)
            | HirExprKind::Float(_)
            | HirExprKind::String(_)
            | HirExprKind::Char(_)
            | HirExprKind::Bool(_)
            | HirExprKind::Continue
            | HirExprKind::Break => {}
        }
    }

    pub fn print_spec(stmt: &HirSpec) {
        Self::print_spec_inner(stmt, 0);
    }

    pub(crate) fn print_spec_inner(spec: &HirSpec, indent: usize) {
        Self::print_header(spec, indent);
        // match spec.inner() {
        //     MirSpecKind::Named => {}
        //     MirSpecKind::Tuple => {}
        //     MirSpecKind::Unit => {}
        //     MirSpecKind::SelfType => {}
        // }
    }

    pub fn print_item(stmt: &HirItem) {
        Self::print_item_inner(stmt, 0);
    }

    fn print_item_inner(item: &HirItem, indent: usize) {
        Self::print_header(item, indent);
        match item.inner() {
            HirItemKind::Variable(variable) => {
                variable
                    .spec
                    .as_ref()
                    .map(|spec| Self::print_spec_inner(spec.as_ref(), indent + 1));

                variable
                    .init
                    .as_ref()
                    .map(|init| Self::print_expr_inner(init.as_ref(), indent + 1));
            }
            HirItemKind::Struct(structure) => {
                structure
                    .fields
                    .iter()
                    .for_each(|field| Self::print_field_inner(field.as_ref(), indent + 1));

                structure
                    .methods
                    .iter()
                    .for_each(|method| Self::print_item_inner(method.as_ref(), indent + 1));
            }
            HirItemKind::Function(function) => {
                function
                    .params
                    .iter()
                    .for_each(|param| Self::print_param_inner(param.as_ref(), indent + 1));

                Self::print_spec_inner(function.ret.as_ref(), indent + 1);
                Self::print_expr_inner(function.body.as_ref(), indent + 1);
            }
        }
    }

    fn print_param_inner(param: &HirParam, indent: usize) {
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

    fn print_field_inner(field: &HirField, indent: usize) {
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
