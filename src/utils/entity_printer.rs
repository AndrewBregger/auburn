use crate::analysis::{Entity, EntityInfo};
use crate::utils::MirPrinter;
use std::borrow::Borrow;
use std::ops::Deref;

pub struct EntityPrinter;

impl EntityPrinter {
    fn indent(indent: usize) -> String {
        (0..indent).map(|_| '\t').collect()
    }

    pub fn print(entity: &Entity) {
        Self::print_impl(entity, 0);
    }

    pub fn print_impl(entity: &Entity, indent: usize) {
        let name = entity.name();
        let ty = entity.ty();
        println!("{}Entity({}, {})", Self::indent(indent), name, ty);

        match entity.kind() {
            EntityInfo::Unresolved(..) => println!("{}Unresolved", Self::indent(indent + 1)),
            EntityInfo::Resolving => println!("{}Resolving", Self::indent(indent + 1)),
            EntityInfo::Structure { scope, .. } => {
                for member in scope.elements().values() {
                    Self::print_impl(&member.deref().borrow(), indent + 1);
                }
            }
            EntityInfo::Function { params, body, .. } => {
                println!("{}Params:", Self::indent(indent));
                for member in params.elements().values() {
                    Self::print_impl(&member.deref().borrow(), indent + 1);
                }
                if let Some(body) = body {
                    for member in body.elements().values() {
                        Self::print_impl(&member.deref().borrow(), indent + 1);
                    }
                }
            }
            EntityInfo::Variable { default, .. } => {
                if let Some(default) = default {
                    MirPrinter::print_expr_inner(&default.deref().borrow(), indent + 1);
                }
            }

            EntityInfo::Param { default, index, .. } | EntityInfo::Field { default, index, .. } => {
                println!("{}Index: {}", Self::indent(indent + 1), index);
                if let Some(default) = default {
                    MirPrinter::print_expr_inner(&default.deref().borrow(), indent + 1);
                }
            }
            EntityInfo::Primitive => {}
        }
    }
}
