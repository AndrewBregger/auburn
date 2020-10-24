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
        println!(
            "{}Entity({:?}, {}, {}, {}, path: {})",
            Self::indent(indent),
            entity as *const _,
            entity.type_name(),
            name,
            ty,
            entity.path()
        );

        match entity.kind() {
            EntityInfo::Unresolved(..) => println!("{}Unresolved", Self::indent(indent + 1)),
            EntityInfo::Resolving => println!("{}Resolving", Self::indent(indent + 1)),
            EntityInfo::Structure(structure) => {
                for member in structure.fields.elements().values() {
                    Self::print_impl(&member.deref().borrow(), indent + 1);
                }

                for member in structure.methods.elements().values() {
                    Self::print_impl(&member.deref().borrow(), indent + 1);
                }
            }
            EntityInfo::Function(function) => {
                println!("{}Params:", Self::indent(indent));
                for member in function.params.elements().values() {
                    Self::print_impl(&member.deref().borrow(), indent + 1);
                }
                if let Some(body) = function.body_scope.as_ref() {
                    for member in body.elements().values() {
                        Self::print_impl(&member.deref().borrow(), indent + 1);
                    }
                }
            }
            EntityInfo::Variable(variable) => {
                if let Some(default) = variable.default.as_ref() {
                    MirPrinter::print_expr_inner(&default.deref().borrow(), indent + 1);
                }
            }

            EntityInfo::Param(local_info) | EntityInfo::Field(local_info) => {
                println!("{}Index: {}", Self::indent(indent + 1), local_info.index);
                if let Some(default) = local_info.default.as_ref() {
                    MirPrinter::print_expr_inner(&default.deref().borrow(), indent + 1);
                }
            }
            EntityInfo::SelfParam { mutable } => {
                println!("{}Self, Mutable {}", Self::indent(indent + 1), mutable)
            }
            EntityInfo::AssociatedFunction(associated_function) => {
                println!("{}Params:", Self::indent(indent));
                for member in associated_function.params.elements().values() {
                    Self::print_impl(&member.deref().borrow(), indent + 1);
                }
                if let Some(body) = associated_function.body_scope.as_ref() {
                    for member in body.elements().values() {
                        Self::print_impl(&member.deref().borrow(), indent + 1);
                    }
                }
            }
            EntityInfo::Primitive => {}
        }
    }
}
