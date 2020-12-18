use crate::analysis::typer::*;
use crate::error::Error;
use crate::ir::ast::{Node, Spec, SpecKind};
use crate::ir::hir::{HirSpec, HirSpecKind, MirNode};
use crate::types::TypeKind;
use std::ops::Deref;
use std::rc::Rc;

#[allow(unused)]
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
    pub(crate) fn resolve_spec(&mut self, spec: &Spec) -> Result<Rc<HirSpec>, Error> {
        match spec.kind() {
            SpecKind::Named(expr) => {
                let entity = self.resolve_type_expr(expr.as_ref())?;
                if entity.deref().borrow().is_type() {
                    let ty = entity.deref().borrow().ty();
                    Ok(Rc::new(HirSpec::new(
                        HirSpecKind::Named,
                        spec.position(),
                        ty,
                    )))
                } else {
                    panic!()
                }
            }
            SpecKind::SelfType => todo!(),
            SpecKind::Tuple(_) | SpecKind::Unit | SpecKind::Infer => todo!("{:?}", spec),
            SpecKind::Array(element_type, size) => {
                let mir_spec = self.resolve_spec(element_type.as_ref())?;
                match size.as_ref() {
                    Some(size) => {
                        let mir_size = self.resolve_expr(size.as_ref(), None)?;
                        let mir_size_type = mir_size.ty();
                        // reduce the expression if possible
                        // let mir_size = MirExpr::reduce(mir_size)?;
                        // the resulting expression must be a constant value.
                        // @TODO: Add support for constant expressions
                        if mir_size.is_literal() {
                            if mir_size_type.is_integer() {
                                let size = mir_size.as_integer() as usize;
                                let ty = self.insert_type(TypeKind::Array {
                                    element_type: mir_spec.ty(),
                                    size,
                                });

                                Ok(Rc::new(HirSpec::new(
                                    HirSpecKind::Array,
                                    spec.position(),
                                    ty,
                                )))
                            } else {
                                let err = Error::invalid_array_size_type(mir_size_type.as_ref());
                                Err(err)
                            }
                        } else {
                            let err = Error::invalid_array_size_type(mir_size_type.as_ref());
                            Err(err)
                        }
                    }
                    None => {
                        let ty = self.insert_type(TypeKind::Slice {
                            element_type: mir_spec.ty(),
                        });

                        Ok(Rc::new(HirSpec::new(
                            HirSpecKind::Slice,
                            spec.position(),
                            ty,
                        )))
                    }
                }
            }
            SpecKind::Mutable { inner } => {
                let mir_inner = self.resolve_spec(inner.as_ref())?;
                let ty = self.insert_type(TypeKind::Mutable {
                    inner: mir_inner.ty(),
                });

                Ok(Rc::new(HirSpec::new(
                    HirSpecKind::Mutable,
                    spec.position(),
                    ty,
                )))
            }
        }
    }
}
