use crate::analysis::scope::{Scope, ScopeKind};
use crate::analysis::typer::Typer;
use crate::analysis::typer::{
    ASSOCIATIVE_FUNCTION, EXPR_RESULT_USED, FUNCTION, FUNCTION_BODY, FUNCTION_PARAM,
    SELF_PARAM_IDENT, STRUCT,
};
use crate::analysis::{Entity, EntityInfo, EntityRef};
use crate::error::Error;
use crate::ir::ast::{Expr, FunctionBody, Identifier, Item, ItemKind, Node, Spec, Visibility};
use crate::ir::hir::{HirExprPtr, HirSpec, HirSpecKind, HirSpecPtr, MirNode};
use crate::syntax::Position;
use crate::types::{Type, TypeKind};
use crate::utils::{new_ptr, Ptr};
use crate::{
    analysis::entity::{
        AssociatedFunctionInfo, FunctionInfo, LocalInfo, Path, StructureInfo, VariableInfo,
    },
    ir::ast::ExprKind,
};
use std::cell::RefCell;
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
    pub(crate) fn resolve_top_level_item(&mut self, item: &Item) -> Result<EntityRef, Error> {
        if let Some(name) = item.get_name() {
            if let Some(entity) = self.shallow_lookup(name.kind().value.as_str()) {
                entity.deref().borrow_mut().to_resolving();
                self.resolve_item_impl(item, entity, true, true)
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

    pub(crate) fn resolve_item(&mut self, item: &Item) -> Result<EntityRef, Error> {
        if let Some(name) = item.get_name() {
            let vis = item.get_visibility();
            self.check_duplicate_item_name(name)?;
            let entity = Ptr::new(RefCell::new(Entity::resolving(
                vis,
                name.kind().value.clone(),
                self.type_map.get_invalid(),
            )));
            self.resolve_item_impl(item, entity, false, false)
        } else {
            panic!("Compiler Error: attempting to resolving a field, param, or self as an item. These should be resolved locally");
        }
    }

    pub(crate) fn resolve_item_impl(
        &mut self,
        item: &Item,
        entity: EntityRef,
        declared: bool,
        is_file_scope: bool,
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
                is_file_scope,
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
            ItemKind::Param { .. } | ItemKind::SelfParam { .. } | ItemKind::Field { .. } => {
                panic!()
            }
        }
    }

    pub(crate) fn check_duplicate_item_name(&self, name: &Identifier) -> Result<(), Error> {
        if let Some(_) = self.shallow_lookup(name.kind().value.as_str()) {
            let err =
                Error::duplicate_name(name.kind().value.clone()).with_position(name.position());
            Err(err)
        } else {
            Ok(())
        }
    }

    pub(crate) fn resolve_variable(
        &mut self,
        entity: EntityRef,
        _vis: Visibility,
        mutable: bool,
        name: Identifier,
        init: Option<&Box<Expr>>,
        spec: Option<&Box<Spec>>,
        position: Position,
        declared: bool,
        is_file_scope: bool,
    ) -> Result<EntityRef, Error> {
        let (spec, init, result_type) = self.resolve_local(spec, init, position)?;

        let variable_info = VariableInfo {
            spec,
            mutable,
            global: is_file_scope,
            default: init.clone(),
        };

        entity.borrow_mut().resolve(
            result_type,
            EntityInfo::Variable(variable_info),
            Path::empty(),
        );

        if !declared {
            self.insert_entity(name.kind().value.as_str(), entity.clone());
        }

        Ok(entity)
    }

    pub(crate) fn resolve_struct(
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

    pub(crate) fn resolve_local(
        &mut self,
        spec: Option<&Box<Spec>>,
        init: Option<&Box<Expr>>,
        position: Position,
    ) -> Result<(Option<HirSpecPtr>, Option<HirExprPtr>, Rc<Type>), Error> {
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

    pub(crate) fn resolve_field(
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

    pub(crate) fn split_struct_members(
        members: &[Box<Item>],
    ) -> (Vec<&Box<Item>>, Vec<&Box<Item>>) {
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

    pub(crate) fn resolve_function(
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
                            Rc::new(HirSpec::new(
                                HirSpecKind::Infer,
                                position,
                                self.type_map.get_unit(),
                            ))
                        } else {
                            self.resolve_spec(return_spec)?
                        };

                        let mir_expr = if let ExprKind::Block(stmts) = expr.kind() {
                            self.resolve_block_expression(stmts, true, expr.position())?
                        } else {
                            unreachable!()
                        };

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
                                Rc::new(HirSpec::new(HirSpecKind::Infer, position, mir_expr.ty()))
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

    pub(crate) fn resolve_param(
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
}
