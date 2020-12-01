use crate::analysis::entity::Path;
use crate::analysis::scope::{Scope, ScopeKind, ScopeRef};
use crate::analysis::{Entity, EntityInfo, EntityRef};
use crate::error::Error;
use crate::ir::ast::{Identifier, ItemKind, Node, StmtKind};
use crate::ir::hir::{HirFile, HirStmtKind};
use crate::syntax::ParsedFile;
use crate::types::{Type, TypeKind, TypeMap};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

mod expressions;
mod items;
mod statements;
mod type_specs;

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

pub(super) struct Typer<'a> {
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
}

impl<'src> Typer<'src> {
    pub fn resolve_file(mut self, parsed_file: ParsedFile) -> Result<HirFile, Error> {
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
                HirStmtKind::Expr(..) | HirStmtKind::Assignment(..) => globals.push(stmt.clone()),
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

        Ok(HirFile::new(parsed_file.file_id, globals, entities))
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
}
