use crate::analysis::EntityRef;
use crate::system::FileId;
use crate::utils::{new_ptr, Ptr};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type ScopeRef = Rc<Scope>;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ScopeKind {
    Prelude,
    File(FileId),
    Param(String),
}

#[derive(Debug, Clone)]
pub struct Scope {
    kind: ScopeKind,
    elements: HashMap<String, EntityRef>,
    parent: Option<ScopeRef>,
    children: Vec<ScopeRef>,
}

impl Scope {
    pub fn new(kind: ScopeKind, parent: Option<ScopeRef>) -> Self {
        Self {
            kind,
            elements: HashMap::new(),
            parent,
            children: vec![],
        }
    }

    pub fn parent(&self) -> Option<ScopeRef> {
        self.parent.clone()
    }

    pub fn kind(&self) -> &ScopeKind {
        &self.kind
    }

    pub fn add_child(&mut self, child: ScopeRef) {
        self.children.push(child)
    }

    pub fn new_ref(kind: ScopeKind, parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(Scope::new(kind, parent))
    }

    pub fn shallow_lookup(&self, name: &str) -> Option<EntityRef> {
        self.elements.get(name).map(Ptr::clone)
    }

    pub fn add_element(&mut self, name: &str, entity: EntityRef) {
        self.elements.insert(name.to_string(), entity);
    }
}
