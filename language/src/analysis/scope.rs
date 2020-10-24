use crate::analysis::EntityRef;
use crate::system::FileId;
use crate::utils::Ptr;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

pub type ScopeRef = Rc<Scope>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ScopeKind {
    Invalid,
    Prelude,
    File {
        file_id: FileId,
        file_name: String,
    },
    Param(String),
    Block,
    Struct(String),
    StructMethods,
}

#[derive(Debug, Clone)]
pub struct PossibleEntity {
    entity: Option<EntityRef>,
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

    pub fn elements(&self) -> &HashMap<String, EntityRef> {
        self.elements.borrow()
    }

    pub fn parent(&self) -> Option<ScopeRef> {
        self.parent.clone()
    }

    pub fn children(&self) -> &[ScopeRef] {
        self.children.as_slice()
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
