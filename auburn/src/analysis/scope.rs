use std::collections::HashMap;
use std::rc::Rc;

use crate::analysis::EntityRef;
use crate::system::FileId;

pub type ScopeRef = Rc<Scope>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ScopeKind {
    Invalid,
    Prelude,
    File { file_id: FileId, file_name: String },
    Param(String),
    Block,
    Struct(String),
    StructMethods(String),
}

#[derive(Debug, Clone)]
pub struct PossibleEntity {
    entity: Option<EntityRef>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    kind: ScopeKind,
    names: HashMap<String, usize>,
    elements: Vec<EntityRef>,
    parent: Option<ScopeRef>,
    children: Vec<ScopeRef>,
}

impl Scope {
    pub fn new(kind: ScopeKind, parent: Option<ScopeRef>) -> Self {
        Self {
            kind,
            names: HashMap::new(),
            elements: vec![],
            parent,
            children: vec![],
        }
    }

    pub fn elements(&self) -> &[EntityRef] {
        self.elements.as_slice()
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn get(&self, name: &str) -> Option<&EntityRef> {
        let idx = self.names.get(name)?;
        Some(&self.elements[*idx])
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
        self.get(name).map(Rc::clone)
    }

    pub fn add_element(&mut self, name: &str, entity: EntityRef) {
        let idx = self.len();
        self.names.insert(name.to_string(), idx);
        self.elements.push(entity);
    }
}
