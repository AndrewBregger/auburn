use crate::analysis::scope::ScopeRef;
use crate::mir::{MirExpr, MirItem};
use crate::syntax::ast::{Item, Visibility};
use crate::types::Type;
use crate::utils::{new_ptr, Ptr};
use std::rc::Rc;

pub type EntityRef = Ptr<Entity>;

#[derive(Debug, Clone)]
pub enum EntityInfo {
    Unresolved(Box<Item>),
    Resolving,
    Primitive,
    Structure {
        scope: ScopeRef,
        mir: Rc<MirItem>,
    },
    Function {
        params: ScopeRef,
        body: Option<ScopeRef>,
        mir: Rc<MirItem>,
    },
    Variable {
        default: Option<Rc<MirExpr>>,
        mir: Rc<MirItem>,
    },
    Param {
        index: usize,
        default: Option<Rc<MirExpr>>,
    },
    Field {
        index: usize,
        default: Option<Rc<MirExpr>>,
    },
}

#[derive(Debug, Clone)]
pub struct Entity {
    visibility: Visibility,
    name: String,
    ty: Rc<Type>,
    kind: EntityInfo,
}

impl Entity {
    pub fn unresolved(
        visibility: Visibility,
        name: String,
        item: Box<Item>,
        invalid_type: Rc<Type>,
    ) -> Self {
        Self {
            visibility,
            name,
            ty: invalid_type,
            kind: EntityInfo::Unresolved(item),
        }
    }

    pub fn resolving(visibility: Visibility, name: String, invalid_type: Rc<Type>) -> Self {
        Self {
            visibility,
            name,
            ty: invalid_type,
            kind: EntityInfo::Resolving,
        }
    }

    pub fn resolve(&mut self, ty: Rc<Type>, kind: EntityInfo) {
        self.ty = ty;
        self.kind = kind;
    }

    pub fn set_visibility(&mut self, visibility: Visibility) {
        self.visibility = visibility;
    }

    pub fn to_resolving(self) -> Self {
        Self {
            visibility: self.visibility,
            name: self.name,
            ty: self.ty,
            kind: EntityInfo::Resolving,
        }
    }

    pub fn new(visibility: Visibility, name: String, ty: Rc<Type>, kind: EntityInfo) -> Self {
        Self {
            visibility,
            name,
            ty,
            kind,
        }
    }

    pub fn new_ref(
        visibility: Visibility,
        name: String,
        ty: Rc<Type>,
        kind: EntityInfo,
    ) -> EntityRef {
        new_ptr(Self::new(visibility, name, ty, kind))
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn ty(&self) -> Rc<Type> {
        self.ty.clone()
    }

    pub fn kind(&self) -> &EntityInfo {
        &self.kind
    }

    pub fn is_type(&self) -> bool {
        match self.kind {
            EntityInfo::Primitive | EntityInfo::Structure { .. } => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self.kind {
            EntityInfo::Structure { .. } => true,
            _ => false,
        }
    }

    pub fn is_resolved(&self) -> bool {
        match self.kind {
            EntityInfo::Unresolved(_) | EntityInfo::Resolving => false,
            _ => true,
        }
    }

    pub fn is_resolving(&self) -> bool {
        match self.kind {
            EntityInfo::Resolving => true,
            _ => false,
        }
    }

    pub fn is_unresolved(&self) -> bool {
        match self.kind {
            EntityInfo::Unresolved(_) => true,
            _ => false,
        }
    }
}
