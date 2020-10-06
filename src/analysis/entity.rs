use crate::analysis::scope::ScopeRef;
use crate::mir::MirExpr;
use crate::types::Type;
use crate::utils::{new_ptr, Ptr};
use std::cell::RefCell;
use std::rc::Rc;

pub type EntityRef = Ptr<Entity>;

#[derive(Debug, Clone)]
pub enum EntityInfo {
    Primitive,
    Structure {
        scope: ScopeRef,
    },
    Function {
        params: ScopeRef,
        body: Option<ScopeRef>,
    },
    Variable {
        default: Option<Box<MirExpr>>,
    },
    Param {
        default: Option<Box<MirExpr>>,
    },
    Field {
        default: Option<Box<MirExpr>>,
    },
}

#[derive(Debug, Clone)]
pub struct Entity {
    name: String,
    ty: Rc<Type>,
    kind: EntityInfo,
}

impl Entity {
    pub fn new(name: String, ty: Rc<Type>, kind: EntityInfo) -> Self {
        Self { name, ty, kind }
    }

    pub fn new_ref(name: String, ty: Rc<Type>, kind: EntityInfo) -> EntityRef {
        new_ptr(Self::new(name, ty, kind))
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
}
