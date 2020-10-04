use crate::analysis::scope::{Ptr, ScopeRef};
use crate::mir::MirExpr;
use crate::types::Type;
use std::rc::Rc;

pub type EntityRef = Ptr<Entity>;

#[derive(Debug, Clone)]
pub enum EntityInfo {
    Structure {
        scope: ScopeRef,
    },
    Function {
        params: ScopeRef,
        body: Option<ScopeRef>,
    },
    Variable {
        default: Option<MirExpr>,
    },
    Param {
        default: Option<MirExpr>,
    },
    Field {
        default: Option<MirExpr>,
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

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn ty(&self) -> Rc<Type> {
        self.ty.clone()
    }

    pub fn kind(&self) -> &EntityInfo {
        &self.kind
    }
}
