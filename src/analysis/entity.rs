use crate::analysis::scope::ScopeRef;
use crate::mir::{MirExprPtr, MirSpecPtr};
use crate::syntax::ast::{Item, Visibility};
use crate::types::Type;
use crate::utils::{new_ptr, Ptr};
use std::rc::Rc;

pub type EntityRef = Ptr<Entity>;

#[derive(Debug, Clone)]
pub struct StructureInfo {
    pub field: ScopeRef,
}

pub struct MethodInfo {
    pub parent: EntityRef,
    pub method: ScopeRef,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub receiver: Option<EntityRef>,
    pub params: ScopeRef,
    pub body_scope: Option<ScopeRef>,
    pub body: MirExprPtr,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub spec: Option<MirSpecPtr>,
    pub default: Option<MirExprPtr>,
}

#[derive(Debug, Clone)]
pub struct LocalInfo {
    pub index: usize,
    pub spec: Option<MirSpecPtr>,
    pub default: Option<MirExprPtr>,
}

#[derive(Debug, Clone)]
pub enum EntityInfo {
    Unresolved(Box<Item>),
    Resolving,
    Primitive,
    Structure(StructureInfo),
    Function(FunctionInfo),
    Variable(VariableInfo),
    Param(LocalInfo),
    Field(LocalInfo),
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

    pub fn to_resolving(&mut self) {
        self.kind = EntityInfo::Resolving;
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

    pub fn type_name(&self) -> &'static str {
        match self.kind {
            EntityInfo::Unresolved(_) => "unresolved",
            EntityInfo::Resolving => "resolving",
            EntityInfo::Primitive => "primative",
            EntityInfo::Structure { .. } => "structure",
            EntityInfo::Function { .. } => "function",
            EntityInfo::Variable { .. } => "variable",
            EntityInfo::Param { .. } => "param",
            EntityInfo::Field { .. } => "field",
        }
    }
}
