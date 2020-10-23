use crate::analysis::scope::ScopeRef;
use crate::mir::{MirExprPtr, MirSpecPtr};
use crate::syntax::ast::{Item, Visibility};
use crate::types::Type;
use crate::utils::{new_ptr, Ptr};
use std::rc::Rc;

pub type EntityRef = Ptr<Entity>;

#[derive(Debug, Clone)]
pub struct StructureInfo {
    pub fields: ScopeRef,
    pub methods: ScopeRef,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub params: ScopeRef,
    pub body_scope: Option<ScopeRef>,
    pub body: MirExprPtr,
}

#[derive(Debug, Clone)]
pub struct AssociatedFunctionInfo {
    pub entity: EntityRef,
    pub params: ScopeRef,
    pub body_scope: Option<ScopeRef>,
    pub body: MirExprPtr,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub spec: Option<MirSpecPtr>,
    pub mutable: bool,
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
    AssociatedFunction(AssociatedFunctionInfo),
    Variable(VariableInfo),
    Param(LocalInfo),
    SelfParam { mutable: bool },
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

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }

    pub fn ty(&self) -> Rc<Type> {
        self.ty.clone()
    }

    pub fn kind(&self) -> &EntityInfo {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut EntityInfo {
        &mut self.kind
    }

    pub fn as_struct(&self) -> &StructureInfo {
        match &self.kind {
            EntityInfo::Structure(structure_info) => structure_info,
            _ => panic!("Attempting to get a structure of an entity that is not a structure"),
        }
    }

    pub fn as_struct_mut(&mut self) -> &mut StructureInfo {
        match &mut self.kind {
            EntityInfo::Structure(structure_info) => structure_info,
            _ => panic!("Attempting to get a structure of an entity that is not a structure"),
        }
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
            EntityInfo::Primitive => "primitive",
            EntityInfo::Structure { .. } => "structure",
            EntityInfo::Function { .. } => "function",
            EntityInfo::AssociatedFunction(..) => "associated function",
            EntityInfo::Variable { .. } => "variable",
            EntityInfo::Param { .. } => "param",
            EntityInfo::SelfParam { .. } => "self",
            EntityInfo::Field { .. } => "field",
        }
    }
}
