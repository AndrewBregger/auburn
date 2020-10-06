use std::fmt::{Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct TypeId(pub usize);

impl TypeId {
    fn next() -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(0);
        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeKind {
    Invalid,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Char,
    String,
    Unit,
    // Struct {
    // },
    // Function {
    // }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Type {
    id: TypeId,
    kind: TypeKind,
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self {
            id: TypeId::next(),
            kind,
        }
    }

    pub fn id(&self) -> TypeId {
        self.id
    }

    pub fn is_integer(&self) -> bool {
        match self.kind {
            TypeKind::U8
            | TypeKind::U16
            | TypeKind::U32
            | TypeKind::U64
            | TypeKind::I8
            | TypeKind::I16
            | TypeKind::I32
            | TypeKind::I64 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self.kind {
            TypeKind::F32 | TypeKind::F64 => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self.kind {
            TypeKind::Bool => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self.kind {
            TypeKind::String => true,
            _ => false,
        }
    }

    pub fn is_char(&self) -> bool {
        match self.kind {
            TypeKind::Char => true,
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        self.is_integer() || self.is_float() || self.is_bool() || self.is_char()
    }

    pub fn is_struct(&self) -> bool {
        false
    }

    pub fn is_arithmetic(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    pub fn is_signed(&self) -> bool {
        match self.kind {
            TypeKind::I8
            | TypeKind::I16
            | TypeKind::I32
            | TypeKind::I64
            | TypeKind::F32
            | TypeKind::F64 => true,
            _ => false,
        }
    }

    pub fn is_unsigned(&self) -> bool {
        !self.is_signed()
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TypeKind::Invalid => write!(f, "invalid"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U16 => write!(f, "u16"),
            TypeKind::U32 => write!(f, "u32"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::I8 => write!(f, "i8"),
            TypeKind::I16 => write!(f, "i16"),
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::F32 => write!(f, "f32"),
            TypeKind::F64 => write!(f, "f64"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::String => write!(f, "String"),
        }
    }
}
