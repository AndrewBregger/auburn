use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Value {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Bool(bool),
}

impl Value {
    pub fn ty(&self) -> &'static str {
        match self {
            Self::I8(_) => "i8",
            Self::I16(_) => "i16",
            Self::I32(_) => "i32",
            Self::I64(_) => "i64",
            Self::U8(_) => "u8",
            Self::U16(_) => "u16",
            Self::U32(_) => "u32",
            Self::U64(_) => "u64",
            Self::F32(_) => "f32",
            Self::F64(_) => "f64",
            Self::Bool(_) => "bool",
        }
    }

    pub fn as_i8(&self) -> i8 {
        if let Self::I8(val) = self {
            *val
        } else {
            panic!("Attempting to get an i8 from a value of type {}", self.ty());
        }
    }
    pub fn as_i16(&self) -> i16 {
        if let Self::I16(val) = self {
            *val
        } else {
            panic!(
                "Attempting to get an i16 from a value of type {}",
                self.ty()
            );
        }
    }
    pub fn as_i32(&self) -> i32 {
        if let Self::I32(val) = self {
            *val
        } else {
            panic!(
                "Attempting to get an i32 from a value of type {}",
                self.ty()
            );
        }
    }
    pub fn as_i64(&self) -> i64 {
        if let Self::I64(val) = self {
            *val
        } else {
            panic!(
                "Attempting to get an i64 from a value of type {}",
                self.ty()
            );
        }
    }
    pub fn as_u8(&self) -> u8 {
        if let Self::U8(val) = self {
            *val
        } else {
            panic!("Attempting to get an u8 from a value of type {}", self.ty());
        }
    }
    pub fn as_u16(&self) -> u16 {
        if let Self::U16(val) = self {
            *val
        } else {
            panic!(
                "Attempting to get an u16 from a value of type {}",
                self.ty()
            );
        }
    }
    pub fn as_u32(&self) -> u32 {
        if let Self::U32(val) = self {
            *val
        } else {
            panic!(
                "Attempting to get an u32 from a value of type {}",
                self.ty()
            );
        }
    }
    pub fn as_u64(&self) -> u64 {
        if let Self::U64(val) = self {
            *val
        } else {
            panic!(
                "Attempting to get an u64 from a value of type {}",
                self.ty()
            );
        }
    }
    pub fn as_f32(&self) -> f32 {
        if let Self::F32(val) = self {
            *val
        } else {
            panic!(
                "Attempting to get an f32 from a value of type {}",
                self.ty()
            );
        }
    }
    pub fn as_f64(&self) -> f64 {
        if let Self::F64(val) = self {
            *val
        } else {
            panic!(
                "Attempting to get an f64 from a value of type {}",
                self.ty()
            );
        }
    }
    pub fn as_bool(&self) -> bool {
        if let Self::Bool(val) = self {
            *val
        } else {
            panic!(
                "Attempting to get an bool from a value of type {}",
                self.ty()
            );
        }
    }

    pub fn is_i8(&self) -> bool {
        match self {
            Self::I8(_) => true,
            _ => false,
        }
    }

    pub fn is_i16(&self) -> bool {
        match self {
            Self::I16(_) => true,
            _ => false,
        }
    }

    pub fn is_i32(&self) -> bool {
        match self {
            Self::I32(_) => true,
            _ => false,
        }
    }

    pub fn is_i64(&self) -> bool {
        match self {
            Self::I64(_) => true,
            _ => false,
        }
    }
    
    pub fn is_u8(&self) -> bool {
        match self {
            Self::U8(_) => true,
            _ => false,
        }
    }

    pub fn is_u16(&self) -> bool {
        match self {
            Self::U16(_) => true,
            _ => false,
        }
    }

    pub fn is_u32(&self) -> bool {
        match self {
            Self::U32(_) => true,
            _ => false,
        }
    }

    pub fn is_u64(&self) -> bool {
        match self {
            Self::U64(_) => true,
            _ => false,
        }
    }

    pub fn is_f32(&self) -> bool {
        match self {
            Self::F32(_) => true,
            _ => false,
        }
    }

    pub fn is_f64(&self) -> bool {
        match self {
            Self::F64(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::Bool(_) => true,
            _ => false,
        }
    }

}

macro_rules! value_from {
    ($var:ident, $T:ty) => {
        impl From<$T> for Value {
            fn from(other: $T) -> Self {
                Self::$var(other)
            }
        }
    };
}

value_from!(I8, i8);
value_from!(I16, i16);
value_from!(I32, i32);
value_from!(I64, i64);

value_from!(U8, u8);
value_from!(U16, u16);
value_from!(U32, u32);
value_from!(U64, u64);

value_from!(F32, f32);
value_from!(F64, f64);

value_from!(Bool, bool);

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I8(val) => write!(f, "{}", val),
            Self::I16(val) => write!(f, "{}", val),
            Self::I32(val) => write!(f, "{}", val),
            Self::I64(val) => write!(f, "{}", val),
            Self::U8(val) => write!(f, "{}", val),
            Self::U16(val) => write!(f, "{}", val),
            Self::U32(val) => write!(f, "{}", val),
            Self::U64(val) => write!(f, "{}", val),
            Self::F32(val) => write!(f, "{}", val),
            Self::F64(val) => write!(f, "{}", val),
            Self::Bool(val) => write!(f, "{}", val),
        }
    }
}
