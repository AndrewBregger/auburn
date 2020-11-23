use std::fmt::{Display, Formatter};
macro_rules! define_opcodes {
    ($($name:literal => $en:ident), *, $ty:ident) => {
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, Eq, PartialEq)]
        pub enum $ty {
            $(
                $en,
            )*
        }

        impl Display for $ty {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$en => write!(f, $name),
                    )*
                }
            }
        }
    }
}

define_opcodes!(
    "load_i8"  => LoadI8,
    "load_i16" => LoadI16,
    "load_i32" => LoadI32,
    "load_i64" => LoadI64,

    "load_u8"  => LoadU8,
    "load_u16" => LoadU16,
    "load_u32" => LoadU32,
    "load_u64" => LoadU64,

    "load_f32" => LoadF32,
    "load_f64" => LoadF64,

    "add_i8"  => AddI8,
    "add_i16" => AddI16,
    "add_i32" => AddI32,
    "add_i64" => AddI64,
    "add_u8"  => AddU8,
    "add_u16" => AddU16,
    "add_u32" => AddU32,
    "add_u64" => AddU64,
    "add_F32" => AddF32,
    "add_F64" => AddF64,

    "sub_i8"  => SubI8,
    "sub_i16" => SubI16,
    "sub_i32" => SubI32,
    "sub_i64" => SubI64,
    "sub_u8"  => SubU8,
    "sub_u16" => SubU16,
    "sub_u32" => SubU32,
    "sub_u64" => SubU64,
    "sub_F32" => SubF32,
    "sub_F64" => SubF64,

    "mult_i8"  => MultI8,
    "mult_i16" => MultI16,
    "mult_i32" => MultI32,
    "mult_i64" => MultI64,
    "mult_u8"  => MultU8,
    "mult_u16" => MultU16,
    "mult_u32" => MultU32,
    "mult_u64" => MultU64,
    "mult_F32" => MultF32,
    "mult_F64" => MultF64,

    "div_i8"  => DivI8,
    "div_i16" => DivI16,
    "div_i32" => DivI32,
    "div_i64" => DivI64,
    "div_u8"  => DivU8,
    "div_u16" => DivU16,
    "div_u32" => DivU32,
    "div_u64" => DivU64,
    "div_F32" => DivF32,
    "div_F64" => DivF64,

    "less_i8"  => LessI8,
    "less_i16" => LessI16,
    "less_i32" => LessI32,
    "less_i64" => LessI64,
    "less_u8"  => LessU8,
    "less_u16" => LessU16,
    "less_u32" => LessU32,
    "less_u64" => LessU64,
    "less_F32" => LessF32,
    "less_F64" => LessF64,

    "greater_i8"  => GreaterI8,
    "greater_i16" => GreaterI16,
    "greater_i32" => GreaterI32,
    "greater_i64" => GreaterI64,
    "greater_u8"  => GreaterU8,
    "greater_u16" => GreaterU16,
    "greater_u32" => GreaterU32,
    "greater_u64" => GreaterU64,
    "greater_F32" => GreaterF32,
    "greater_F64" => GreaterF64,

    "lesseq_i8"  => LessEqI8,
    "lesseq_i16" => LessEqI16,
    "lesseq_i32" => LessEqI32,
    "lesseq_i64" => LessEqI64,
    "lesseq_u8"  => LessEqU8,
    "lesseq_u16" => LessEqU16,
    "lesseq_u32" => LessEqU32,
    "lesseq_u64" => LessEqU64,
    "lesseq_F32" => LessEqF32,
    "lesseq_F64" => LessEqF64,

    "greatereq_i8"  => GreaterEqI8,
    "greatereq_i16" => GreaterEqI16,
    "greatereq_i32" => GreaterEqI32,
    "greatereq_i64" => GreaterEqI64,
    "greatereq_u8"  => GreaterEqU8,
    "greatereq_u16" => GreaterEqU16,
    "greatereq_u32" => GreaterEqU32,
    "greatereq_u64" => GreaterEqU64,
    "greatereq_F32" => GreaterEqF32,
    "greatereq_F64" => GreaterEqF64,

    "load_true" => LoadTrue,
    "load_false" => LoadFalse,
    "pop" => Pop,
    "print" => Print,
    "__NUMOPS__" => NumOps,
    OpCode
);

impl OpCode {
    pub fn from_u8(other: u8) -> Option<Self> {
        if Self::LoadI8 as u8 <= other && other < Self::NumOps as u8 {
            Some(unsafe { std::mem::transmute(other) })
        } else {
            None
        }
    }
}
