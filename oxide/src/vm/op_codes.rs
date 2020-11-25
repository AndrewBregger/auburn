use ordered_float::OrderedFloat;
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
    "add_f32" => AddF32,
    "add_f64" => AddF64,

    "sub_i8"  => SubI8,
    "sub_i16" => SubI16,
    "sub_i32" => SubI32,
    "sub_i64" => SubI64,
    "sub_u8"  => SubU8,
    "sub_u16" => SubU16,
    "sub_u32" => SubU32,
    "sub_u64" => SubU64,
    "sub_f32" => SubF32,
    "sub_f64" => SubF64,

    "mult_i8"  => MultI8,
    "mult_i16" => MultI16,
    "mult_i32" => MultI32,
    "mult_i64" => MultI64,
    "mult_u8"  => MultU8,
    "mult_u16" => MultU16,
    "mult_u32" => MultU32,
    "mult_u64" => MultU64,
    "mult_f32" => MultF32,
    "mult_f64" => MultF64,

    "div_i8"  => DivI8,
    "div_i16" => DivI16,
    "div_i32" => DivI32,
    "div_i64" => DivI64,
    "div_u8"  => DivU8,
    "div_u16" => DivU16,
    "div_u32" => DivU32,
    "div_u64" => DivU64,
    "div_f32" => DivF32,
    "div_f64" => DivF64,

    "less_i8"  => LessI8,
    "less_i16" => LessI16,
    "less_i32" => LessI32,
    "less_i64" => LessI64,
    "less_u8"  => LessU8,
    "less_u16" => LessU16,
    "less_u32" => LessU32,
    "less_u64" => LessU64,
    "less_f32" => LessF32,
    "less_f64" => LessF64,

    "greater_i8"  => GreaterI8,
    "greater_i16" => GreaterI16,
    "greater_i32" => GreaterI32,
    "greater_i64" => GreaterI64,
    "greater_u8"  => GreaterU8,
    "greater_u16" => GreaterU16,
    "greater_u32" => GreaterU32,
    "greater_u64" => GreaterU64,
    "greater_f32" => GreaterF32,
    "greater_f64" => GreaterF64,

    "lesseq_i8"  => LessEqI8,
    "lesseq_i16" => LessEqI16,
    "lesseq_i32" => LessEqI32,
    "lesseq_i64" => LessEqI64,
    "lesseq_u8"  => LessEqU8,
    "lesseq_u16" => LessEqU16,
    "lesseq_u32" => LessEqU32,
    "lesseq_u64" => LessEqU64,
    "lesseq_f32" => LessEqF32,
    "lesseq_f64" => LessEqF64,

    "greatereq_i8"  => GreaterEqI8,
    "greatereq_i16" => GreaterEqI16,
    "greatereq_i32" => GreaterEqI32,
    "greatereq_i64" => GreaterEqI64,
    "greatereq_u8"  => GreaterEqU8,
    "greatereq_u16" => GreaterEqU16,
    "greatereq_u32" => GreaterEqU32,
    "greatereq_u64" => GreaterEqU64,
    "greatereq_f32" => GreaterEqF32,
    "greatereq_f64" => GreaterEqF64,

    "load_global" => LoadGlobal,

    "load_true" => LoadTrue,
    "load_false" => LoadFalse,
    "jmp_if_true" => JmpTrue,
    "jmp_if_false" => JmpFalse,
    "jmp" => Jmp,
    "exit" => Exit,
    "call" => Call,
    "label" => Label,
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

/// structure to represent a single instruction.
/// might use this in actual execution but for now it is
/// only for disassembly
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    LoadI8(i8),
    LoadI16(i16),
    LoadI32(i32),
    LoadI64(i64),
    LoadU8(u8),
    LoadU16(u16),
    LoadU32(u32),
    LoadU64(u64),
    LoadF32(OrderedFloat<f32>),
    LoadF64(OrderedFloat<f64>),
    LoadGlobal(u32),
    JmpTrue(u32),
    JmpFalse(u32),
    Jmp(u32),
    Label(String),
    AddI8,
    AddI16,
    AddI32,
    AddI64,
    AddU8,
    AddU16,
    AddU32,
    AddU64,
    AddF32,
    AddF64,
    SubI8,
    SubI16,
    SubI32,
    SubI64,
    SubU8,
    SubU16,
    SubU32,
    SubU64,
    SubF32,
    SubF64,
    MultI8,
    MultI16,
    MultI32,
    MultI64,
    MultU8,
    MultU16,
    MultU32,
    MultU64,
    MultF32,
    MultF64,
    DivI8,
    DivI16,
    DivI32,
    DivI64,
    DivU8,
    DivU16,
    DivU32,
    DivU64,
    DivF32,
    DivF64,
    LessI8,
    LessI16,
    LessI32,
    LessI64,
    LessU8,
    LessU16,
    LessU32,
    LessU64,
    LessF32,
    LessF64,
    GreaterI8,
    GreaterI16,
    GreaterI32,
    GreaterI64,
    GreaterU8,
    GreaterU16,
    GreaterU32,
    GreaterU64,
    GreaterF32,
    GreaterF64,
    LessEqI8,
    LessEqI16,
    LessEqI32,
    LessEqI64,
    LessEqU8,
    LessEqU16,
    LessEqU32,
    LessEqU64,
    LessEqF32,
    LessEqF64,
    GreaterEqI8,
    GreaterEqI16,
    GreaterEqI32,
    GreaterEqI64,
    GreaterEqU8,
    GreaterEqU16,
    GreaterEqU32,
    GreaterEqU64,
    GreaterEqF32,
    GreaterEqF64,
    LoadTrue,
    LoadFalse,
    Call,
    Pop,
    Print,
    Exit,
}

impl Instruction {
    pub fn op_code(&self) -> OpCode {
        match self {
            Self::LoadI8(_) => OpCode::LoadI8,
            Self::LoadI16(_) => OpCode::LoadI16,
            Self::LoadI32(_) => OpCode::LoadI32,
            Self::LoadI64(_) => OpCode::LoadI64,
            Self::LoadU8(_) => OpCode::LoadU8,
            Self::LoadU16(_) => OpCode::LoadU16,
            Self::LoadU32(_) => OpCode::LoadU32,
            Self::LoadU64(_) => OpCode::LoadU64,
            Self::LoadF32(_) => OpCode::LoadF32,
            Self::LoadF64(_) => OpCode::LoadF64,
            Self::JmpTrue(_) => OpCode::JmpTrue,
            Self::JmpFalse(_) => OpCode::JmpFalse,
            Self::Jmp(_) => OpCode::Jmp,
            Self::Label(_) => OpCode::Label,
            Self::LoadGlobal(_) => OpCode::LoadGlobal,
            Self::Exit => OpCode::Exit,
            Self::Call => OpCode::Call,
            Self::LoadTrue => OpCode::LoadTrue,
            Self::LoadFalse => OpCode::LoadFalse,
            Self::AddI8 => OpCode::AddI8,
            Self::AddI16 => OpCode::AddI16,
            Self::AddI32 => OpCode::AddI32,
            Self::AddI64 => OpCode::AddI64,
            Self::AddU8 => OpCode::AddU8,
            Self::AddU16 => OpCode::AddU16,
            Self::AddU32 => OpCode::AddU32,
            Self::AddU64 => OpCode::AddU64,
            Self::AddF32 => OpCode::AddF32,
            Self::AddF64 => OpCode::AddF64,
            Self::SubI8 => OpCode::SubI8,
            Self::SubI16 => OpCode::SubI16,
            Self::SubI32 => OpCode::SubI32,
            Self::SubI64 => OpCode::SubI64,
            Self::SubU8 => OpCode::SubU8,
            Self::SubU16 => OpCode::SubU16,
            Self::SubU32 => OpCode::SubU32,
            Self::SubU64 => OpCode::SubU64,
            Self::SubF32 => OpCode::SubF32,
            Self::SubF64 => OpCode::SubF64,
            Self::MultI8 => OpCode::MultI8,
            Self::MultI16 => OpCode::MultI16,
            Self::MultI32 => OpCode::MultI32,
            Self::MultI64 => OpCode::MultI64,
            Self::MultU8 => OpCode::MultU8,
            Self::MultU16 => OpCode::MultU16,
            Self::MultU32 => OpCode::MultU32,
            Self::MultU64 => OpCode::MultU64,
            Self::MultF32 => OpCode::MultF32,
            Self::MultF64 => OpCode::MultF64,
            Self::DivI8 => OpCode::DivI8,
            Self::DivI16 => OpCode::DivI16,
            Self::DivI32 => OpCode::DivI32,
            Self::DivI64 => OpCode::DivI64,
            Self::DivU8 => OpCode::DivU8,
            Self::DivU16 => OpCode::DivU16,
            Self::DivU32 => OpCode::DivU32,
            Self::DivU64 => OpCode::DivU64,
            Self::DivF32 => OpCode::DivF32,
            Self::DivF64 => OpCode::DivF64,
            Self::LessI8 => OpCode::LessI8,
            Self::LessI16 => OpCode::LessI16,
            Self::LessI32 => OpCode::LessI32,
            Self::LessI64 => OpCode::LessI64,
            Self::LessU8 => OpCode::LessU8,
            Self::LessU16 => OpCode::LessU16,
            Self::LessU32 => OpCode::LessU32,
            Self::LessU64 => OpCode::LessU64,
            Self::LessF32 => OpCode::LessF32,
            Self::LessF64 => OpCode::LessF64,
            Self::GreaterI8 => OpCode::GreaterI8,
            Self::GreaterI16 => OpCode::GreaterI16,
            Self::GreaterI32 => OpCode::GreaterI32,
            Self::GreaterI64 => OpCode::GreaterI64,
            Self::GreaterU8 => OpCode::GreaterU8,
            Self::GreaterU16 => OpCode::GreaterU16,
            Self::GreaterU32 => OpCode::GreaterU32,
            Self::GreaterU64 => OpCode::GreaterU64,
            Self::GreaterF32 => OpCode::GreaterF32,
            Self::GreaterF64 => OpCode::GreaterF64,
            Self::LessEqI8 => OpCode::LessEqI8,
            Self::LessEqI16 => OpCode::LessEqI16,
            Self::LessEqI32 => OpCode::LessEqI32,
            Self::LessEqI64 => OpCode::LessEqI64,
            Self::LessEqU8 => OpCode::LessEqU8,
            Self::LessEqU16 => OpCode::LessEqU16,
            Self::LessEqU32 => OpCode::LessEqU32,
            Self::LessEqU64 => OpCode::LessEqU64,
            Self::LessEqF32 => OpCode::LessEqF32,
            Self::LessEqF64 => OpCode::LessEqF64,
            Self::GreaterEqI8 => OpCode::GreaterEqI8,
            Self::GreaterEqI16 => OpCode::GreaterEqI16,
            Self::GreaterEqI32 => OpCode::GreaterEqI32,
            Self::GreaterEqI64 => OpCode::GreaterEqI64,
            Self::GreaterEqU8 => OpCode::GreaterEqU8,
            Self::GreaterEqU16 => OpCode::GreaterEqU16,
            Self::GreaterEqU32 => OpCode::GreaterEqU32,
            Self::GreaterEqU64 => OpCode::GreaterEqU64,
            Self::GreaterEqF32 => OpCode::GreaterEqF32,
            Self::GreaterEqF64 => OpCode::GreaterEqF64,
            Self::Pop => OpCode::Pop,
            Self::Print => OpCode::Print,
        }
    }
}

impl From<OpCode> for Instruction {
    fn from(other: OpCode) -> Self {
        match other {
            OpCode::Exit => Self::Exit,
            OpCode::AddI8 => Self::AddI8,
            OpCode::AddI16 => Self::AddI16,
            OpCode::AddI32 => Self::AddI32,
            OpCode::AddI64 => Self::AddI64,
            OpCode::AddU8 => Self::AddU8,
            OpCode::AddU16 => Self::AddU16,
            OpCode::AddU32 => Self::AddU32,
            OpCode::AddU64 => Self::AddU64,
            OpCode::AddF32 => Self::AddF32,
            OpCode::AddF64 => Self::AddF64,
            OpCode::SubI8 => Self::SubI8,
            OpCode::SubI16 => Self::SubI16,
            OpCode::SubI32 => Self::SubI32,
            OpCode::SubI64 => Self::SubI64,
            OpCode::SubU8 => Self::SubU8,
            OpCode::SubU16 => Self::SubU16,
            OpCode::SubU32 => Self::SubU32,
            OpCode::SubU64 => Self::SubU64,
            OpCode::SubF32 => Self::SubF32,
            OpCode::SubF64 => Self::SubF64,
            OpCode::MultI8 => Self::MultI8,
            OpCode::MultI16 => Self::MultI16,
            OpCode::MultI32 => Self::MultI32,
            OpCode::MultI64 => Self::MultI64,
            OpCode::MultU8 => Self::MultU8,
            OpCode::MultU16 => Self::MultU16,
            OpCode::MultU32 => Self::MultU32,
            OpCode::MultU64 => Self::MultU64,
            OpCode::MultF32 => Self::MultF32,
            OpCode::MultF64 => Self::MultF64,
            OpCode::DivI8 => Self::DivI8,
            OpCode::DivI16 => Self::DivI16,
            OpCode::DivI32 => Self::DivI32,
            OpCode::DivI64 => Self::DivI64,
            OpCode::DivU8 => Self::DivU8,
            OpCode::DivU16 => Self::DivU16,
            OpCode::DivU32 => Self::DivU32,
            OpCode::DivU64 => Self::DivU64,
            OpCode::DivF32 => Self::DivF32,
            OpCode::DivF64 => Self::DivF64,
            OpCode::LessI8 => Self::LessI8,
            OpCode::LessI16 => Self::LessI16,
            OpCode::LessI32 => Self::LessI32,
            OpCode::LessI64 => Self::LessI64,
            OpCode::LessU8 => Self::LessU8,
            OpCode::LessU16 => Self::LessU16,
            OpCode::LessU32 => Self::LessU32,
            OpCode::LessU64 => Self::LessU64,
            OpCode::LessF32 => Self::LessF32,
            OpCode::LessF64 => Self::LessF64,
            OpCode::GreaterI8 => Self::GreaterI8,
            OpCode::GreaterI16 => Self::GreaterI16,
            OpCode::GreaterI32 => Self::GreaterI32,
            OpCode::GreaterI64 => Self::GreaterI64,
            OpCode::GreaterU8 => Self::GreaterU8,
            OpCode::GreaterU16 => Self::GreaterU16,
            OpCode::GreaterU32 => Self::GreaterU32,
            OpCode::GreaterU64 => Self::GreaterU64,
            OpCode::GreaterF32 => Self::GreaterF32,
            OpCode::GreaterF64 => Self::GreaterF64,
            OpCode::LessEqI8 => Self::LessEqI8,
            OpCode::LessEqI16 => Self::LessEqI16,
            OpCode::LessEqI32 => Self::LessEqI32,
            OpCode::LessEqI64 => Self::LessEqI64,
            OpCode::LessEqU8 => Self::LessEqU8,
            OpCode::LessEqU16 => Self::LessEqU16,
            OpCode::LessEqU32 => Self::LessEqU32,
            OpCode::LessEqU64 => Self::LessEqU64,
            OpCode::LessEqF32 => Self::LessEqF32,
            OpCode::LessEqF64 => Self::LessEqF64,
            OpCode::GreaterEqI8 => Self::GreaterEqI8,
            OpCode::GreaterEqI16 => Self::GreaterEqI16,
            OpCode::GreaterEqI32 => Self::GreaterEqI32,
            OpCode::GreaterEqI64 => Self::GreaterEqI64,
            OpCode::GreaterEqU8 => Self::GreaterEqU8,
            OpCode::GreaterEqU16 => Self::GreaterEqU16,
            OpCode::GreaterEqU32 => Self::GreaterEqU32,
            OpCode::GreaterEqU64 => Self::GreaterEqU64,
            OpCode::GreaterEqF32 => Self::GreaterEqF32,
            OpCode::GreaterEqF64 => Self::GreaterEqF64,
            OpCode::Pop => Self::Pop,
            OpCode::Print => Self::Print,
            _ => panic!(),
        }
    }
}


impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let op_code = self.op_code();
        match self {
            Self::LoadI8(val) => {
                write!(f, "{:014} {}", op_code, val)
            }
            Self::LoadI16(val) => {
                write!(f, "{:014} {}", op_code, val)
            }
            Self::LoadI32(val) => {
                write!(f, "{:014} {}", op_code, val)
            }
            Self::LoadI64(val) => {
                write!(f, "{:014} {}", op_code, val)
            }
            Self::LoadU8(val) => {
                write!(f, "{:014} {}", op_code, val)
            }
            Self::LoadU16(val) => {
                write!(f, "{:014} {}", op_code, val)

            }
            Self::LoadU32(val) => {
                write!(f, "{:014} {}", op_code, val)
            }
            Self::LoadU64(val) => {
                write!(f, "{:014} {}", op_code, val)
            }
            Self::LoadF32(val) => {
                write!(f, "{:014} {}", op_code, val)
            }
            Self::LoadF64(val) => {
                write!(f, "{:014} {}", op_code, val)
            }
            Self::JmpFalse(val) |
            Self::JmpTrue(val) |
            Self::Jmp(val) => {
                write!(f, "{:014} {}", op_code, val)
            }
            Self::Label(label) => {
                write!(f, "{:014} {}", op_code, label)
            }
            Self::LoadGlobal(idx) => {
                write!(f, "{:014} {}", op_code, idx)
            }
            _ => {
                write!(f, "{:014}", op_code)
            }
        }
    }
}