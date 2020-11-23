mod op_codes;
use crate::runtime;
use crate::Value;
pub use op_codes::OpCode;
use ordered_float::OrderedFloat;
use runtime::Error as RuntimeError;
use runtime::Section;
use std::convert::TryInto;

pub struct Vm {
    stack: Vec<Value>,
    ip: usize,
}

pub trait FromBytes {
    fn from_bytes(bytes: &[u8], len: usize) -> Self;
}

macro_rules! from_bytes {
    ($T:ty) => {
        impl FromBytes for $T {
            fn from_bytes(bytes: &[u8], len: usize) -> Self {
                Self::from_be_bytes(
                    (&bytes[0..len])
                        .try_into()
                        .expect("invalid length for slice"),
                )
            }
        }
    };
}

macro_rules! binary_op {
    ($name:ident, $start_op:ident, $op:tt) => {
        fn $name(&mut self, op: OpCode) -> Value {
            let type_index = op as u8 - OpCode::$start_op as u8;
            match type_index {
                0 => {
                    let lhs = self.pop().as_i8();
                    let rhs = self.pop().as_i8();
                    Value::from(lhs $op rhs)
                }
                1 => {
                    let lhs = self.pop().as_i16();
                    let rhs = self.pop().as_i16();
                    Value::from(lhs $op rhs)
                }
                2 => {
                    let lhs = self.pop().as_i32();
                    let rhs = self.pop().as_i32();
                    Value::from(lhs $op rhs)
                }
                3 => {
                    let lhs = self.pop().as_i64();
                    let rhs = self.pop().as_i64();
                    Value::from(lhs $op rhs)
                }
                4 => {
                    let lhs = self.pop().as_u8();
                    let rhs = self.pop().as_u8();
                    Value::from(lhs $op rhs)
                }
                5 => {
                    let lhs = self.pop().as_u16();
                    let rhs = self.pop().as_u16();
                    Value::from(lhs $op rhs)
                }
                6 => {
                    let lhs = self.pop().as_u32();
                    let rhs = self.pop().as_u32();
                    Value::from(lhs $op rhs)
                }
                7 => {
                    let lhs = self.pop().as_u64();
                    let rhs = self.pop().as_u64();
                    Value::from(lhs $op rhs)
                }
                8 => {
                    let lhs = self.pop().as_f32();
                    let rhs = self.pop().as_f32();
                    Value::from(lhs $op rhs)
                }
                9 => {
                    let lhs = self.pop().as_f64();
                    let rhs = self.pop().as_f64();
                    Value::from(lhs $op rhs)
                }
                _ => panic!("Invalid Opcode {} for {}", op, stringify!($name)),
            }
        }
    };
}

macro_rules! conditional_binary_op {
    ($name:ident, $start_op:ident, $op:tt) => {
        fn $name(&mut self, op: OpCode) -> Value {
            let type_index = op as u8 - OpCode::$start_op as u8;
            match type_index {
                0 => {
                    let lhs = self.pop().as_i8();
                    let rhs = self.pop().as_i8();
                    Value::from(lhs $op rhs)
                }
                1 => {
                    let lhs = self.pop().as_i16();
                    let rhs = self.pop().as_i16();
                    Value::from(lhs $op rhs)
                }
                2 => {
                    let lhs = self.pop().as_i32();
                    let rhs = self.pop().as_i32();
                    Value::from(lhs $op rhs)
                }
                3 => {
                    let lhs = self.pop().as_i64();
                    let rhs = self.pop().as_i64();
                    Value::from(lhs $op rhs)
                }
                4 => {
                    let lhs = self.pop().as_u8();
                    let rhs = self.pop().as_u8();
                    Value::from(lhs $op rhs)
                }
                5 => {
                    let lhs = self.pop().as_u16();
                    let rhs = self.pop().as_u16();
                    Value::from(lhs $op rhs)
                }
                6 => {
                    let lhs = self.pop().as_u32();
                    let rhs = self.pop().as_u32();
                    Value::from(lhs $op rhs)
                }
                7 => {
                    let lhs = self.pop().as_u64();
                    let rhs = self.pop().as_u64();
                    Value::from(lhs $op rhs)
                }
                8 => {
                    let lhs = OrderedFloat::from(self.pop().as_f32());
                    let rhs = OrderedFloat::from(self.pop().as_f32());
                    Value::from(lhs $op rhs)
                }
                9 => {
                    let lhs = OrderedFloat::from(self.pop().as_f64());
                    let rhs = OrderedFloat::from(self.pop().as_f64());
                    Value::from(lhs $op rhs)
                }
                _ => panic!("Invalid Opcode {} for {}", op, stringify!($name)),
            }
        }
    };
}

from_bytes!(i8);
from_bytes!(i16);
from_bytes!(i32);
from_bytes!(i64);

from_bytes!(u8);
from_bytes!(u16);
from_bytes!(u32);
from_bytes!(u64);

from_bytes!(f32);
from_bytes!(f64);

pub fn read_to<T: FromBytes>(data: &[u8]) -> T {
    println!("{:?} {}", data, std::mem::size_of::<T>());
    T::from_bytes(data, std::mem::size_of::<T>())
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: vec![],
            ip: 0,
        }
    }

    pub fn run(&mut self, section: &Section) -> Result<(), RuntimeError> {
        self.run_from(section, 0)
    }

    pub fn push_stack(&mut self, data: Value) {
        self.stack.push(data);
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack is empty")
    }

    pub fn top(&self) -> &Value {
        self.stack.last().expect("stack is empty")
    }

    pub fn run_from(&mut self, section: &Section, start_ip: usize) -> Result<(), RuntimeError> {
        println!("Section {} begin", section.id());
        self.ip = start_ip;

        loop {
            if self.ip >= section.len() {
                // is this an unexepcted end of section?
                break;
            }

            // read the next op code and advance the instruction pointer.
            let op_code_raw = unsafe { section.read_unchecked(self.ip) };
            self.ip += 1;
            println!("Raw Opcode: {}", op_code_raw);
            let op_code = OpCode::from_u8(*op_code_raw).unwrap();
            println!("Getting Opcode: {}", op_code);
            match op_code {
                OpCode::LoadI8 => {
                    let data = section.read(self.ip..).expect("unable to read buffer");
                    let value = read_to::<i8>(data);
                    self.ip += 1;

                    self.push_stack(Value::from(value));
                }
                OpCode::LoadI16 => {
                    let data = section.read(self.ip..).expect("unable to read buffer");
                    let value = read_to::<i16>(data);
                    self.ip += 2;

                    self.push_stack(Value::from(value));
                }
                OpCode::LoadI32 => {
                    let data = section.read(self.ip..).expect("unable to read buffer");
                    let value = read_to::<i32>(data);
                    self.ip += 4;

                    self.push_stack(Value::from(value));
                }
                OpCode::LoadI64 => {
                    let data = section.read(self.ip..).expect("unable to read buffer");
                    let value = read_to::<i64>(data);
                    self.ip += 8;

                    self.push_stack(Value::from(value));
                }

                OpCode::LoadU8 => {
                    let data = section.read(self.ip..).expect("unable to read buffer");
                    let value = read_to::<u8>(data);
                    self.ip += 1;

                    self.push_stack(Value::from(value));
                }
                OpCode::LoadU16 => {
                    let data = section.read(self.ip..).expect("unable to read buffer");
                    let value = read_to::<u16>(data);
                    self.ip += 2;

                    self.push_stack(Value::from(value));
                }
                OpCode::LoadU32 => {
                    let data = section.read(self.ip..).expect("unable to read buffer");
                    let value = read_to::<u32>(data);
                    self.ip += 4;

                    self.push_stack(Value::from(value));
                }
                OpCode::LoadU64 => {
                    let data = section.read(self.ip..).expect("unable to read buffer");
                    let value = read_to::<u64>(data);
                    self.ip += 8;

                    self.push_stack(Value::from(value));
                }
                OpCode::LoadF32 => {
                    let data = section.read(self.ip..).expect("unable to read buffer");
                    let value = read_to::<f32>(data);
                    self.ip += 4;

                    self.push_stack(Value::from(value));
                }
                OpCode::LoadF64 => {
                    let data = section.read(self.ip..).expect("unable to read buffer");
                    let value = read_to::<f64>(data);
                    self.ip += 8;

                    self.push_stack(Value::from(value));
                }
                OpCode::LoadTrue => {
                    self.push_stack(Value::from(true));
                }
                OpCode::LoadFalse => {
                    self.push_stack(Value::from(false));
                }
                OpCode::AddI8
                | OpCode::AddI16
                | OpCode::AddI32
                | OpCode::AddI64
                | OpCode::AddU8
                | OpCode::AddU16
                | OpCode::AddU32
                | OpCode::AddU64
                | OpCode::AddF32
                | OpCode::AddF64 => {
                    let value = self.perform_add(op_code);
                    self.push_stack(value);
                }
                OpCode::SubI8
                | OpCode::SubI16
                | OpCode::SubI32
                | OpCode::SubI64
                | OpCode::SubU8
                | OpCode::SubU16
                | OpCode::SubU32
                | OpCode::SubU64
                | OpCode::SubF32
                | OpCode::SubF64 => {
                    let value = self.perform_sub(op_code);
                    self.push_stack(value);
                }
                OpCode::MultI8
                | OpCode::MultI16
                | OpCode::MultI32
                | OpCode::MultI64
                | OpCode::MultU8
                | OpCode::MultU16
                | OpCode::MultU32
                | OpCode::MultU64
                | OpCode::MultF32
                | OpCode::MultF64 => {
                    let value = self.perform_mult(op_code);
                    self.push_stack(value);
                }
                OpCode::DivI8
                | OpCode::DivI16
                | OpCode::DivI32
                | OpCode::DivI64
                | OpCode::DivU8
                | OpCode::DivU16
                | OpCode::DivU32
                | OpCode::DivU64
                | OpCode::DivF32
                | OpCode::DivF64 => {
                    let value = self.perform_div(op_code);
                    self.push_stack(value);
                }
                OpCode::LessI8
                | OpCode::LessI16
                | OpCode::LessI32
                | OpCode::LessI64
                | OpCode::LessU8
                | OpCode::LessU16
                | OpCode::LessU32
                | OpCode::LessU64
                | OpCode::LessF32
                | OpCode::LessF64 => {
                    let value = self.perform_less(op_code);
                    self.push_stack(value);
                }
                OpCode::GreaterI8
                | OpCode::GreaterI16
                | OpCode::GreaterI32
                | OpCode::GreaterI64
                | OpCode::GreaterU8
                | OpCode::GreaterU16
                | OpCode::GreaterU32
                | OpCode::GreaterU64
                | OpCode::GreaterF32
                | OpCode::GreaterF64 => {
                    let value = self.perform_greater(op_code);
                    self.push_stack(value);
                }
                OpCode::LessEqI8
                | OpCode::LessEqI16
                | OpCode::LessEqI32
                | OpCode::LessEqI64
                | OpCode::LessEqU8
                | OpCode::LessEqU16
                | OpCode::LessEqU32
                | OpCode::LessEqU64
                | OpCode::LessEqF32
                | OpCode::LessEqF64 => {
                    let value = self.perform_lesseq(op_code);
                    self.push_stack(value);
                }
                OpCode::GreaterEqI8
                | OpCode::GreaterEqI16
                | OpCode::GreaterEqI32
                | OpCode::GreaterEqI64
                | OpCode::GreaterEqU8
                | OpCode::GreaterEqU16
                | OpCode::GreaterEqU32
                | OpCode::GreaterEqU64
                | OpCode::GreaterEqF32
                | OpCode::GreaterEqF64 => {
                    let value = self.perform_greatereq(op_code);
                    self.push_stack(value);
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::Print => {
                    let value = self.top();
                    println!("{}", value)
                }
                OpCode::NumOps => {}
            }
        }

        println!("Section Complete");
        Ok(())
    }

    binary_op!(perform_add, AddI8, +);
    binary_op!(perform_sub, SubI8, -);
    binary_op!(perform_mult, MultI8, *);
    binary_op!(perform_div, DivI8, /);
    conditional_binary_op!(perform_less, LessI8, <);
    conditional_binary_op!(perform_greater, GreaterI8, >);
    conditional_binary_op!(perform_lesseq, LessEqI8, <);
    conditional_binary_op!(perform_greatereq, GreaterEqI8, >);
}
