mod op_codes;
use crate::mem::{read_to, FromBytes};
use crate::runtime;
use crate::Value;
pub use op_codes::{Instruction, OpCode};
use ordered_float::OrderedFloat;
use runtime::Error as RuntimeError;
use runtime::Section;

pub struct Vm {
    stack: Vec<Value>,
    ip: usize,
}

macro_rules! binary_op {
    ($name:ident, $start_op:ident, $op:tt) => {
        fn $name(&mut self, op: OpCode) -> Value {
            let type_index = op as u8 - OpCode::$start_op as u8;
            match type_index {
                0 => {
                    let rhs = self.pop().as_i8();
                    let lhs = self.pop().as_i8();
                    Value::from(lhs $op rhs)
                }
                1 => {
                    let rhs = self.pop().as_i16();
                    let lhs = self.pop().as_i16();
                    Value::from(lhs $op rhs)
                }
                2 => {
                    let rhs = self.pop().as_i32();
                    let lhs = self.pop().as_i32();
                    Value::from(lhs $op rhs)
                }
                3 => {
                    let rhs = self.pop().as_i64();
                    let lhs = self.pop().as_i64();
                    Value::from(lhs $op rhs)
                }
                4 => {
                    let rhs = self.pop().as_u8();
                    let lhs = self.pop().as_u8();
                    Value::from(lhs $op rhs)
                }
                5 => {
                    let rhs = self.pop().as_u16();
                    let lhs = self.pop().as_u16();
                    Value::from(lhs $op rhs)
                }
                6 => {
                    let rhs = self.pop().as_u32();
                    let lhs = self.pop().as_u32();
                    Value::from(lhs $op rhs)
                }
                7 => {
                    let rhs = self.pop().as_u64();
                    let lhs = self.pop().as_u64();
                    Value::from(lhs $op rhs)
                }
                8 => {
                    let rhs = self.pop().as_f32();
                    let lhs = self.pop().as_f32();
                    Value::from(lhs $op rhs)
                }
                9 => {
                    let rhs = self.pop().as_f64();
                    let lhs = self.pop().as_f64();
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
                    let rhs = self.pop().as_i8();
                    let lhs = self.pop().as_i8();
                    Value::from(lhs $op rhs)
                }
                1 => {
                    let rhs = self.pop().as_i16();
                    let lhs = self.pop().as_i16();
                    println!("{} {}", rhs, lhs);
                    Value::from(lhs $op rhs)
                }
                2 => {
                    let rhs = self.pop().as_i32();
                    let lhs = self.pop().as_i32();
                    Value::from(lhs $op rhs)
                }
                3 => {
                    let rhs = self.pop().as_i64();
                    let lhs = self.pop().as_i64();
                    Value::from(lhs $op rhs)
                }
                4 => {
                    let rhs = self.pop().as_u8();
                    let lhs = self.pop().as_u8();
                    Value::from(lhs $op rhs)
                }
                5 => {
                    let rhs = self.pop().as_u16();
                    let lhs = self.pop().as_u16();
                    Value::from(lhs $op rhs)
                }
                6 => {
                    let rhs = self.pop().as_u32();
                    let lhs = self.pop().as_u32();
                    Value::from(lhs $op rhs)
                }
                7 => {
                    let rhs = self.pop().as_u64();
                    let lhs = self.pop().as_u64();
                    Value::from(lhs $op rhs)
                }
                8 => {
                    let rhs = OrderedFloat::from(self.pop().as_f32());
                    let lhs = OrderedFloat::from(self.pop().as_f32());
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

macro_rules! load_constant {
    ($cond:ident, $name:literal, $self:expr, $section:expr) => {
        let idx = *unsafe { $section.read_unchecked($self.ip) };
        $self.ip += 1;
        let value = $section.get_constant(idx as usize);
        if value.$cond() {
            $self.push_stack(value);
        }
        else {
            panic!("loading {} constant that is not an {}", $name, $name);
        }
    }
}
impl Vm {
    pub fn new() -> Self {
        Self {
            stack: vec![],
            ip: 0,
        }
    }

    pub fn run(&mut self, section: &mut Section) -> Result<(), RuntimeError> {
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

    pub fn run_from(&mut self, section: &mut Section, start_ip: usize) -> Result<(), RuntimeError> {
        println!("Section {} begin", section.id());
        self.ip = start_ip;

        loop {
            if self.ip >= section.len() {
                // is this an unexepcted end of section?
                break;
            }


            // for (idx, value) in self.stack.iter().enumerate() {
            //     println!("{}| {}", idx, value);
            // }

            // read the next op code and advance the instruction pointer.
            let op_code_raw = unsafe { section.read_unchecked(self.ip) };
            self.ip += 1;
            let op_code = OpCode::from_u8(*op_code_raw).unwrap();
            match op_code {
                OpCode::LoadI8 => {
                    load_constant!(is_i8, "i8", self, section);
                }
                OpCode::LoadI16 => {
                    load_constant!(is_i16, "i16", self, section);
                }
                OpCode::LoadI32 => {
                    load_constant!(is_i32, "i32", self, section);
                }
                OpCode::LoadI64 => {
                    load_constant!(is_i64, "i64", self, section);
                }
                OpCode::LoadU8 => {
                    load_constant!(is_u8,  "u8", self, section);
                }
                OpCode::LoadU16 => {
                    load_constant!(is_u16, "u16", self, section);
                }
                OpCode::LoadU32 => {
                    load_constant!(is_u32, "u32", self, section);
                }
                OpCode::LoadU64 => {
                    load_constant!(is_u64, "u64", self, section);
                }
                OpCode::LoadF32 => {
                    load_constant!(is_f32, "f8", self, section);
                }
                OpCode::LoadF64 => {
                    load_constant!(is_f64, "f64", self, section);
                }
                OpCode::LoadTrue => {
                    self.push_stack(Value::from(true));
                }
                OpCode::LoadFalse => {
                    self.push_stack(Value::from(false));
                }
                OpCode::LoadGlobal => {
                    let value = read_to::<u8>(section.data(), &mut self.ip);
                    self.push_stack(section.get_global(value as usize));
                }
                OpCode::Label => {
                    // skip the label I am not sure how else to reprsent this.
                    let len = read_to::<u8>(section.data(), &mut self.ip);
                    self.ip += 1 + len as usize;
                }
                OpCode::JmpTrue => {
                    let value = read_to::<u16>(section.data(), &mut self.ip);
                    let cond = self.top().as_bool();
                    if cond {
                        self.ip += value as usize;
                    }
                }
                OpCode::JmpFalse => {
                    let value = read_to::<u16>(section.data(), &mut self.ip);
                    let cond = self.top().as_bool();
                    if cond == false {
                        self.ip += value as usize;
                    }
                }
                OpCode::Jmp => {
                    let value = read_to::<u16>(section.data(), &mut self.ip);
                    self.ip += value as usize;
                }
                OpCode::Loop => {
                    let value = read_to::<u16>(section.data(), &mut self.ip);
                    self.ip -= value as usize;
                }
                OpCode::SetGlobal => {
                    let idx= read_to::<u8>(section.data(), &mut self.ip);
                    let top = self.pop();
                    section.set_global(idx as usize, top);
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
                OpCode::Call => {}
                OpCode::Print => {
                    let value = self.top();
                    println!("{}", value)
                }
                OpCode::Exit => { break }
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
