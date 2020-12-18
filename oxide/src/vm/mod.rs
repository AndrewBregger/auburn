mod call_frame;
mod op_codes;

use crate::mem::read_to;
use crate::runtime;
use crate::Value;
use call_frame::CallFrame;
pub use op_codes::{Instruction, OpCode};
use ordered_float::OrderedFloat;
use runtime::{Error as RuntimeError, OxFunction};

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
    ($cond:ident, $name:literal, $self:expr) => {
        let frame = $self.frame_mut();
        let idx = *unsafe {
            let section = frame.section();
            section.read_unchecked(frame.ip)
        };
        frame.ip += 1;
        let value = frame.section().get_constant(idx as usize);
        if value.$cond() {
            $self.push_stack(value);
        } else {
            panic!("loading {} constant that is an {}", $name, value.ty());
        }
    };
}

pub struct Vm {
    stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: vec![],
            call_stack: Vec::with_capacity(512),
        }
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

    pub fn peek(&self, offset: usize) -> &Value {
        &self.stack[self.stack.len() - offset]
    }

    pub fn frame(&self) -> &CallFrame {
        self.call_stack.last().expect("call stack empty")
    }

    pub fn frame_mut(&mut self) -> &mut CallFrame {
        self.call_stack.last_mut().expect("call stack empty")
    }

    pub fn call_value(&mut self) {
        let value = {
            let frame = self.frame_mut();
            let arity = unsafe { *frame.section().read_unchecked(frame.ip) };
            frame.ip += 1;
            arity
        };

        let top = self.peek(value as usize + 1);
        match top {
            Value::Function(funct) => {
                if value != funct.arity() {
                    panic!(
                        "{} is not given the correct number of parameters, expected {}",
                        funct,
                        funct.arity()
                    );
                }

                let funct = funct.as_ref() as *const _;
                let call_frame = CallFrame::new(funct, self.stack.len() - value as usize);
                self.call_stack.push(call_frame);
            }
            _ => {
                panic!("Unable to call non function object")
            }
        }
    }

    pub fn run(&mut self, function: Box<OxFunction>) -> Result<(), RuntimeError> {
        let funct = function.as_ref() as *const _;

        let call_frame = CallFrame::new(funct, self.stack.len());
        self.push_stack(Value::Function(function));
        self.call_stack.push(call_frame);

        loop {
            // read the next op code and advance the instruction pointer.

            let op_code_raw = {
                let frame = self.frame_mut();
                let code = *unsafe { frame.section().read_unchecked(frame.ip) };
                frame.ip += 1;
                code
            };

            let op_code = OpCode::from_u8(op_code_raw).unwrap();
            println!("OpCode {}", op_code);
            match op_code {
                OpCode::LoadI8 => {
                    for (idx, value) in self.stack.iter().enumerate() {
                        println!("{}| {}", idx, value);
                    }
                    load_constant!(is_i8, "i8", self);
                }
                OpCode::LoadI16 => {
                    load_constant!(is_i16, "i16", self);
                }
                OpCode::LoadI32 => {
                    load_constant!(is_i32, "i32", self);
                }
                OpCode::LoadI64 => {
                    load_constant!(is_i64, "i64", self);
                }
                OpCode::LoadU8 => {
                    load_constant!(is_u8, "u8", self);
                }
                OpCode::LoadU16 => {
                    load_constant!(is_u16, "u16", self);
                }
                OpCode::LoadU32 => {
                    load_constant!(is_u32, "u32", self);
                }
                OpCode::LoadU64 => {
                    load_constant!(is_u64, "u64", self);
                }
                OpCode::LoadF32 => {
                    load_constant!(is_f32, "f8", self);
                }
                OpCode::LoadF64 => {
                    load_constant!(is_f64, "f64", self);
                }
                OpCode::LoadStr => {
                    load_constant!(is_string, "string", self);
                }
                OpCode::LoadTrue => {
                    self.push_stack(Value::from(true));
                }
                OpCode::LoadFalse => {
                    self.push_stack(Value::from(false));
                }
                OpCode::LoadGlobal => {
                    let global = {
                        let frame = self.frame_mut();
                        let mut ip = frame.ip;
                        let value = read_to::<u8>(frame.section().data(), &mut ip);
                        frame.ip = ip;
                        frame.section().get_global(value as usize)
                    };

                    self.push_stack(global);
                }
                OpCode::Label => {
                    // skip the label I am not sure how else to reprsent this.
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let len = read_to::<u8>(frame.section().data(), &mut ip);
                    frame.ip = ip + 1 + len as usize;
                }
                OpCode::JmpTrue => {
                    let cond = self.top().as_bool();
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let value = read_to::<u16>(frame.section().data(), &mut ip);
                    if cond {
                        frame.ip = ip + value as usize;
                    }
                }
                OpCode::JmpFalse => {
                    let cond = self.top().as_bool();
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let value = read_to::<u16>(frame.section().data(), &mut ip);
                    if cond == false {
                        frame.ip = ip + value as usize;
                    }
                }
                OpCode::Jmp => {
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let value = read_to::<u16>(frame.section().data(), &mut ip);
                    frame.ip = ip + value as usize;
                }
                OpCode::Loop => {
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let value = read_to::<u16>(frame.section().data(), &mut ip);
                    frame.ip = ip - value as usize;
                }
                OpCode::SetGlobal => {
                    let top = self.top().clone();
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let idx = read_to::<u8>(frame.section().data(), &mut ip);
                    frame
                        .funct_mut()
                        .section_mut()
                        .set_global(idx as usize, top);
                    frame.ip = ip;
                }
                OpCode::Return => {
                    let top = self.pop();
                    let current_frame = self.call_stack.pop().expect("call stack is empty");
                    self.stack.truncate(current_frame.local_start);
                    self.push_stack(top);

                    if self.call_stack.is_empty() {
                        break;
                    }
                }
                OpCode::LoadLocal => {
                    let frame = self.frame_mut();
                    let ip = frame.ip;
                    let local = frame.local_start;
                    let idx = unsafe { *frame.section().read_unchecked(ip) };
                    frame.ip += 1;
                    self.push_stack(self.stack[local + idx as usize].clone());
                }
                OpCode::SetLocal => {
                    let frame = self.frame_mut();
                    let ip = frame.ip;
                    let local = frame.local_start;
                    let idx = unsafe { *frame.section().read_unchecked(ip) };
                    frame.ip += 1;
                    let value = self.top().clone();
                    self.stack[local + idx as usize] = value;
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
                OpCode::Call => self.call_value(),
                OpCode::Print => {
                    let value = self.top();
                    println!("{}", value)
                }
                OpCode::Exit => break,
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
