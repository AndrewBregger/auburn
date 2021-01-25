mod call_frame;
mod op_codes;

use std::alloc::Layout;

use crate::{OxInstance, OxModule, Section,  OxString, Value, Object, gc::{Allocator, Gc}, runtime::{self, Buffer}};
use crate::{
    gc::{Address, GcAlloc},
    mem::read_to,
};
use call_frame::CallFrame;
use itertools::{self, Itertools};
pub use op_codes::{Instruction, OpCode};
use ordered_float::OrderedFloat;
use runtime::{ArrayBuffer, Error as RuntimeError, OxFunction, OxStruct};

static DEFAULT_MEM_SIZE: usize = 2056;
static DEFAULT_STACK_SIZE: usize = 2056;

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
        let idx = {
            let section = frame.section();
            section.read(frame.ip)
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
    allocator: GcAlloc,

    stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    registers: [Value; 8],
    top_stack: usize,
    top_frame: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            allocator: GcAlloc::new(DEFAULT_MEM_SIZE),
            stack: vec![Value::Unit; DEFAULT_STACK_SIZE],
            registers: [Value::Unit; 8],
            call_stack: vec![CallFrame::default(); 512],
            top_stack: 0,
            top_frame: 0,
        }
    }

    pub fn allocator(&self) -> Allocator {
        self.allocator.allocator()
    }

    pub fn push_stack(&mut self, value: Value) {
        self.stack[self.top_stack] = value;
        self.top_stack += 1;
    }

    pub fn pop(&mut self) -> Value {
        let value = self.top().clone();
        self.top_stack = self.top_stack.saturating_sub(1);
        value
    }

    pub fn top(&self) -> &Value {
        &self.stack[self.top_stack - 1]
    }

    pub fn peek(&self, offset: usize) -> &Value {
        &self.stack[self.top_stack - offset - 1]
    }

    pub fn frame(&self) -> &CallFrame {
        &self.call_stack[self.top_frame - 1]
    }

    pub fn push_frame(&mut self, frame: CallFrame) {
        self.call_stack[self.top_frame] = frame;
        self.top_frame += 1;
    }

    pub fn print_stack(&self) {
        for i in 0..self.top_stack {
            println!("{}| {}", i, self.stack[i]);
        }
    }

    pub fn pop_frame(&mut self) -> CallFrame {
        let value = self.frame().clone();
        self.top_frame = self.top_frame.saturating_sub(1);
        value
    }

    pub fn frame_mut(&mut self) -> &mut CallFrame {
        &mut self.call_stack[self.top_frame.saturating_sub(1)]
    }

    pub fn call_value(&mut self) {
        let value = {
            let frame = self.frame_mut();
            let arity = frame.section().read(frame.ip);
            frame.ip += 1;
            arity
        };
        let top = self.peek(value as usize);
        match top {
            Value::Function(funct) => {
                if value != funct.arity() {
                    panic!(
                        "{} is not given the correct number of parameters, expected {}",
                        funct,
                        funct.arity()
                    );
                }

                let call_frame = CallFrame::new(*funct, self.top_stack - value as usize);
                self.push_frame(call_frame);
            }
            _ => {
                panic!("Unable to call non function object: {}", top);
            }
        }
    }

    pub fn new_instance(&mut self, fields: u16) -> Gc<OxInstance> {
        let instance_address = self.allocate_instance(fields);
        let instance = unsafe {
            let ptr = instance_address.as_ptr() as *mut OxInstance;
            &mut *ptr
        };

        let fields_slice =
            unsafe { std::slice::from_raw_parts_mut(instance.fields_ptr_mut(), fields as usize) };
        let field_values = (0..fields).map(|_| self.pop()).collect_vec();
        let name = self.pop().clone().as_string().clone();
        for (idx, field) in field_values.into_iter().rev().enumerate() {
            fields_slice[idx] = field;
        }
        *instance = OxInstance::new(name, fields);

        Gc::<OxInstance>::new(instance_address)
    }

    pub fn run_module(&mut self, module: Gc<OxModule>) -> Result<(), RuntimeError> {
        self.run(module.code())
    }

    pub fn run(&mut self, function: Gc<OxFunction>) -> Result<(), RuntimeError> {
        let call_frame = CallFrame::new(function, self.top_stack);
        self.push_stack(Value::Function(function));
        self.push_frame(call_frame);

        loop {
            // read the next op code and advance the instruction pointer.

            let op_code_raw = {
                let frame = self.frame_mut();
                let code = frame.section().read(frame.ip);
                frame.ip += 1;
                code
            };

            let op_code = OpCode::from_u8(op_code_raw).unwrap();
            println!("OpCode {:014x} {}", self.frame().ip - 1, op_code);
            match op_code {
                OpCode::LoadI8 => {
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
                OpCode::SetGlobal => {
                    let top = self.pop();
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let idx = read_to::<u8>(frame.section().data(), &mut ip);
                    frame.funct().section_mut().set_global(idx as usize, top);
                    frame.ip = ip;
                }
                OpCode::LoadLocal => {
                    let frame = self.frame_mut();
                    let ip = frame.ip;
                    let local = frame.local_start;
                    let idx = frame.section().read(ip);
                    frame.ip += 1;
                    self.push_stack(self.stack[local + idx as usize].clone());
                }
                OpCode::SetLocal => {
                    let frame = self.frame_mut();
                    let ip = frame.ip;
                    let local = frame.local_start;
                    let idx = frame.section().read(ip);
                    frame.ip += 1;
                    let value = self.pop();
                    self.stack[local + idx as usize] = value;
                }
                OpCode::Label => {
                    // skip the label I am not sure how else to reprsent this.
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let len = read_to::<u8>(frame.section().data(), &mut ip);
                    frame.ip = ip + 1 + len as usize;
                }
                OpCode::JmpTrue => {
                    let cond = self.pop().as_bool();
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let value = read_to::<u16>(frame.section().data(), &mut ip);
                    if cond {
                        frame.ip = ip + value as usize;
                    } else {
                        frame.ip = ip;
                    }
                }
                OpCode::JmpFalse => {
                    let cond = self.pop().as_bool();
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let value = read_to::<u16>(frame.section().data(), &mut ip);
                    if cond == false {
                        frame.ip = ip + value as usize;
                    } else {
                        frame.ip = ip;
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
                OpCode::Return => {
                    let top = self.pop();
                    let last_frame = self.pop_frame();
                    self.top_stack = last_frame.local_start.saturating_sub(1);
                    self.push_stack(top);

                    // if we are returning from the top function then exit.
                    if self.top_frame == 0 {
                        break;
                    }
                }
                OpCode::SetRegister => {
                    let frame = self.frame();
                    let reg = frame.read_byte() as usize;
                    let value = self.pop();
                    self.registers[reg] = value;
                }
                OpCode::LoadRegister => {
                    let frame = self.frame();
                    let reg = frame.read_byte() as usize;
                    self.push_stack(self.registers[reg]);
                }
                OpCode::NewObject => {
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let fields = read_to::<u16>(frame.section().data(), &mut ip);
                    frame.ip = ip;
                    let instance = self.new_instance(fields);
                    let value = Value::Instance(instance);
                    self.push_stack(value);
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
                    let value = self.pop();
                    println!("{}", value)
                }
                OpCode::Exit => {
                    self.print_stack();
                    break;
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

    // allocation interface
    pub fn collect(&mut self) {
        self.mark();
        self.sweep();
    }

    fn mark(&mut self) {}

    fn sweep(&mut self) {}

    pub fn free(&mut self) {
        self.allocator.free_all_allocations();
    }

    pub fn allocate_instance(&mut self, fields: u16) -> Address {
        let size =
            std::mem::size_of::<OxInstance>() + fields as usize * std::mem::size_of::<Value>();
        let layout = Layout::from_size_align(size, std::mem::align_of::<OxStruct>())
            .expect("failed to layout memory for struct");

        self.allocate(layout)
    }

    fn allocate(&mut self, layout: Layout) -> Address {
        match self.allocator.alloc(layout) {
            Some(address) => address,
            None => {
                self.collect();
                self.allocator.alloc(layout).unwrap()
            }
        }
    }

    pub fn deallocate(&mut self, address: Address) {
        self.allocator.dealloc(address)
    }

    pub fn allocate_buffer(&mut self, size: usize) -> Gc<Buffer> {
        // the size of the buffer header and the size of the actual buffer
        let buffer_size = std::mem::size_of::<Buffer>() + size;
        let layout = Layout::from_size_align(buffer_size, std::mem::align_of::<Buffer>())
            .expect("memory layout of buffer is improper");
        let buffer_address = self.allocate(layout);

        unsafe {
            let buffer = &mut *(buffer_address.as_ptr_mut() as *mut Buffer);
            *buffer = Buffer::new(size);
        }

        Gc::<Buffer>::new(buffer_address)
    }

    pub fn allocate_array_buffer<Ty: Sync + Send + Copy + Sized>(
        &mut self,
        count: usize,
    ) -> Gc<ArrayBuffer<Ty>> {
        let buffer = self.allocate_buffer(count * std::mem::size_of::<Ty>());
        Gc::<ArrayBuffer<Ty>>::new(buffer.ptr())
    }

    pub fn allocate_string(&mut self, value: &str) -> OxString {
        let array_buffer = self.allocate_array_buffer::<char>(value.len());
        let mut ox_string = OxString::new(array_buffer, value.len());
        ox_string.set_from_str(value);
        ox_string
    }

    pub fn allocate_function(&mut self, name: OxString, arity: u8, section: Section) -> Gc<OxFunction> {
        let layout = Layout::from_size_align(std::mem::size_of::<OxFunction>(), std::mem::align_of::<OxFunction>()).expect("memory layout of OxFunction is improper");
        let function_address = self.allocate(layout);

        unsafe {
            let buffer = &mut *(function_address.as_ptr_mut() as *mut OxFunction);
            *buffer = OxFunction::new(name, arity, section);
        }

        Gc::<OxFunction>::new(function_address)
    }

    pub fn allocate_module(&mut self, name: OxString, code: Gc<OxFunction>, objects: Vec<Object, Allocator>) -> Gc<OxModule> {
        let layout = Layout::from_size_align(std::mem::size_of::<OxModule>(), std::mem::align_of::<OxModule>()).expect("memory layout of OxModule is improper");
        let module_address = self.allocate(layout);

        unsafe {
            let buffer = &mut *(module_address.as_ptr_mut() as *mut OxModule);
            *buffer = OxModule::new(name, code, objects);
        }
    
        Gc::<OxModule>::new(module_address)
    }
}
