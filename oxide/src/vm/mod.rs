mod call_frame;
mod op_codes;

use std::alloc::Layout;

use crate::{
    gc::{Address, Gc, GcAlloc, Object, ObjectKind, VecAllocator},
    mem::read_to,
    runtime, OxInstance, OxModule, OxString, OxTuple, OxVec, Section, Value,
};
use call_frame::CallFrame;
pub use op_codes::{Instruction, OpCode};
use ordered_float::OrderedFloat;
use runtime::{AttributeAccess, OxFunction, OxStruct};

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
    gray_list: Vec<Address>,
    registers: [Value; 8],
    top_stack: usize,
    top_frame: usize,
    no_collection: bool,
    code_gen: bool, // the vm is in code gen mode
    locals: bool,   // these values are locals and not currently on the stack
    pub compiler_address: Vec<Address>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            allocator: GcAlloc::new(),
            stack: vec![Value::Unit; DEFAULT_STACK_SIZE],
            registers: [Value::Unit; 8],
            gray_list: vec![],
            call_stack: vec![CallFrame::default(); 512],
            top_stack: 0,
            top_frame: 0,
            no_collection: false,
            code_gen: false,
            locals: false,
            compiler_address: vec![],
        }
    }

    pub fn set_code_gen(&mut self, mode: bool) {
        self.code_gen = mode;
        if !mode && !self.locals {
            println!("Clearing compiler addresses: code_gen");
            self.compiler_address.clear();
        }
    }

    pub fn set_locals_mode(&mut self, mode: bool) {
        self.locals = mode;
        if !mode && !self.code_gen {
            println!("Clearing compiler addresses: locals");
            self.compiler_address.clear();
        }
    }

    pub fn force_no_collection(&mut self, val: bool) {
        self.no_collection = val;
    }

    pub fn memory_usage(&self) -> usize {
        self.allocator.memory_usage()
    }

    pub fn allocator_vec(&self) -> VecAllocator {
        self.allocator.allocator_vec()
    }

    pub fn push_stack(&mut self, value: Value) {
        self.stack[self.top_stack] = value;
        self.top_stack += 1;
    }

    pub fn allocator(&self) -> &GcAlloc {
        &self.allocator
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

    fn call_value(&mut self, value: &Value, arity: u8) -> Result<(), runtime::Error> {
        println!("call value: {}", value);
        match value {
            Value::Function(funct) => {
                if arity != funct.arity() {
                    panic!(
                        "{} is not given the correct number of parameters, expected {}",
                        funct,
                        funct.arity()
                    );
                }
                let stack_start = self.top_stack - arity as usize;
                println!("calling {} start_stack: {}", funct.name(), stack_start);
                // self.print_stack();
                let call_frame = CallFrame::new(*funct, stack_start);
                self.push_frame(call_frame);
                Ok(())
            }
            _ => Err(runtime::Error::CallingInvalidValue(value.ty().to_string())),
        }
    }

    fn call(&mut self) -> Result<(), runtime::Error> {
        let arity = {
            let frame = self.frame_mut();
            let arity = frame.section().read(frame.ip);
            frame.ip += 1;
            arity
        };

        let function = self.peek(arity as usize).clone();
        self.call_value(&function, arity)
    }

    pub fn run_module(&mut self, module: Gc<OxModule>) -> Result<(), runtime::Error> {
        println!("pushing module to stack");
        self.push_stack(Value::from(module.clone()));
        println!("Entry lookup");
        if let Some(entry_function) = module.entry() {
            println!("found entry");
            self.push_stack(entry_function.clone());
            println!("calling value");
            self.call_value(&entry_function, 0)?;

            self.run()
        } else {
            Err(runtime::Error::missing_module_entry(
                module.as_ref().name().clone(),
            ))
        }
    }

    pub fn run(&mut self) -> Result<(), runtime::Error> {
        //self.push_frame(call_frame);

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
                OpCode::LoadChar => {
                    load_constant!(is_char, "char", self);
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
                    let local = frame.local_start;
                    let idx = frame.section().read(frame.ip);
                    frame.ip += 1;
                    self.push_stack(self.stack[local + idx as usize].clone());
                }
                OpCode::SetLocal => {
                    let frame = self.frame_mut();
                    let local = frame.local_start;
                    let idx = frame.section().read(frame.ip);
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
                OpCode::NewInstance => {
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let count = read_to::<u16>(frame.section().data(), &mut ip);
                    frame.ip = ip;

                    let mut fields = OxVec::with_capacity(self.allocator_vec(), count as usize);
                    (0..count).rev().for_each(|_| fields.push(self.pop()));
                    println!("{:#?}", fields);
                    // println!("Struct: {}", self.top());
                    let object = self.pop().as_struct().clone();
                    println!("struct {}", object.as_ref());
                    let instance = self.new_instance(object, fields);
                    self.push_stack(Value::from(instance));
                }
                OpCode::NewTuple => {
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let count = read_to::<u16>(frame.section().data(), &mut ip);
                    // println!("elements: {}", count);
                    frame.ip = ip;
                    let mut elements = OxVec::with_capacity(self.allocator_vec(), count as usize);
                    (0..count).rev().for_each(|_| elements.push(self.pop()));

                    let tuple = self.new_tuple(elements);

                    self.push_stack(Value::from(tuple));
                }
                OpCode::InstanceAttr => {
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let count = read_to::<u16>(frame.section().data(), &mut ip);
                    frame.ip = ip;

                    let value = self.pop();
                    let s = value.as_instance().clone();
                    self.push_stack(s.get_attr(count as usize).clone())
                }
                OpCode::TupleAttr => {
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let count = read_to::<u16>(frame.section().data(), &mut ip);
                    frame.ip = ip;

                    let value = self.pop();
                    let s = value.as_tuple().clone();
                    self.push_stack(s.get_attr(count as usize).clone())
                }
                OpCode::SetAttr => {
                    let frame = self.frame_mut();
                    let idx = frame.section().read(frame.ip);
                    frame.ip += 1;
                    let value = self.pop();
                    let mut obj = self.pop();
                    *obj.as_instance_mut().get_attr_mut(idx as usize) = value;
                }
                OpCode::PushLocal => {
                    // no-op
                    // Technically this can be remvoed.
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
                    self.print_stack();
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
                OpCode::Call => self.call()?,
                OpCode::Echo => {
                    let value = self.pop();
                    println!("{}", value)
                }
                OpCode::FrameStack => {
                    let frame = self.frame();
                    let local_stack = frame.local_start;
                    println!("-------Frame Stack {:10}---------", frame.function.name());
                    for idx in local_stack..self.top_stack {
                        println!("{}| {}", idx, self.stack[idx]);
                    }
                    println!("------------------------------");
                }
                OpCode::Exit => {
                    self.print_stack();
                    break;
                }
                OpCode::NumOps => {}
            }
        }

        println!("Section Complete");
        println!("Memory Usage at end: {}", self.allocator.memory_usage());
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

    fn allocate(&mut self, kind: ObjectKind, layout: Layout) -> Address {
        // self.allocator.dump_mem_stats();
        // println!("Should collect: {}", self.allocator.should_collect());
        if !self.no_collection && self.allocator.should_collect() {
            self.collect();
        }

        let address = match self.allocator.alloc(kind, layout) {
            Ok(address) => address,
            Err(_) => {
                // collect and retry
                self.collect();
                self.allocator
                    .alloc(kind, layout)
                    .expect("alloc failed: out of memory")
            }
        };

        if self.locals || self.code_gen {
            println!("adding to compiler list");
            self.compiler_address.push(address);
        }

        address
    }

    fn allocate_from<Ty: Object>(&mut self) -> Address {
        let layout = Layout::new::<Ty>();
        self.allocate(Ty::object_kind(), layout)
    }

    pub fn deallocate(&mut self, address: Address) {
        self.allocator.dealloc(address)
    }

    pub fn new_string_from_str(&mut self, val: &str) -> OxString {
        let x = OxString::with_value(self.allocator_vec(), val);
        // if cfg!(debug_assertions) {
        //     println!("new_string {}", self.allocator.last_record().unwrap());
        // }
        x
    }

    pub fn new_gc_string_from_str(&mut self, val: &str) -> Gc<OxString> {
        let address = self.allocate_from::<OxStruct>();
        let x = Gc::with_value(address, self.new_string_from_str(val));
        if cfg!(debug_assertions) {
            println!("new_string_gc {}", self.allocator.last_record().unwrap());
        }
        x
    }

    pub fn vec_with_capacity<Ty>(&mut self, len: usize) -> OxVec<Ty> {
        let x = OxVec::with_capacity(self.allocator_vec(), len);
        if cfg!(debug_assertions) {
            println!(
                "vec_with_capacity {}",
                self.allocator.last_record().unwrap()
            );
        }
        x
    }

    pub fn new_vec<Ty>(&mut self) -> OxVec<Ty> {
        OxVec::new(self.allocator_vec())
    }

    pub fn new_module(&mut self, name: OxString, objects: OxVec<Value>) -> Gc<OxModule> {
        let address = self.allocate_from::<OxModule>();
        if cfg!(debug_assertions) {
            println!("new_module {}", self.allocator.last_record().unwrap());
        }
        Gc::with_value(address, OxModule::new(name, None, objects))
    }

    pub fn new_entry_module(
        &mut self,
        name: OxString,
        entry: usize,
        objects: OxVec<Value>,
    ) -> Gc<OxModule> {
        let address = self.allocate_from::<OxModule>();
        if cfg!(debug_assertions) {
            println!("new_entry_module {}", self.allocator.last_record().unwrap());
        }
        Gc::with_value(address, OxModule::new(name, Some(entry), objects))
    }

    pub fn new_function(&mut self, name: OxString, arity: u8, section: Section) -> Gc<OxFunction> {
        let address = self.allocate_from::<OxFunction>();
        Gc::with_value(address, OxFunction::new(name, section, arity))
    }

    pub fn new_section(&mut self) -> Section {
        if cfg!(debug_assertions) {
            Section::new(self)
        } else {
            Section::new(self)
        }
    }

    pub fn new_struct(&mut self, name: OxString, methods: OxVec<Gc<OxFunction>>) -> Gc<OxStruct> {
        let address = self.allocate_from::<OxStruct>();
        if cfg!(debug_assertions) {
            println!("new_struct {}", self.allocator.last_record().unwrap());
        }
        Gc::with_value(address, OxStruct::new(name, methods))
    }

    pub fn new_instance(&mut self, object: Gc<OxStruct>, fields: OxVec<Value>) -> Gc<OxInstance> {
        let address = self.allocate_from::<OxInstance>();
        Gc::with_value(address, OxInstance::new(object, fields))
    }

    pub fn new_tuple(&mut self, elements: OxVec<Value>) -> Gc<OxTuple> {
        let address = self.allocate_from::<OxTuple>();
        Gc::with_value(address, OxTuple::new(elements))
    }

    pub fn dump_mem_stats(&self) {
        self.allocator.dump_mem_stats();
    }
}

impl Vm {
    fn collect(&mut self) {
        println!("--Beginning collect: {}", self.memory_usage());
        // let backtrace = std::backtrace::Backtrace::force_capture();
        // println!("{}", backtrace);

        self.mark();
        self.sweep();

        // clean up after collection
        self.gray_list.clear();
        println!("--End collect: {}", self.memory_usage());
    }

    fn mark(&mut self) {
        // println!("--mark");
        self.mark_roots();
        self.mark_compiler();

        self.follow_references();
    }

    fn sweep(&mut self) {
        self.allocator.sweep();
    }

    fn mark_roots(&mut self) {
        // println!("----mark roots");
        // println!("stack size: {}", self.top_stack);
        for value in &self.stack[0..self.top_stack] {
            Self::mark_value(&mut self.gray_list, value);
        }
        // println!("gray list size: {}", self.gray_list.len());
    }
    fn mark_compiler(&mut self) {
        // println!("----mark compiler");
        // what needs to be marked here?
        for address in self.compiler_address.as_mut_slice() {
            address.mark(true);
        }

        self.gray_list
            .extend_from_slice(self.compiler_address.as_slice());

        // println!(
        //     "Gray list from compiler: {} {}",
        //     self.gray_list.len(),
        //     self.compiler_address.len()
        // );
    }

    fn follow_references(&mut self) {
        // println!("----follow references");
        while !self.gray_list.is_empty() {
            let address = self
                .gray_list
                .pop()
                .expect("gray list should always have a value here");
            Self::trace_references(&mut self.gray_list, address);
        }
    }

    fn mark_value(gray_list: &mut Vec<Address>, value: &Value) {
        // print!("marking value: {}", value);
        let address = match value {
            Value::String(val) => val.ptr(),
            Value::Function(val) => val.ptr(),
            Value::Struct(val) => val.ptr(),
            Value::Instance(val) => val.ptr(),
            Value::Module(val) => val.ptr(),
            Value::Tuple(val) => val.ptr(),
            _ => {
                // println!();
                return;
            }
        };

        Self::mark_object(gray_list, address);
    }

    fn mark_object(gray_list: &mut Vec<Address>, mut address: Address) {
        let cell = address.cell_mut();
        if cell.marked {
            println!();
            return;
        }
        cell.mark(true);
        gray_list.push(address);

        // println!(" addr: {:p}", address.as_ptr());
    }

    fn trace_references(gray_list: &mut Vec<Address>, address: Address) {
        let cell = address.cell();
        // println!("cell: {:?}", cell);
        match cell.kind {
            ObjectKind::Function => {
                let f = address.into_ref::<OxFunction>();
                Self::trace_function(gray_list, f);
            }
            ObjectKind::Instance => {
                let i = address.into_ref::<OxInstance>();
                Self::trace_instance(gray_list, i);
            }
            ObjectKind::Module => {
                let m = address.into_ref::<OxModule>();
                Self::trace_module(gray_list, m);
            }
            ObjectKind::String => {
                let s = address.into_ref::<OxString>();
                Self::trace_string(gray_list, s);
            }
            ObjectKind::Struct => {
                let s = address.into_ref::<OxStruct>();
                Self::trace_struct(gray_list, s);
            }
            ObjectKind::Tuple => {
                let t = address.into_ref::<OxTuple>();
                Self::trace_tuple(gray_list, t);
            }
            ObjectKind::Section => {
                unreachable!()
            }
            ObjectKind::Vec => {
                unreachable!()
            }
        }
    }

    fn trace_module(gray_list: &mut Vec<Address>, module: &OxModule) {
        // println!("trace module: {}", module.name());
        Self::trace_string(gray_list, module.name());
        Self::trace_vec_value(gray_list, module.objects());
    }

    fn trace_struct(gray_list: &mut Vec<Address>, structure: &OxStruct) {
        // println!("trace struct: {}", structure.name());
        Self::trace_string(gray_list, structure.name());
        Self::trace_vec_gc(gray_list, structure.methods());
    }

    fn trace_tuple(gray_list: &mut Vec<Address>, tuple: &OxTuple) {
        // println!("trace tuple");
        Self::trace_vec_value(gray_list, tuple.elements());
    }

    fn trace_instance(gray_list: &mut Vec<Address>, instance: &OxInstance) {
        // println!("trace instance");
        Self::trace_struct(gray_list, instance.object());
        for field in instance.fields().iter() {
            Self::mark_value(gray_list, field);
        }
    }

    fn trace_function(gray_list: &mut Vec<Address>, funct: &OxFunction) {
        // println!("trace function: {}", funct.name());
        // since these are owned by the function, their references will be followed here.
        let name = funct.name();
        Self::trace_string(gray_list, name);
        let section = funct.section();
        Self::trace_section(gray_list, section);
    }

    fn trace_string(_gray_list: &mut Vec<Address>, vec: &OxString) {
        // println!("trace string");
        // do anything need to happen here?
        Self::trace_vec(vec.buffer());
    }

    fn trace_section(gray_list: &mut Vec<Address>, section: &Section) {
        // println!("trace section");
        Self::trace_vec_value(gray_list, section.constants_ref());
        Self::trace_vec_value(gray_list, section.globals_ref());
        Self::trace_vec(section.data_ref());
    }

    fn trace_vec_value(gray_list: &mut Vec<Address>, vec: &OxVec<Value>) {
        // println!("trace vec by value");
        if Self::trace_vec(vec) {
            for value in vec.iter() {
                Self::mark_value(gray_list, value);
            }
        }
    }

    fn trace_vec_gc<T: Object>(gray_list: &mut Vec<Address>, vec: &OxVec<Gc<T>>) {
        // println!("trace vec by gc object");
        if Self::trace_vec(vec) {
            for value in vec.iter() {
                Self::mark_object(gray_list, value.ptr());
            }
        }
    }

    fn trace_vec<T>(vec: &OxVec<T>) -> bool {
        // println!("trace vec");
        if vec.is_empty() {
            return true;
        }
        let mut address = vec.ptr();
        let cell = address.cell_mut();
        if cell.marked {
            false
        } else {
            cell.mark(true);
            true
        }
    }
}
