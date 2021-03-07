mod call_frame;
mod op_codes;

use std::alloc::Layout;

use crate::{
    AttributeAccess,
    gc::{Allocator, Gc, VecAllocator, Address, Cell, GcAlloc, GcObject, ObjectKind},
    runtime,
    OxInstance, OxModule, OxString, OxTuple, Section, Value, VecBuffer,
    mem::read_to,
};
use call_frame::CallFrame;
pub use op_codes::{Instruction, OpCode};
use ordered_float::OrderedFloat;
use runtime::{OxFunction, OxStruct};

static DEFAULT_MEM_SIZE: usize = 2056;
static DEFAULT_STACK_SIZE: usize = 2056;
static DEFAULT_GRAY_LIST_SIZE: usize = 32;

type ObjectPtr = *mut Cell;

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

    pub fn memory_usage(&self) -> usize {
        self.allocator.memory_usage()
    }

    pub fn allocator(&self) -> Allocator {
        self.allocator.allocator()
    }

    pub fn allocator_vec(&self) -> VecAllocator {
        self.allocator.allocator_vec()
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

    fn call_value(&mut self, value: &Value, arity: u8) {
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
                self.print_stack();
                let call_frame = CallFrame::new(*funct, stack_start);
                self.push_frame(call_frame);
            }
            _ => {
                panic!("Unable to call non function object: {}", value);
            }
        }
    }

    fn call(&mut self) {
        let arity = {
            let frame = self.frame_mut();
            let arity = frame.section().read(frame.ip);
            frame.ip += 1;
            arity
        };

        let function = self.peek(arity as usize).clone();
        self.call_value(&function, arity);

    }

    pub fn run_module(&mut self, module: Gc<OxModule>) -> Result<(), runtime::Error> {
        self.push_stack(Value::from(module.clone()));
        if let Some(entry_function) = module.get_entry() {
            self.push_stack(entry_function.clone());
            self.call_value(&entry_function, 0);

            self.run()
        }
        else {
            Err(runtime::Error::missing_module_entry(module.as_ref().name().clone()))
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
            // println!("OpCode {:014x} {}", self.frame().ip - 1, op_code);
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
                    let fields = read_to::<u16>(frame.section().data(), &mut ip);
                    frame.ip = ip;
                    let instance = self.new_instance(fields);
                    let value = Value::Instance(instance);
                    self.push_stack(value);
                }
                OpCode::NewTuple => {
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let fields = read_to::<u16>(frame.section().data(), &mut ip);
                    frame.ip = ip;
                    let instance = self.new_tuple(fields);
                    let value = Value::Tuple(instance);
                    self.push_stack(value);
                }
                OpCode::InstanceAttr => {
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let idx = read_to::<u16>(frame.section().data(), &mut ip);
                    frame.ip = ip;
                    let value = self.pop();
                    debug_assert!(value.is_instance()); 
                    let instance = value.as_instance();
                    debug_assert!(idx < instance.len());

                    self.push_stack(instance.get_attr(idx as usize).clone());
                }
                OpCode::TupleAttr => {
                    let frame = self.frame_mut();
                    let mut ip = frame.ip;
                    let index = read_to::<u16>(frame.section().data(), &mut ip) as usize;
                    frame.ip = ip;
                    let value = self.pop();
                    debug_assert!(value.is_tuple());

                    let tuple = value.as_tuple();
                    debug_assert!(index < tuple.len(), "invalid tuple index");
                    self.push_stack(tuple.get_attr(index).clone());

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
                OpCode::Call => self.call(),
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
    
    #[inline(always)]
    fn mark_object(gray_list: &mut VecBuffer<ObjectPtr>, obj: ObjectPtr) {
        let cell = unsafe { &mut (*obj) };
        
        // if already marked then return
        if cell.marked {
            println!("\tAlready Marked {:?}", obj);
            return;
        }

        cell.mark(true);
        gray_list.push(obj);
        println!("--- Marking Object {:?} ---", obj);
    }

    #[inline(always)]
    fn mark_value(gray_list: &mut VecBuffer<ObjectPtr>, value: &Value) {
        match value.addr() {
            Some(addr) => {
                let b = addr.as_ptr_mut() as ObjectPtr;
                Self::mark_object(gray_list, b);
            }
            None => {}
        }
    }

    // allocation interface
    pub fn collect<'a>(&'a mut self) {
        return;
        println!("--- Begin Gc pass ---");
        let mut gray_list = VecBuffer::<ObjectPtr>::empty(self.allocator_vec());
        gray_list.reserve_exact(DEFAULT_GRAY_LIST_SIZE);

        self.mark_roots(&mut gray_list);

        self.mark_references(&mut gray_list);

        self.sweep();

        println!("--- End Gc pass ---");
    }

    fn mark_roots<'a>(&'a mut self, gray_list: &mut VecBuffer<ObjectPtr>) {
        println!("--- Begin Mark Roots ---");
        for idx in 0..self.top_stack {
            let value = &self.stack[idx];
            Self::mark_value(gray_list, value);
        }

        // are there any compiler roots that need to be marked?
        println!("--- Marked Roots Count: ");// {} ---", gray_list.len());
    }

    fn mark_references<'a>(&'a mut self, gray_list: &mut VecBuffer<ObjectPtr>) {
        println!("--- Begin Mark references ---");

        while !gray_list.is_empty() {
            if let Some(value) = gray_list.pop() {
                self.blacken_object(gray_list, value);
            }
            else { break };
        }

        println!("--- End Mark references ---");
    }

    fn blacken_object<'a>(&mut self, gray_list: &mut VecBuffer<ObjectPtr>, obj: ObjectPtr) {
        println!("--- Begin Blacked Object {:?}---", obj);
        let cell = unsafe { &*obj};
        let kind = cell.kind.clone();

        match kind {
            ObjectKind::Function => {
                println!("\tFunction");
                let function = unsafe { &mut *(obj as *mut OxFunction) };
                let name = function.name.as_cell_mut() as ObjectPtr;
                Self::mark_object(gray_list, name);

                let ptr = function.section.as_cell_mut() as ObjectPtr;
                Self::mark_object(gray_list, ptr);
            }
            ObjectKind::String => {
                println!("\tString");
                let string = unsafe {&mut *(obj as *mut OxString) };
                Self::mark_object(gray_list, string.buffer.as_cell_mut() as ObjectPtr);
            }

            ObjectKind::Struct => {
                println!("\tStruct");
                let structure = unsafe { &mut *(obj as *mut OxStruct) };
                Self::mark_object(gray_list, structure.name.as_cell_mut() as ObjectPtr);
                Self::mark_object(gray_list, structure.methods.as_cell_mut() as ObjectPtr);

            }
            ObjectKind::Instance => {
                println!("\tInstance");
                let instance = unsafe { &mut *(obj as *mut OxInstance) };
                Self::mark_object(gray_list, instance.object.as_cell_mut() as ObjectPtr);
                let fields = instance.fields_ptr_mut();
                for idx in 0..instance.fields {
                    let value = unsafe { &*fields.add(idx as usize) };
                    Self::mark_value(gray_list, value);
                }
            }
            ObjectKind::Module => {
                println!("\tModule");
                let module = unsafe { &mut *(obj as *mut OxModule) };
                Self::mark_object(gray_list, module.name.as_cell_mut() as ObjectPtr);
                for value in module.values.iter() {
                    Self::mark_value(gray_list, value);
                }
            }
            ObjectKind::Tuple => {
                println!("\tTuple");
                let tuple = unsafe { &mut *(obj as *mut OxTuple) };
                for value in tuple.elements.iter() {
                    Self::mark_value(gray_list, value);
                }
            }
            ObjectKind::Section => {
                println!("\tSection");
                let section = unsafe { &mut *(obj as *mut Section) };
                Self::mark_object(gray_list, section.data.as_cell_mut() as ObjectPtr);
                Self::mark_object(gray_list, section.constants.as_cell_mut() as ObjectPtr);
                Self::mark_object(gray_list, section.globals.as_cell_mut() as ObjectPtr);
            }
            
            // no children
            ObjectKind::VecBuffer => {
                println!("\tVecBuffer");
            }
            ObjectKind::Buffer => {
                println!("\tBuffer");
            }
        }
        println!("--- End Blacked Object ---");
    }

    fn sweep<'a>(&'a mut self) {
        self.allocator.sweep();
    }

    pub fn free(&mut self) {
        self.allocator.clean_up();
    }

    fn allocate(&mut self, layout: Layout) -> Address {
        if self.allocator.should_collect() {
            self.collect();
        }

        match self.allocator.alloc(layout) {
            Some(address) => address,
            None => {
                self.collect();
                self.allocator.alloc(layout).unwrap()
            }
        }
    }

    fn allocate_from<Ty: GcObject>(&mut self) -> Address {
        let layout = Layout::new::<Ty>();
        self.allocate(layout)
    }

    pub fn deallocate(&mut self, address: Address) {
        self.allocator.dealloc(address)
    }
    
    pub fn new_instance(&mut self, fields: u16) -> Gc<OxInstance> {
        let instance_address = self.allocate_instance(fields);
        let instance = unsafe {
            let ptr = instance_address.as_ptr() as *mut OxInstance;
            &mut *ptr
        };

        let fields_slice =
            unsafe { std::slice::from_raw_parts_mut(instance.fields_ptr_mut(), fields as usize) };
        (0..fields).rev().for_each(|idx| fields_slice[idx as usize] = self.pop());
        let name = self.pop().clone().as_struct().clone();
        *instance = OxInstance::new(name, fields);

        Gc::<OxInstance>::new(instance_address)
    }

    pub fn new_tuple(&mut self, elements_count: u16) -> Gc<OxTuple> {
        let instance_address = self.allocate_tuple();
        let instance = unsafe {
            let ptr = instance_address.as_ptr() as *mut OxTuple;
            &mut *ptr
        };

        let mut elements = self.allocate_vec(elements_count as usize);
        unsafe { elements.set_len(elements_count as usize) };
        (0..elements_count).rev().for_each(|idx| elements[idx as usize] = self.pop());
        *instance = OxTuple::new(elements);
        Gc::<OxTuple>::new(instance_address)

    }

    pub fn allocate_instance(&mut self, fields: u16) -> Address {
        let size =
            std::mem::size_of::<OxInstance>() + fields as usize * std::mem::size_of::<Value>();
        let layout = Layout::from_size_align(size, std::mem::align_of::<OxStruct>())
            .expect("failed to layout memory for struct");

        self.allocate(layout)
    }

    pub fn allocate_tuple(&mut self) -> Address {
        self.allocate_from::<OxTuple>()
    }

    pub fn allocate_vec_ptr<Ty: Sync + Send + Sized>(
        &mut self,
        count: usize,
    ) -> Gc<VecBuffer<Ty>> {
        let address = self.allocate_from::<VecBuffer<Ty>>();
        unsafe {
            let buffer = address.as_ptr_mut() as *mut VecBuffer<Ty>;
            *buffer = self.allocate_vec::<Ty>(count);
        }
        Gc::<VecBuffer<Ty>>::new(address)
    }

    pub fn allocate_vec<Ty: Sync + Send + Sized>(&mut self, count: usize) -> VecBuffer<Ty> {
        if self.allocator.should_collect() {
            self.collect();
            self.allocator.post_collect();
        }
        VecBuffer::<Ty>::new(Vec::with_capacity_in(count, self.allocator_vec()))
    }

    pub fn allocate_string(&mut self, value: &str) -> OxString {
        let array_buffer = self.allocate_vec::<char>(value.len());
        let mut ox_string = OxString::new(array_buffer, value.len());
        ox_string.set_from_str(value);
        ox_string
    }

    pub fn allocate_string_ptr(&mut self, value: &str) -> Gc<OxString> {
        let address = self.allocate_from::<OxString>();

        unsafe {
            let buffer = address.as_ptr_mut() as *mut OxString;
            *buffer = self.allocate_string(value);
        }

        Gc::<OxString>::new(address)
    }

    pub fn allocate_function(
        &mut self,
        name: OxString,
        arity: u8,
        section: Section,
    ) -> Gc<OxFunction> {
        let function_address = self.allocate_from::<OxFunction>();

        unsafe {
            let buffer = &mut *(function_address.as_ptr_mut() as *mut OxFunction);
            *buffer = OxFunction::new(name, arity, section);
        }

        Gc::<OxFunction>::new(function_address)
    }

    pub fn allocate_struct(&mut self, name: OxString, methods: VecBuffer<Value>) -> Gc<OxStruct> {
        let struct_address = self.allocate_from::<OxStruct>();

        unsafe {
            let buffer = &mut *(struct_address.as_ptr_mut() as *mut OxStruct);
            *buffer = OxStruct::new(name, methods);
        }

        Gc::<OxStruct>::new(struct_address)
    }

    pub fn allocate_module(
        &mut self,
        name: OxString,
        elements: VecBuffer<Value>,
    ) -> Gc<OxModule> {
        let module_address = self.allocate_from::<OxModule>();

        unsafe {
            let buffer = &mut *(module_address.as_ptr_mut() as *mut OxModule);
            *buffer = OxModule::new(name, elements);
        }

        Gc::<OxModule>::new(module_address)
    }
}
