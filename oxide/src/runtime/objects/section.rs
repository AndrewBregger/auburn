use crate::{
    disassembler::Disassembler,
    gc::{Object, ObjectKind, VecAllocator},
    vm::{Instruction, OpCode},
    OxVec, Value, Vm,
};

use std::{
    fmt::{Display, Formatter},
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct SectionId(usize);

impl SectionId {
    pub fn next() -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(1);
        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

impl Display for SectionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// section of executable byte code.
#[derive(Debug, Clone)]
pub struct Section {
    /// unique identification of this executable code
    id: SectionId,
    /// constant values known at compile time
    constants: OxVec<Value>,
    /// global values populated at compiler or just before runtime.
    globals: OxVec<Value>,
    /// raw opcode data
    data: OxVec<u8>,
}

impl Section {
    // #[cfg(debug_assertions)]
    // pub fn new(allocator: VecAllocator, vm: &Vm) -> Self {
    //     let constants = OxVec::new(allocator.clone());
    //     println!("constants {}", vm.allocator().last_record().unwrap());

    //     let globals = OxVec::new(allocator.clone());
    //     println!("globals {}", vm.allocator().last_record().unwrap());

    //     let data = OxVec::new(allocator.clone());
    //     println!("data {}", vm.allocator().last_record().unwrap());

    //     Self {
    //         id: SectionId::next(),
    //         constants,
    //         globals,
    //         data,
    //     }
    // }

    // #[cfg(not(debug_assertions))]
    pub fn new(allocator: VecAllocator, _: &Vm) -> Self {
        let constants = OxVec::new(allocator.clone());
        let globals = OxVec::new(allocator.clone());
        let data = OxVec::new(allocator.clone());

        Self {
            id: SectionId::next(),
            constants,
            globals,
            data,
        }
    }

    pub fn get_constant(&self, index: usize) -> Value {
        self.constants[index].clone()
    }

    pub fn get_global(&self, index: usize) -> Value {
        self.globals[index].clone()
    }

    pub fn set_global(&mut self, index: usize, value: Value) {
        self.globals[index] = value;
    }

    pub fn id(&self) -> SectionId {
        self.id
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn read(&self, index: usize) -> u8 {
        self.data[index]
    }

    pub fn data(&self) -> &[u8] {
        self.data.as_slice()
    }

    pub fn debug_print(&self) {
        // println!("{:#?}", self.data);
    }

    pub fn disassemble(&self) -> Vec<Instruction> {
        Disassembler::disassemble_section(self)
    }

    pub fn globals(&self) -> &[Value] {
        self.globals.as_slice()
    }

    pub fn constants(&self) -> &[Value] {
        self.constants.as_slice()
    }
}

impl Section {
    /// adds constant value to constants block
    pub fn add_constant(&mut self, value: Value) -> u8 {
        let index = self.constants.len();
        self.constants.push(value);
        index as _
    }

    /// allocates a new global, sets it to unit
    pub fn add_global(&mut self) -> u8 {
        let index = self.globals.len();
        self.globals.push(Value::Unit);
        index as _
    }

    pub fn write_byte(&mut self, byte: u8) {
        self.data.push(byte);
    }

    pub fn write_arg(&mut self, op: OpCode, index: u8) {
        self.write_op(op);
        self.write_byte(index);
    }

    pub fn write_op(&mut self, op: OpCode) {
        self.data.push(op as u8);
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.data.extend_from_slice(bytes)
    }

    // returns the first byte of the jmp operand.
    pub fn write_jmp(&mut self, op: OpCode) -> usize {
        self.write_op(op);
        self.write_bytes(&[0xff, 0xff]);
        self.len() - 2
    }

    pub fn patch_jmp(&mut self, offset: usize) {
        dbg!("patch_jmp: {} {}", self.len(), offset);
        let jump: u16 = (self.len() - offset - 2) as _;
        self.data[offset] = ((jump >> 8) & 0xff) as u8;
        self.data[offset + 1] = (jump & 0xff) as u8;
    }

    pub fn write_loop(&mut self, start: usize) {
        self.write_op(OpCode::Loop);
        let offset = (self.len() - start + 2) as u16;
        self.write_bytes(&offset.to_be_bytes());
    }
}

impl Object for Section {
    fn object_kind() -> ObjectKind {
        ObjectKind::Section
    }
}
