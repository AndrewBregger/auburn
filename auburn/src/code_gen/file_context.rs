use std::collections::HashMap;

use oxide::{Object, OxFunction, Section, Value, Vm, gc::Gc, vm::OpCode};

use crate::ir::hir::HirFile;

#[derive(Debug, Clone, Default)]
pub(crate) struct GlobalInfo {
    pub name: String,
    pub section_idx: u8,
    pub object_idx: usize,
}

impl GlobalInfo {
    pub fn new(name: String, section_idx: u8) -> Self {
        Self {
            name,
            section_idx,
            object_idx: 0,
        }
    }
}

pub(crate) struct FileContext<'ctx> {
    /// the this context is for.
    pub(crate) file: &'ctx HirFile,
    /// all of the global names for this file
    pub(crate) globals: HashMap<String, GlobalInfo>,
    // constants: HashMap<Value, u16>,
    /// section of code for the top level expressions.
    pub(crate) section: Section,
    /// stack of function sections that are being generated
    pub(crate) function_stack: Vec<Gc<OxFunction>>,
    /// list of objects for this file.
    pub(crate) objects: Vec<Object>
}

impl<'ctx> FileContext<'ctx> {
    pub fn new(file: &'ctx HirFile, vm: &Vm) -> Self {
        Self {
            file,
            globals: HashMap::new(),
            section: Section::new(vm),
            function_stack: vec![],
            objects: vec![],
        }
    }

    pub fn push_function(&mut self, funct: Gc<OxFunction>) {
        self.function_stack.push(funct);
    }

    pub fn pop_function(&mut self) -> Gc<OxFunction> {
        self.function_stack.pop().expect("FileContext::push and pop function not balenced")
    }

    pub fn push_object(&mut self, object: Object) -> usize {
        self.objects.push(object);
        self.objects.len() - 1
    }

    pub fn current_section(&self) -> &Section {
        if let Some(function) = self.function_stack.last() {
            function.section()
        }
        else {
            &self.section
        }
    }

    pub fn current_section_mut(&mut self) -> &mut Section {
        if let Some(function) = self.function_stack.last_mut() {
            function.section_mut()
        }
        else {
            &mut self.section
        }
    }

    pub fn load_constant(&mut self, op: OpCode, value: Value) {
        let section = self.current_section_mut();
        let idx = section.add_constant(value);
        section.write_arg(op, idx);
    }
}
