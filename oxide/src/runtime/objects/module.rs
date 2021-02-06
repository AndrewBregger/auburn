use std::{fmt::Display, mem::ManuallyDrop};

use crate::{
    gc::{Allocator, Gc, GcObject},
    OxString,
};
use crate::{
    gc::{Cell, ObjectKind},
    value::Object,
    OxFunction,
};

#[derive(Debug, Clone)]
pub struct OxModule {
    /// gc info
    cell: Cell,
    /// name of module
    name: OxString,
    /// entry object index
    entry: Option<usize>,
    /// list of objects
    objects: ManuallyDrop<Vec<Object, Allocator>>,
}

impl OxModule {
    pub fn new(name: OxString, code: Gc<OxFunction>, objects: Vec<Object, Allocator>) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Module),
            name,
            entry: None,
            objects: ManuallyDrop::new(objects),
        }
    }

    pub fn name(&self) -> &OxString {
        &self.name 
    }

    pub fn objects(&self) -> &[Object] {
        self.objects.as_slice()
    }

    pub fn set_entry(&mut self, entry: usize) {
        self.entry = Some(entry);
    }

    pub fn disassemble(&self) {
        println!(
            "Module: {}, objects: {} entry: {}",
            self.name,
            self.objects.len(),
            self.entry.map_or("None".to_string(), |e| format!("{}", e))
        );
        for (idx, object) in self.objects.iter().enumerate() {
            object.disassemble();
            println!();
        }
    }

    pub fn get_entry(&self) -> Option<Gc<OxFunction>> {
        if let Some(idx) = self.entry {
            match &self.objects[idx] {
                Object::Function(function) => Some(function.clone()),
                _ => panic!("Compiler Error: entry for module '{}' is not a function", self.name),
            }
        }
        else { None }
    }
}

impl GcObject for OxModule {
    fn as_cell(&self) -> &Cell {
        &self.cell
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        &mut self.cell
    }
}

impl Display for OxModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<module {}>", self.name)
    }
}
