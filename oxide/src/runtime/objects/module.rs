use std::{fmt::Display, mem::ManuallyDrop};

use crate::{AttributeAccess, OxString, Value, gc::{Allocator, Gc, GcObject}};
use crate::{
    gc::{Cell, ObjectKind},
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
    /// list of values
    values: ManuallyDrop<Vec<Value, Allocator>>,
}

impl OxModule {
    pub fn new(name: OxString, values: Vec<Value, Allocator>) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Module),
            name,
            entry: None,
            values: ManuallyDrop::new(values),
        }
    }

    pub fn name(&self) -> &OxString {
        &self.name 
    }

    pub fn values(&self) -> &[Value] {
        self.values.as_slice()
    }

    pub fn set_entry(&mut self, entry: usize) {
        self.entry = Some(entry);
    }

    pub fn disassemble(&self) {
        println!(
            "Module: {}, Values: {} Entry: {}",
            self.name,
            self.values.len(),
            self.entry.map_or("None".to_string(), |e| format!("{}", e))
        );
        for (idx, object) in self.values.iter().enumerate() {
            object.disassemble();
            println!();
        }
    }

    pub fn get_entry(&self) -> Option<Gc<OxFunction>> {
        if let Some(idx) = self.entry {
            match self.get_attr(idx) {
                Value::Function(function) => Some(function.clone()),
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

impl AttributeAccess for OxModule {
    type Output = Value;

    fn get_attr(&self, idx: usize) -> &<Self as AttributeAccess>::Output {
        unsafe { self.values.get_unchecked(idx) }
    }

    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output {
        unsafe { self.values.get_unchecked_mut(idx) }
    }

}

