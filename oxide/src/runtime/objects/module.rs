use std::{fmt::Display, ops::{Deref, DerefMut}};
use crate::{AttributeAccess, OxString, Value, VecBuffer, gc:: GcObject};
use crate::gc::{Cell, ObjectKind};

#[repr(C)]
#[derive(Debug, Clone)]
pub struct OxModule {
    /// gc info
    cell: Cell,
    /// name of module
    pub name: OxString,
    /// entry object index
    entry: Option<usize>,
    /// list of values
    pub values: VecBuffer<Value>,
}

impl OxModule {
    pub fn new(name: OxString, values: VecBuffer<Value>) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Module),
            name,
            entry: None,
            values,
        }
    }

    pub fn name(&self) -> &OxString {
        &self.name 
    }

    pub fn values(&self) -> &[Value] {
        self.values.deref().as_slice()
    }

    pub fn set_entry(&mut self, entry: usize) {
        self.entry = Some(entry);
    }

    pub fn disassemble(&self) {
        println!(
            "Module: {}, Values: {} Entry: {}",
            self.name,
            self.values.deref().len(),
            self.entry.map_or("None".to_string(), |e| format!("{}", e))
        );
        for (idx, object) in self.values.deref().iter().enumerate() {
            object.disassemble();
            println!();
        }
    }

    pub fn get_entry(&self) -> Option<Value> {
        if let Some(idx) = self.entry {
            let value = self.get_attr(idx);
            debug_assert!(value.is_function(), "Compiler Error: entry for module '{}' is not a function", self.name);
            Some(value.clone())
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
        unsafe { self.values.deref().get_unchecked(idx) }
    }

    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output {
        unsafe { self.values.deref_mut().get_unchecked_mut(idx) }
    }

}

