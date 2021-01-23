use std::fmt::Display;

use crate::{
    gc::{Cell, Gc, GcObject, ObjectKind},
    OxString, Value,
};

#[derive(Debug, Clone)]
pub struct OxInstance {
    cell: Cell,
    name: OxString,
    fields: u16,
}

impl OxInstance {
    pub fn new(name: OxString, fields: u16) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Instance),
            name,
            fields,
        }
    }

    pub fn fields_ptr(&self) -> *const Value {
        unsafe {
            (self as *const OxInstance as *const u8).add(std::mem::size_of::<OxInstance>())
                as *const Value
        }
    }

    pub fn fields_ptr_mut(&mut self) -> *mut Value {
        unsafe {
            (self as *mut OxInstance as *mut u8).add(std::mem::size_of::<OxInstance>())
                as *mut Value
        }
    }
}

impl Display for OxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<instance {}>", self.name)
    }
}

impl GcObject for OxInstance {
    fn as_cell(&self) -> &Cell {
        &self.cell
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        &mut self.cell
    }
}
