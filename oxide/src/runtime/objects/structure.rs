use std::fmt::Display;

use crate::{
    gc::{Cell, GcObject, ObjectKind},
    OxString, Value,
};

#[derive(Debug, Clone, Copy)]
pub struct OxStruct {
    cell: Cell,
}

impl OxStruct {
    pub fn new() -> Self {
        Self {
            cell: Cell::new(ObjectKind::Struct),
        }
    }

    pub fn fields_ptr(&self) -> *const Value {
        unsafe {
            (self as *const OxStruct as *const u8).add(std::mem::size_of::<Cell>()) as *const Value
        }
    }

    pub fn fields_ptr_mut(&mut self) -> *mut Value {
        unsafe { (self as *mut OxStruct as *mut u8).add(std::mem::size_of::<Cell>()) as *mut Value }
    }
}

impl Display for OxStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<struct instance>")
    }
}

impl GcObject for OxStruct {
    fn as_cell(&self) -> &Cell {
        &self.cell
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        &mut self.cell
    }
}
