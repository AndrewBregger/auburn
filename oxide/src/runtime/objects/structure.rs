use std::fmt::Display;

use crate::{
    gc::{Cell, GcObject, ObjectKind},
    value::Object,
    OxString, Value, VecBuffer,
};

#[derive(Debug, Clone)]
pub struct OxStruct {
    cell: Cell,
    name: OxString,
    elements: VecBuffer<Object>,
}

impl OxStruct {
    pub fn new(name: OxString, elements: VecBuffer<Object>) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Struct),
            name,
            elements,
        }
    }

    pub fn disassemble(&self) {
        println!("<disassembly for {}>", self.name);
        
        for object in self.elements.as_slice() {
            object.disassemble();
            println!();
        }

    }
}

impl Display for OxStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<struct {}>", self.name)
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
