use std::{fmt::Display, ops::Deref};

use crate::{
    gc::{Cell, GcObject, ObjectKind},
    OxString, Value, VecBuffer,
};

#[derive(Debug, Clone)]
pub struct OxStruct {
    cell: Cell,
    name: OxString,
    elements: VecBuffer<Value>,
}

impl OxStruct {
    pub fn new(name: OxString, elements: VecBuffer<Value>) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Struct),
            name,
            elements,
        }
    }

    pub fn disassemble(&self) {
        println!("<disassembly for {}>", self.name);

        for value in self.elements.deref().as_slice() {
            value.disassemble();
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
