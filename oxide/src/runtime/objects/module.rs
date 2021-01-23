use std::{fmt::Display, mem::ManuallyDrop};

use crate::{OxFunction, gc::{Cell, ObjectKind}, value::Object};
use crate::{
    gc::{Allocator, Gc, GcObject},
    OxString
};

#[derive(Debug, Clone)]
pub struct OxModule {
    cell: Cell,
    name: OxString,
    code: Gc<OxFunction>,
    objects: ManuallyDrop<Vec<Object, Allocator>>,
}

impl OxModule {
    pub fn new(name: OxString, code: Gc<OxFunction>, objects: Vec<Object, Allocator>) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Module),
            name,
            code,
            objects: ManuallyDrop::new(objects),
        }
    }

    pub fn code(&self) -> Gc<OxFunction> {
        self.code
    }

    pub fn objects(&self) -> &[Object] {
        self.objects.as_slice()
    }

    pub fn disassemble(&self) {
        println!("Module: {}", self.name);
        for object in self.objects.as_slice() {
            object.disassemble();
            println!();
        }
        
        self.code.disassemble();
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
