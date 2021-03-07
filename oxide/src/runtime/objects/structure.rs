use std::{fmt::Display, ops::Deref};

use crate::{AttributeAccess, OxString, Value, VecBuffer, gc::{Cell, GcObject, ObjectKind}};

#[derive(Debug, Clone)]
pub struct OxAttribute {
    /// name of this attribute
    name: OxString,
    // this should refer to a global types table that stores all of the types in the program.
    // type_id: TypeId,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct OxStruct {
    cell: Cell,
    pub name: OxString,
    // is this needed
    // attributes: VecBuffer<OxAttribute>,
    pub methods: VecBuffer<Value>,
}

impl OxStruct {
    pub fn new(name: OxString, /*attributes: VecBuffer<OxAttribute>,*/ methods: VecBuffer<Value>) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Struct),
            name,
            //attributes,
            methods,
        }
    }

    pub fn name(&self) -> &OxString {
        &self.name
    }

    pub fn disassemble(&self) {
        println!("<disassembly for {}>", self.name);

        for value in self.methods.deref().as_slice() {
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

impl AttributeAccess for OxStruct {
    type Output = Value;

    fn get_attr(&self, idx: usize) -> &<Self as AttributeAccess>::Output {
        unsafe { self.methods.get_unchecked(idx) }
    }

    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output {
        unsafe { self.methods.get_unchecked_mut(idx) }
    }
}


