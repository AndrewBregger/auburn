use std::{fmt::Display, ops::Deref};
use itertools::{self, Itertools};

use crate::{AttributeAccess, OxString, Value, VecBuffer, gc::{Cell, GcObject, ObjectKind}};

#[repr(C)]
#[derive(Debug, Clone)]
pub struct OxTuple {
    cell: Cell,
    pub elements: VecBuffer<Value>,
}

impl OxTuple {
    pub fn new(elements: VecBuffer<Value>) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Tuple),
            elements,
        }
    }


    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn disassemble(&self) {
        println!("<tuple {}>", self.elements.len());
    }
}

impl Display for OxTuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = self.elements.iter().map(|x| format!("{}", x)).collect_vec().join(", ");
        write!(f, "({})", value)
    }
}

impl GcObject for OxTuple {
    fn as_cell(&self) -> &Cell {
        &self.cell
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        &mut self.cell
    }
}


impl AttributeAccess for OxTuple {
    type Output = Value;

    fn get_attr(&self, idx: usize) -> &<Self as AttributeAccess>::Output {
        unsafe { self.elements.get_unchecked(idx) }
    }

    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output {
        unsafe { self.elements.get_unchecked_mut(idx) }
    }

}
