use std::fmt::Display;

use itertools::Itertools;

use crate::{
    gc::{Object, ObjectKind},
    AttributeAccess, OxVec, Value,
};

#[derive(Debug, Clone)]
pub struct OxTuple {
    elements: OxVec<Value>,
}

impl OxTuple {
    pub fn new(elements: OxVec<Value>) -> Self {
        Self { elements }
    }

    pub fn elements(&self) -> &OxVec<Value> {
        &self.elements
    }

    pub fn disassemble(&self, indent: usize) {
        println!(
            "{}<tuple {}>",
            (0..indent).map(|_| '\t').collect::<String>(),
            self
        );
    }
}

impl Display for OxTuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.elements.iter().map(Value::to_string).join(", ")
        )
    }
}

impl Object for OxTuple {
    fn object_kind() -> ObjectKind {
        ObjectKind::String
    }
}

impl AttributeAccess for OxTuple {
    type Output = Value;

    fn get_attr(&self, idx: usize) -> &<Self as AttributeAccess>::Output {
        &self.elements[idx]
    }

    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output {
        &mut self.elements[idx]
    }
}
