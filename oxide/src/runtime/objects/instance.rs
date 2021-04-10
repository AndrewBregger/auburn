use std::fmt::Display;

use crate::{
    gc::{Gc, Object, ObjectKind},
    AttributeAccess, OxString, OxStruct, OxVec, Value,
};

#[derive(Debug, Clone)]
pub struct OxInstance {
    object: Gc<OxStruct>,
    fields: OxVec<Value>,
}

impl OxInstance {
    pub fn new(object: Gc<OxStruct>, fields: OxVec<Value>) -> Self {
        Self { object, fields }
    }

    pub fn name(&self) -> &OxString {
        self.object.name()
    }

    pub fn object(&self) -> &OxStruct {
        self.object.as_ref()
    }

    pub fn fields(&self) -> &OxVec<Value> {
        &self.fields
    }

    pub fn fields_mut(&mut self) -> &mut OxVec<Value> {
        &mut self.fields
    }

    pub fn disassemble(&self, indent: usize) {
        println!("{}{}", (0..indent).map(|_| '\t').collect::<String>(), self);
    }
}

impl Display for OxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<instance {}>", self.name())
    }
}

impl Object for OxInstance {
    fn object_kind() -> ObjectKind {
        ObjectKind::Instance
    }
}

impl AttributeAccess for OxInstance {
    type Output = Value;

    fn get_attr(&self, idx: usize) -> &<Self as AttributeAccess>::Output {
        &self.fields[idx]
    }

    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output {
        &mut self.fields[idx]
    }
}
