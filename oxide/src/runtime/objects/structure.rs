use std::fmt::Display;

use crate::{
    gc::{Gc, Object, ObjectKind},
    AttributeAccess, OxFunction, OxString, OxVec,
};

#[derive(Debug, Clone)]
pub struct OxStruct {
    name: OxString,
    methods: OxVec<Gc<OxFunction>>,
}

impl OxStruct {
    pub fn new(name: OxString, methods: OxVec<Gc<OxFunction>>) -> Self {
        Self { name, methods }
    }

    pub fn name(&self) -> &OxString {
        &self.name
    }

    pub fn methods(&self) -> &OxVec<Gc<OxFunction>> {
        &self.methods
    }

    pub fn disassemble(&self, indent: usize) {
        println!(
            "{}<struct {}>",
            (0..indent).map(|_| '\t').collect::<String>(),
            self.name
        );
    }
}

impl Display for OxStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<struct {}>", self.name)
    }
}

impl Object for OxStruct {
    fn object_kind() -> ObjectKind {
        ObjectKind::String
    }
}

impl AttributeAccess for OxStruct {
    type Output = Gc<OxFunction>;

    fn get_attr(&self, idx: usize) -> &<Self as AttributeAccess>::Output {
        &self.methods[idx]
    }

    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output {
        &mut self.methods[idx]
    }
}
