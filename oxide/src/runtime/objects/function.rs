use std::fmt::{Display, Formatter};

use crate::{
    gc::{Object, ObjectKind},
    OxString, Section,
};

#[derive(Debug, Clone)]
pub struct OxFunction {
    name: OxString,
    section: Section,
    arity: u8,
}

impl OxFunction {
    pub fn new(name: OxString, section: Section, arity: u8) -> Self {
        Self {
            name,
            section,
            arity,
        }
    }

    pub fn name(&self) -> &OxString {
        &self.name
    }

    pub fn section(&self) -> &Section {
        &self.section
    }

    pub fn section_mut(&mut self) -> &mut Section {
        &mut self.section
    }

    pub fn arity(&self) -> u8 {
        self.arity
    }

    pub fn disassemble(&self) {
        println!("disassembly {}:", self.name);
        for inst in self.section.disassemble() {
            println!("{}", inst);
        }
    }
}

impl Display for OxFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl Object for OxFunction {
    fn object_kind() -> ObjectKind {
        ObjectKind::String
    }
}
