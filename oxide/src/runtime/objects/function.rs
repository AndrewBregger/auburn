use crate::{OxString, Section};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct OxFunction {
    // cell: Cell,
    name: Box<OxString>,
    arity: u8,
    section: Section,
}

impl OxFunction {
    pub fn new(name: Box<OxString>, arity: u8, section: Section) -> Self {
        Self {
            name,
            arity,
            section,
        }
    }

    pub fn is_script(&self) -> bool {
        self.name.str().is_empty()
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

    pub fn name(&self) -> &OxString {
        self.name.as_ref()
    }

    pub fn disassemble(&self) {
        let section = self.section();
        for value in section.globals().iter() {
            if value.is_function() {
                let fnct = value.as_function();
                fnct.disassemble();
            }
        }
        println!("disassembly for {}", self.name);
        for instruction in section.disassemble() {
            println!("\t{}", instruction);
        }
    }
}

impl Display for OxFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_script() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {} {:p}>", self.name, self as *const _)
        }
    }
}
