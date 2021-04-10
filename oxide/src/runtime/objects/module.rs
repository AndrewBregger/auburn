use std::fmt::Display;

use crate::{
    gc::{Object, ObjectKind, VecAllocator},
    OxString, OxVec, Value,
};

#[derive(Debug, Clone)]
pub struct OxModule {
    name: OxString,
    entry: Option<usize>,
    objects: OxVec<Value>,
}

impl OxModule {
    pub fn new(name: OxString, entry: Option<usize>, objects: OxVec<Value>) -> Self {
        Self {
            name,
            entry,
            objects,
        }
    }

    pub fn empty(name: OxString, allocator: VecAllocator) -> Self {
        Self::new(name, None, OxVec::new(allocator))
    }

    pub fn name(&self) -> &OxString {
        &self.name
    }

    pub fn disassemble(&self, indent: usize) {
        println!(
            "{}disassemble {}:",
            (0..indent).map(|_| '\t').collect::<String>(),
            self.name
        );
        for obj in self.objects.iter() {
            obj.disassemble(indent + 1);
        }
    }

    pub fn objects(&self) -> &OxVec<Value> {
        &self.objects
    }

    pub fn entry(&self) -> Option<&Value> {
        self.entry.map(|idx| self.objects.get(idx)).flatten()
    }

    pub fn set_entry(&mut self, entry: usize) {
        self.entry = Some(entry);
    }
}

impl Display for OxModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<module {}>", self.name)
    }
}

impl Object for OxModule {
    fn object_kind() -> ObjectKind {
        ObjectKind::String
    }
}
