use crate::{gc::Gc, OxString, Section};

#[derive(Debug, Clone, Copy)]
pub struct OxModule {
    name: Gc<OxString>,
    code: Section,
}

impl OxModule {
    pub fn new(name: Gc<OxString>, code: Section) -> Self {
        Self { name, code }
    }
}
