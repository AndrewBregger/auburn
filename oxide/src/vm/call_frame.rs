use crate::{
    gc::{Address, Gc},
    runtime::OxFunction,
    Section,
};

#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub function: Gc<OxFunction>,
    pub local_start: usize,
    pub ip: usize,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            function: Gc::null(),
            local_start: 0,
            ip: 0,
        }
    }
}

impl CallFrame {
    pub fn new(function: Gc<OxFunction>, local_start: usize) -> Self {
        Self {
            function,
            local_start,
            ip: 0,
        }
    }

    pub fn funct(&self) -> Gc<OxFunction> {
        self.function
    }

    pub fn section(&self) -> &Section {
        assert_ne!(self.function.ptr(), Address::null());
        self.function.section()
    }

    pub fn read_byte(&self) -> u8 {
        self.section().read(self.ip)
    }
}
