use crate::{runtime::OxFunction, Section};

pub struct CallFrame {
    pub function: *const OxFunction,
    pub local_start: usize,
    pub ip: usize,
}

impl CallFrame {
    pub fn new(function: *const OxFunction, local_start: usize) -> Self {
        Self {
            function,
            local_start,
            ip: 0,
        }
    }

    pub fn funct(&self) -> &OxFunction {
        unsafe { std::mem::transmute(self.function) }
    }

    pub fn funct_mut(&self) -> &mut OxFunction {
        unsafe { std::mem::transmute(self.function) }
    }

    pub fn section(&self) -> &Section {
        unsafe { self.function.as_ref() }
            .expect("function pointer is null")
            .section()
    }
}
