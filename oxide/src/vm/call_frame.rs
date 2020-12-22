use crate::{runtime::OxFunction, Section};

#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub function: *const OxFunction,
    pub local_start: usize,
    pub ip: usize,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            function: std::ptr::null() as *const OxFunction,
            local_start: 0,
            ip: 0,
        }
    }
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
        println!("getting function");
        unsafe { std::mem::transmute(self.function) }
    }

    pub fn funct_mut(&mut self) -> &mut OxFunction {
        println!("getting function mut");
        unsafe { std::mem::transmute(self.function) }
    }

    pub fn section(&self) -> &Section {
        unsafe { self.function.as_ref() }
            .expect("function pointer is null")
            .section()
    }
}
