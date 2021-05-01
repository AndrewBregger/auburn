use std::collections::HashMap;

use oxide::{gc::Gc, vm::OpCode, OxFunction, OxModule, OxStruct, Section, Value};

use crate::ir::hir::HirFile;

pub static SELF_GLOBAL_IDX: u8 = 0;

#[derive(Debug, Clone, Default)]
pub(crate) struct GlobalInfo {
    pub name: String,
    pub object_idx: usize,
}

impl GlobalInfo {
    pub fn new(name: String, object_idx: usize) -> Self {
        Self { name, object_idx }
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct LocalInfo {
    pub name: String,
    pub scope_level: usize,
    pub stack_idx: usize,
}

pub(crate) struct FunctionInfo {
    pub(crate) function: Gc<OxFunction>,
    pub(crate) global_map: HashMap<usize, u8>,
    pub(crate) locals: Vec<LocalInfo>,
}

impl FunctionInfo {
    pub fn new(function: Gc<OxFunction>) -> Self {
        Self {
            function,
            global_map: HashMap::new(),
            locals: vec![],
        }
    }

    pub fn section(&self) -> &Section {
        self.function.section()
    }

    pub fn section_mut(&mut self) -> &mut Section {
        self.function.section_mut()
    }

    pub fn look_up_local(&self, name: &str) -> Option<&LocalInfo> {
        for local_info in self.locals.iter().rev() {
            if local_info.name == name {
                return Some(local_info);
            }
        }
        None
    }
}

pub(crate) struct FileContext<'ctx> {
    #[allow(dead_code)]
    /// the this context is for.
    pub(crate) file: &'ctx HirFile,
    /// all of the global names for this file
    pub(crate) globals: HashMap<String, GlobalInfo>,
    /// current function: None if file scope
    pub(crate) current_function: Option<usize>,
    /// stack of function sections that are being generated
    pub(crate) function_stack: Vec<FunctionInfo>,
    /// module object for this file.
    pub(crate) module: Gc<OxModule>,
    /// structure currently being processed.
    /// None when processing a function
    /// Some when processing an associated function
    pub(crate) structure: Option<Gc<OxStruct>>,
}

impl<'ctx> FileContext<'ctx> {
    pub fn new(file: &'ctx HirFile, module: Gc<OxModule>) -> Self {
        Self {
            file,
            globals: HashMap::new(),
            current_function: None,
            function_stack: vec![],
            module,
            structure: None,
        }
    }

    pub fn push_function(&mut self, funct: Gc<OxFunction>) {
        self.current_function = Some(self.function_stack.len());
        self.function_stack.push(FunctionInfo::new(funct));
    }

    #[allow(dead_code)]
    pub fn pop_function(&mut self) {
        self.current_function.as_mut().map(|i| *i -= 1);
    }

    pub fn push_object(&mut self, value: Value) -> usize {
        self.module.add_object(value)
    }

    pub fn current_section(&self) -> &Section {
        if let Some(function) = self.current_function() {
            function.section()
        } else {
            panic!("unable to get section, not in function")
        }
    }

    pub fn current_section_mut(&mut self) -> &mut Section {
        if let Some(function) = self.current_function_mut() {
            function.section_mut()
        } else {
            panic!("unable to get section, not in function")
        }
    }

    pub fn current_function(&self) -> Option<&FunctionInfo> {
        self.current_function
            .map(|i| self.function_stack.get(i))
            .flatten()
    }

    pub fn current_function_mut(&mut self) -> Option<&mut FunctionInfo> {
        self.current_function
            .map(move |i| self.function_stack.get_mut(i))
            .flatten()
    }

    pub fn load_constant(&mut self, op: OpCode, value: Value) {
        let section = self.current_section_mut();
        let idx = section.add_constant(value);
        section.write_arg(op, idx);
    }

    pub fn load_global_in_function(&mut self, name: &str) -> u8 {
        // find the global by name.
        // panics if name doesnt exist:
        //  should be handled by the type checker.
        let global_idx = match self.globals.get(name) {
            Some(info) => info.object_idx,
            None => panic!(
                "attempting to load an object that is not a global: '{}'",
                name
            ),
        };

        // if self.values.len() <= global_idx {
        //     panic!(
        //         "{}: {} global value not found {}",
        //         name,
        //         global_idx,
        //         self.values.len()
        //     );
        // }

        let value = self
            .module
            .get(global_idx)
            .expect(
                format!(
                    "{}: {} is out of bounds for module of with objects {}",
                    name,
                    global_idx,
                    self.module.num_objects()
                )
                .as_str(),
            )
            .clone();

        // let value = self.values[global_idx].clone();
        if let Some(function) = self.current_function_mut() {
            if function.global_map.contains_key(&global_idx) {
                function.global_map[&global_idx]
            } else {
                let idx = function.section_mut().add_global();
                function.global_map.insert(global_idx, idx);
                function
                    .section_mut()
                    .set_global(idx as usize, value.clone());
                idx
            }
        } else {
            panic!()
        }
    }

    pub fn push_local(&mut self, name: &str, scope_level: usize) -> u8 {
        let function = self
            .current_function_mut()
            .expect("unable to get current function");
        let local_idx = function.locals.len();

        let local_info = LocalInfo {
            name: name.to_owned(),
            scope_level,
            stack_idx: local_idx,
        };

        function.locals.push(local_info);

        local_idx as u8
    }

    pub fn set_self_global(&mut self, value: Value) {
        assert!(
            self.globals.is_empty(),
            "there shouldn't be any globals loaded at this point"
        );

        self.globals.insert(
            "__SELF__".to_string(),
            GlobalInfo::new("self".to_string(), SELF_GLOBAL_IDX as usize),
        );

        let section = self.current_section_mut();
        let idx = section.add_global();
        assert_eq!(idx, SELF_GLOBAL_IDX);
        section.set_global(idx as usize, value);
    }
}
