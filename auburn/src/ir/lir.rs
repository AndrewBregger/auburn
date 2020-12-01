use crate::types::Type;
use oxide::vm::OpCode;
use oxide::Value;
use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct LirFile {
    blocks: Vec<LirBlock>,
    functions: BTreeMap<usize, LirFunction>,
}

#[derive(Debug, Clone)]
pub struct LirFunction {
    id: usize,
    /// name of the function
    name: String,
    /// type of this function
    ty: Rc<Type>,
    blocks: Vec<LirBlock>,
}

#[derive(Debug, Clone)]
pub struct LirBlock {
    name: LirLabelRef,
    level: usize,
    instructions: Vec<LirInstruction>,
    locals: HashMap<String, Local>,
}

impl LirBlock {
    pub fn new(name: LirLabelRef, level: usize) -> Self {
        Self {
            name,
            level,
            instructions: vec![],
            locals: HashMap::new(),
        }
    }

    pub fn push_instruction(&mut self, inst: LirInstruction) {
        self.instructions.push(inst);
    }

    pub fn add_local(&mut self, local: Local) {
        self.locals.insert(local.name.clone(), local);
    }
}
#[derive(Debug, Clone)]
pub struct Local {
    name: String,
    slot: usize,
    scope: usize,
}

#[derive(Debug, Clone)]
pub struct CallInfo {
    name: String,
    function_id: usize,
    result_used: bool,
    // module: ModuleId,
}

#[derive(Debug, Clone)]
pub enum LirInstruction {
    /// instructor for creating a new local in the current scope
    CreateLocal {
        name: String,
        init: bool,
    },

    SetLocal {
        name: String,
    },

    LoadLocal {
        name: String,
    },

    Add {
        ty: Rc<Type>,
    },

    Sub {
        ty: Rc<Type>,
    },

    Mult {
        ty: Rc<Type>,
    },

    Divide {
        ty: Rc<Type>,
    },

    Less {
        ty: Rc<Type>,
    },

    Greater {
        ty: Rc<Type>,
    },

    LessEq {
        ty: Rc<Type>,
    },

    GreaterEq {
        ty: Rc<Type>,
    },

    EqEq {
        ty: Rc<Type>,
    },

    NotEq {
        ty: Rc<Type>,
    },

    Return,

    LoadGlobal {
        name: String,
        slot: usize,
    },

    Constant(Value),

    BoolConstant(bool),

    StringConstant(String),

    SetGlobal {
        name: String,
        slot: usize,
    },

    Call(CallInfo),

    /// general jump, the type of the jmp specifies with type of jump to make
    Jmp {
        ty: OpCode,
        label: LirLabelRef,
    },
}

#[derive(Debug, Clone)]
pub struct LirLabel {
    label: String,
    location: usize,
}

pub type LirLabelRef = Rc<RefCell<LirLabel>>;

impl Display for LirLabel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.label)
    }
}

#[derive(Debug, Clone)]
pub struct LabelBuilder {
    label_prefix: String,
    next_label: RefCell<usize>,
}

impl LabelBuilder {
    pub fn new(prefix: &str) -> Self {
        Self {
            label_prefix: prefix.to_owned(),
            next_label: RefCell::new(0),
        }
    }

    pub fn next(&self) -> LirLabel {
        let label = format!("{}_{}", self.label_prefix, self.next_label.borrow());
        *self.next_label.borrow_mut() += 1;
        LirLabel::new(label)
    }
}

impl LirLabel {
    pub fn new(label: String) -> Self {
        Self { label, location: 0 }
    }

    pub fn set_location(&mut self, location: usize) {
        self.location = location;
    }

    pub fn label(&self) -> &str {
        self.label.as_str()
    }
}
