use std::{
    collections::HashMap,
    fmt::Display,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use oxide::Value;

use crate::ir::ast::{BinaryOp, UnaryOp};
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct LocalInfo {}

#[derive(Debug, Clone)]
pub struct GlobalInfo {}

#[derive(Debug, Clone)]
pub struct FunctionInfo {}



#[derive(Debug, Clone)]
pub enum SymbolInfo {
    Local(LocalInfo),
    Global(GlobalInfo),
    Function(FunctionInfo),
}

#[derive(Debug, Clone)]
pub struct Symbol {
    name: String,
    info: SymbolInfo,
}

impl Symbol {
    pub fn new(name: String, info: SymbolInfo) -> Self {
        Self {
            name,
            info
        }
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn info(&self) -> &SymbolInfo {
        &self.info
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeId(usize);

impl ScopeId {
    fn next() -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(0);
        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    next: ScopeId,
    children: Vec<Rc<Scope>>,
    elements: HashMap<String, Symbol>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionId(String, usize);

impl FunctionId {
    fn new(name: String) -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(0);
        Self(name, TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

impl Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.0, self.1)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    id: FunctionId,
    arity: usize,
    scope: Scope,
    body: Vec<BasicBlock>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockId(usize);

impl BlockId {
    fn next() -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(0);
        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    id: BlockId,
    instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub op: BinaryOp,
    pub op_type: Rc<Type>,
}

impl Binary {
    pub fn new(op: BinaryOp, op_type: Rc<Type>) -> Self {
        Self { op, op_type }
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub op_type: Rc<Type>,
}

impl Unary {
    pub fn new(op: UnaryOp, op_type: Rc<Type>) -> Self {
        Self { op, op_type }
    }
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub function: FunctionId,
    pub arity: usize,
}

#[derive(Debug, Clone)]
pub enum JumpOp {
    JmpTrue,
    JmpFalse,
    JmpLoop,
}

#[derive(Debug, Clone)]
pub struct Jump {
    pub op: JumpOp,
    pub dst: BlockId,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConstant(Value),
    Binary(Binary),
    Unary(Unary),
    FnCall(FnCall),
    Jump(Jump)
}
