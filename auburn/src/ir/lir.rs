use std::{
    collections::HashMap,
    fmt::Display,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::ir::ast::{BinaryOp, UnaryOp};
use crate::types::Type;

pub struct ScopeId(usize);

impl ScopeId {
    fn next() -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(0);
        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

pub struct Scope {
    next: ScopeId,
    children: Vec<Rc<Scope>>,
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

pub struct Function {
    id: FunctionId,
    arity: usize,
    scope: Scope,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockId(usize);

impl BlockId {
    fn next() -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(0);
        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

pub struct LirBasicBlock {
    id: BlockId,
    instructions: Vec<Instruction>,
}

pub struct Binary {
    pub op: BinaryOp,
    pub op_type: Rc<Type>,
}

impl Binary {
    pub fn new(op: BinaryOp, op_type: Rc<Type>) -> Self {
        Self { op, op_type }
    }
}

pub struct Unary {
    pub op: UnaryOp,
    pub op_type: Rc<Type>,
}

impl Unary {
    pub fn new(op: UnaryOp, op_type: Rc<Type>) -> Self {
        Self { op, op_type }
    }
}

pub struct FnCall {
    pub function: FunctionId,
    pub arity: usize,
}

pub enum Instruction {
    Binary(Binary),
    Unary(Unary),
    FnCall(FnCall),
}
