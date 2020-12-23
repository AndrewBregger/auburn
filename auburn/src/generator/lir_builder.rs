use crate::ir::ast::{BinaryOp, NodeType};
use crate::ir::hir::{HirExprKind, HirExprPtr, HirItemPtr, HirSpecPtr, HirStmtPtr, MirNode};
use crate::ir::lir::{LabelBuilder, LirBlock, LirInstruction, LirLabelRef};
use crate::oxide::{OxFunction, OxString};
use crate::types::TypeKind;
use oxide::vm::Instruction;
use oxide::Value;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

pub struct Context {
    blocks: Vec<LirBlock>,
    labels: HashMap<String, LirLabelRef>,
    level: usize,
    label_builder: LabelBuilder,
}

impl Context {
    pub fn new() -> Self {
        Self {
            blocks: vec![],
            labels: HashMap::new(),
            level: 0,
            label_builder: LabelBuilder::new("l"),
        }
    }

    fn new_label(&self) -> LirLabelRef {
        Rc::new(RefCell::new(self.label_builder.next()))
    }

    pub fn push_block(&mut self) {
        self.blocks
            .push(LirBlock::new(self.new_label(), self.level));
    }

    pub fn push_instruction(&mut self, inst: LirInstruction) {
        self.blocks
            .last_mut()
            .map(|block| block.push_instruction(inst));
    }
}

// pub enum LirResult {
//     Constant(usize),
//     Local(usize),
//     Global(usize),
// }

pub struct ByteCodeBuilder<'ctx> {
    ctx: &'ctx mut Context,
}

impl<'ctx> ByteCodeBuilder<'ctx> {
    pub fn new(ctx: &'ctx mut Context) -> Self {
        Self { ctx }
    }

    pub fn process_expr(&mut self, expr: HirExprPtr) {
        let inner = expr.inner();
        match inner.kind() {
            &HirExprKind::Integer(val) => match expr.ty().kind() {
                TypeKind::U8 => self
                    .push_instruction(LirInstruction::Constant(Value::U8(val.try_into().unwrap()))),
                TypeKind::U16 => self.push_instruction(LirInstruction::Constant(Value::U16(
                    val.try_into().unwrap(),
                ))),
                TypeKind::U64 => self.push_instruction(LirInstruction::Constant(Value::U64(
                    val.try_into().unwrap(),
                ))),
                TypeKind::I8 => self
                    .push_instruction(LirInstruction::Constant(Value::I8(val.try_into().unwrap()))),
                TypeKind::I16 => self.push_instruction(LirInstruction::Constant(Value::I16(
                    val.try_into().unwrap(),
                ))),
                TypeKind::I32 => self.push_instruction(LirInstruction::Constant(Value::I32(
                    val.try_into().unwrap(),
                ))),
                TypeKind::I64 => {
                    self.push_instruction(LirInstruction::Constant(Value::I64(val.into())))
                }
                TypeKind::U32 | TypeKind::Integer => self.push_instruction(
                    LirInstruction::Constant(Value::U32(val.try_into().unwrap())),
                ),
                _ => unreachable!(),
            },
            HirExprKind::Float(val) => match expr.ty().kind() {
                TypeKind::F32 => {
                    self.push_instruction(LirInstruction::Constant(Value::F32(val.0 as f32)))
                }
                TypeKind::F64 => self.push_instruction(LirInstruction::Constant(Value::F64(val.0))),
                TypeKind::Float => {
                    self.push_instruction(LirInstruction::Constant(Value::F32(val.0 as f32)))
                }
                _ => {}
            },
            HirExprKind::String(val) => {
                self.push_instruction(LirInstruction::StringConstant(val.to_owned()))
            }
            HirExprKind::Char(_) => {}
            HirExprKind::Bool(val) => self.push_instruction(LirInstruction::BoolConstant(*val)),
            HirExprKind::Name(_) => {}
            HirExprKind::Binary(binary_expr) => {
                self.process_expr(binary_expr.left.clone());
                self.process_expr(binary_expr.right.clone());
                match binary_expr.op {
                    BinaryOp::Plus => self.push_instruction(LirInstruction::Add { ty: expr.ty() }),
                    BinaryOp::Minus => self.push_instruction(LirInstruction::Sub { ty: expr.ty() }),
                    BinaryOp::Astrick => {
                        self.push_instruction(LirInstruction::Mult { ty: expr.ty() })
                    }
                    BinaryOp::Slash => {
                        self.push_instruction(LirInstruction::Divide { ty: expr.ty() })
                    }
                    BinaryOp::Less => self.push_instruction(LirInstruction::Less { ty: expr.ty() }),
                    BinaryOp::Greater => {
                        self.push_instruction(LirInstruction::Greater { ty: expr.ty() })
                    }
                    BinaryOp::LessEq => {
                        self.push_instruction(LirInstruction::LessEq { ty: expr.ty() })
                    }
                    BinaryOp::GreaterEq => {
                        self.push_instruction(LirInstruction::GreaterEq { ty: expr.ty() })
                    }
                    // BinaryOp::Ampersand => self
                    //
                    //     .push_instruction(LirInstruction::Add { ty: expr.ty() }),
                    // BinaryOp::Pipe => self
                    //
                    //     .push_instruction(LirInstruction::Add { ty: expr.ty() }),
                    // BinaryOp::Percent => self
                    //
                    //     .push_instruction(LirInstruction::Add { ty: expr.ty() }),
                    BinaryOp::EqualEqual => {
                        self.push_instruction(LirInstruction::EqEq { ty: expr.ty() })
                    }
                    BinaryOp::BangEqual => {
                        self.push_instruction(LirInstruction::NotEq { ty: expr.ty() })
                    }
                    _ => {} // BinaryOp::LessLess =>
                            // BinaryOp::GreaterGreater =>
                }
            }
            HirExprKind::Unary(_) => {}
            HirExprKind::Field(_) => {}
            HirExprKind::Index(_) => {}
            HirExprKind::FieldAccess(_) => {}
            HirExprKind::Call(_) => {}
            HirExprKind::Method(_) => {}
            HirExprKind::AssociatedFunction(_) => {}
            HirExprKind::Block(block_expr) => {}
            HirExprKind::Tuple(_) => {}
            HirExprKind::Loop(_) => {}
            HirExprKind::While(_) => {}
            HirExprKind::For(_) => {}
            HirExprKind::If(_) => {}
            HirExprKind::StructExpr(_) => {}
            HirExprKind::SelfLit => {}
            HirExprKind::Break => {}
            HirExprKind::Continue => {}
            HirExprKind::Return(_) => {}
        }
    }
    pub fn process_item(&mut self, expr: HirItemPtr) {}
    pub fn process_spec(&mut self, expr: HirSpecPtr) {}
    pub fn process_stmt(&mut self, expr: HirStmtPtr) {}
}
