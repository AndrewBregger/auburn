use std::fmt::Debug;
use std::rc::Rc;

use ordered_float::OrderedFloat;

use crate::analysis::EntityRef;
use crate::ir::ast::{
    AssignmentOp, AstNodeType, BinaryOp, Identifier, NodeId, NodeType, UnaryOp, Visibility,
};
use crate::syntax::Position;
use crate::system::FileId;
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub left: Rc<HirExpr>,
    pub right: Rc<HirExpr>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub operand: Rc<HirExpr>,
}

#[derive(Debug, Clone)]
pub struct FieldExpr {
    pub operand: Rc<HirExpr>,
    pub field: EntityRef,
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub operand: HirExprPtr,
    pub index: HirExprPtr,
}

#[derive(Debug, Clone)]
pub struct FieldAccessExpr {}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub operand: Rc<HirExpr>,
    pub function_type: Rc<Type>,
    pub actuals: Vec<Rc<HirExpr>>,
}

#[derive(Debug, Clone)]
pub struct MethodExpr {
    pub function_type: Rc<Type>,
    pub name: String,
    pub actuals: Vec<Rc<HirExpr>>,
}

#[derive(Debug, Clone)]
pub struct AssociatedFunctionExpr {
    pub function_type: Rc<Type>,
    pub name: String,
    pub actuals: Vec<Rc<HirExpr>>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub stmts: Vec<Rc<HirStmt>>,
    pub function_block: bool,
}

#[derive(Debug, Clone)]
pub struct TupleExpr {
    pub elements: Vec<Rc<HirExpr>>,
}

#[derive(Debug, Clone)]
pub struct TupleIndex {
    pub tuple: Rc<HirExpr>,
    pub field: u64,
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
    pub body: Rc<HirExpr>,
}

#[derive(Debug, Clone)]
pub struct WhileExpr {
    pub cond: Rc<HirExpr>,
    pub body: Rc<HirExpr>,
}

#[derive(Debug, Clone)]
pub struct ForExpr {
    pub element: Identifier,
    pub expr: Rc<HirExpr>,
    pub body: Rc<HirExpr>,
}

#[derive(Debug, Clone)]
pub enum IfExprBranch {
    Conditional {
        cond: HirExprPtr,
        body: HirExprPtr,
        first: bool,
    },
    Unconditional {
        body: HirExprPtr,
    },
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub branches: Vec<IfExprBranch>,
}

#[derive(Debug, Clone)]
pub struct StructExpr {
    pub struct_type: Rc<Type>,
    pub fields: Vec<(usize, Rc<HirExpr>)>,
}

#[derive(Debug, Clone)]
pub enum HirExprKind {
    Integer(i64),
    Float(OrderedFloat<f64>),
    String(String),
    Char(char),
    Bool(bool),
    Name(EntityRef),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Field(FieldExpr),
    Index(IndexExpr),
    FieldAccess(FieldAccessExpr),
    Call(CallExpr),
    Method(MethodExpr),
    AssociatedFunction(AssociatedFunctionExpr),
    Block(BlockExpr),
    Tuple(TupleExpr),
    TupleIndex(TupleIndex),
    Loop(LoopExpr),
    While(WhileExpr),
    For(ForExpr),
    If(IfExpr),
    StructExpr(StructExpr),
    SelfLit,
    Break,
    Continue,
    Return(HirExprPtr),
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AddressMode {
    Value,
    Address,
    Error,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct ResultMeta {
    pub mutable: bool,
    pub inherited: bool,
    pub mutable_type: bool,
    pub rvalue: bool,
    pub is_type: bool,
    pub is_call: bool,
    pub uses_result: bool,
}

impl ResultMeta {
    pub fn new(
        mutable: bool,
        inherited: bool,
        mutable_type: bool,
        rvalue: bool,
        is_type: bool,
    ) -> Self {
        Self {
            mutable,
            inherited,
            mutable_type,
            rvalue,
            is_type,
            is_call: false,
            uses_result: false,
        }
    }

    pub fn funct(
        mutable: bool,
        inherited: bool,
        mutable_type: bool,
        rvalue: bool,
        is_type: bool,
        is_call: bool,
        uses_result: bool,
    ) -> Self {
        Self {
            mutable,
            inherited,
            mutable_type,
            rvalue,
            is_type,
            is_call,
            uses_result,
        }
    }

    pub fn literal() -> Self {
        Self::new(false, false, false, true, false)
    }
}

#[derive(Debug, Clone)]
pub struct HirExprInner {
    address_mode: AddressMode,
    meta: ResultMeta,
    kind: HirExprKind,
}

impl HirExprInner {
    pub fn new(address_mode: AddressMode, meta: ResultMeta, kind: HirExprKind) -> Self {
        Self {
            address_mode,
            meta,
            kind,
        }
    }

    pub fn kind(&self) -> &HirExprKind {
        &self.kind
    }

    pub fn address_mode(&self) -> AddressMode {
        self.address_mode
    }

    pub fn meta(&self) -> ResultMeta {
        self.meta
    }
}

impl NodeType for HirExprInner {
    fn name(&self) -> &'static str {
        self.kind.name()
    }

    fn ty(&self) -> AstNodeType {
        self.kind.ty()
    }
}

impl HirExprKind {
    pub fn is_self(&self) -> bool {
        match self {
            Self::SelfLit => true,
            _ => false,
        }
    }
}

impl NodeType for HirExprKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "Integer",
            Self::Float(_) => "Float",
            Self::String(_) => "String",
            Self::Char(_) => "Char",
            Self::Bool(_) => "Bool",
            Self::Name(_) => "Name",
            Self::Binary(..) => "Binary",
            Self::Unary(..) => "Unary",
            Self::Field(..) => "Field",
            Self::FieldAccess(..) => "Field Access",
            Self::Call { .. } => "Call",
            Self::Method { .. } => "Method",
            Self::AssociatedFunction(..) => "Associated Function",
            Self::Block(..) => "Block",
            Self::Tuple(..) => "Tuple",
            Self::TupleIndex(..) => "Tuple Index",
            Self::Loop(..) => "Loop",
            Self::While(..) => "While",
            Self::For { .. } => "For",
            Self::If { .. } => "If",
            Self::StructExpr { .. } => "Struct Expr",
            Self::SelfLit => "Self Literal",
            Self::Break => "Break",
            Self::Continue => "Continue",
            Self::Return(..) => "Return",
            Self::Index(..) => "Index",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Expr
    }
}

impl NodeType for HirStmtKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Expr(_) => "Expr Stmt",
            Self::Item(_) => "Item Stmt",
            Self::Assignment(_) => "Assignment Stmt",
            Self::Echo(_) => "Echo Stmt",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Stmt
    }
}

impl NodeType for HirItemKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Variable { .. } => "Variable",
            Self::Function { .. } => "Function",
            Self::Struct { .. } => "Struct",
            // Self::Param { .. } => "Param",
            // Self::Field { .. } => "Field",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Item
    }
}

impl NodeType for HirSpecKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Named => "Named",
            Self::Tuple => "Tuple",
            Self::Unit => "Unit",
            Self::SelfType => "Self",
            Self::Infer => "Infer",
            Self::Array => "Array",
            Self::Slice => "Slice",
            Self::Mutable => "Mutable",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Spec
    }
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub op: AssignmentOp,
    // pub lvalue: EntityRef,
    pub lvalue: Rc<HirExpr>,
    pub rhs: Rc<HirExpr>,
}

#[derive(Debug, Clone)]
pub enum HirStmtKind {
    Expr(Rc<HirExpr>),
    Item(EntityRef),
    Assignment(Assignment),
    Echo(HirExprPtr),
}

// the actual type will be the ty() of MirNode
#[derive(Debug, Clone)]
pub enum HirSpecKind {
    Named,
    Tuple,
    Unit,
    SelfType,
    Infer,
    Array,
    Slice,
    Mutable,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub vis: Visibility,
    pub mutable: bool,
    pub name: Identifier,
    pub init: Option<Rc<HirExpr>>,
    pub spec: Option<Rc<HirSpec>>,
}

#[derive(Debug, Clone)]
pub struct Structure {
    pub vis: Visibility,
    pub name: Identifier,
    pub fields: Vec<Rc<HirField>>,
    pub methods: Vec<Rc<HirItem>>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub vis: Visibility,
    pub name: Identifier,
    pub params: Vec<Rc<HirParam>>,
    pub ret: Rc<HirSpec>,
    pub body: Rc<HirExpr>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Identifier,
    pub spec: Option<Rc<HirSpec>>,
    pub init: Option<Rc<HirExpr>>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub vis: Visibility,
    pub name: Identifier,
    pub spec: Option<Rc<HirSpec>>,
    pub init: Option<Rc<HirExpr>>,
}

impl NodeType for Param {
    fn name(&self) -> &'static str {
        "param"
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Param
    }
}

impl NodeType for Field {
    fn name(&self) -> &'static str {
        "field"
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Field
    }
}

#[derive(Debug, Clone)]
pub enum HirItemKind {
    Variable(Variable),
    Struct(Structure),
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct HirNodeBase<Inner> {
    id: NodeId,
    position: Position,
    inner: Inner,
    ty: Rc<Type>,
}

pub trait MirNode {
    fn position(&self) -> Position;

    fn id(&self) -> NodeId;

    fn ty(&self) -> Rc<Type>;
}

impl<Inner: NodeType> HirNodeBase<Inner> {
    pub fn new(inner: Inner, position: Position, ty: Rc<Type>) -> Self {
        Self {
            id: NodeId::next(),
            inner,
            position,
            ty,
        }
    }

    pub fn inner(&self) -> &Inner {
        &self.inner
    }
}

impl<Inner> MirNode for HirNodeBase<Inner> {
    fn position(&self) -> Position {
        self.position
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn ty(&self) -> Rc<Type> {
        self.ty.clone()
    }
}

pub type HirExpr = HirNodeBase<HirExprInner>;
pub type HirStmt = HirNodeBase<HirStmtKind>;
pub type HirSpec = HirNodeBase<HirSpecKind>;
pub type HirItem = HirNodeBase<HirItemKind>;
pub type HirParam = HirNodeBase<Param>;
pub type HirField = HirNodeBase<Field>;

pub type HirExprPtr = Rc<HirExpr>;
pub type HirStmtPtr = Rc<HirStmt>;
pub type HirSpecPtr = Rc<HirSpec>;
pub type HirItemPtr = Rc<HirItem>;
pub type HirParamPtr = Rc<HirParam>;
pub type HirFieldPtr = Rc<HirField>;

impl HirExpr {
    pub fn is_literal(&self) -> bool {
        match self.inner().kind() {
            HirExprKind::Integer(_)
            | HirExprKind::Float(_)
            | HirExprKind::String(_)
            | HirExprKind::Char(_) => true,
            _ => false,
        }
    }

    pub fn as_integer(&self) -> i64 {
        match self.inner().kind() {
            HirExprKind::Integer(val) => *val,
            _ => panic!("Compiler Error: attempting to get an integer from a non-integer node"),
        }
    }

    pub fn returned_expression(&self) -> Option<&HirExpr> {
        match self.inner().kind() {
            HirExprKind::Block(block_expr) => block_expr
                .stmts
                .first()
                .map(|stmt| match stmt.inner() {
                    HirStmtKind::Expr(expr) => Some(expr.as_ref()),
                    _ => None,
                })
                .unwrap(),
            HirExprKind::While(..) | HirExprKind::For(..) | HirExprKind::Loop(..) => None,
            _ => Some(self),
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirFile {
    id: FileId,
    entry: Option<EntityRef>,
    stem: String,
    stmts: Vec<HirStmtPtr>,
}

impl HirFile {
    pub fn new(id: FileId, stem: String, stmts: Vec<HirStmtPtr>) -> Self {
        Self {
            id,
            stem,
            stmts,
            entry: None,
        }
    }

    pub fn stem(&self) -> &str {
        self.stem.as_str()
    }

    pub fn stmts(&self) -> &[HirStmtPtr] {
        self.stmts.as_slice()
    }

    pub fn id(&self) -> FileId {
        self.id
    }

    pub fn is_root(&self) -> bool {
        self.entry.is_some()
    }

    pub fn set_entry(&mut self, entry: EntityRef) {
        self.entry = Some(entry);
    }

    pub fn find_entity_by_name(&self, name: &str) -> Option<EntityRef> {
        for stmt in self.stmts() {
            match stmt.inner() {
                HirStmtKind::Item(entity) => {
                    let entity_borrow = entity.borrow();
                    if entity_borrow.name() == name {
                        std::mem::drop(entity_borrow);
                        return Some(entity.clone());
                    } else {
                        continue;
                    }
                }
                _ => continue,
            }
        }
        None
    }

    pub fn get_entry(&self) -> Option<EntityRef> {
        self.entry.as_ref().map(|e| e.clone())
    }
}
