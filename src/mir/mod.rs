use crate::analysis::EntityRef;
use crate::syntax::ast::{
    AstNodeType, BinaryOp, Identifier, NodeId, NodeType, UnaryOp, Visibility,
};
use crate::syntax::Position;
use crate::system::FileId;
use crate::types::Type;
use ordered_float::OrderedFloat;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub left: Rc<MirExpr>,
    pub right: Rc<MirExpr>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub operand: Rc<MirExpr>,
}

#[derive(Debug, Clone)]
pub struct FieldExpr {
    pub operand: Rc<MirExpr>,
    pub field_idx: usize,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub operand: Rc<MirExpr>,
    pub function_type: Rc<Type>,
    pub actuals: Vec<Rc<MirExpr>>,
}

#[derive(Debug, Clone)]
pub struct MethodExpr {
    pub operand: Rc<MirExpr>,
    pub function_type: Rc<Type>,
    pub actuals: Vec<Rc<MirExpr>>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub stmts: Vec<Rc<MirStmt>>,
    pub return_used: bool,
}

#[derive(Debug, Clone)]
pub struct TupleExpr {
    pub elements: Vec<Rc<MirExpr>>,
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
    pub body: Rc<MirExpr>,
}

#[derive(Debug, Clone)]
pub struct WhileExpr {
    pub cond: Rc<MirExpr>,
    pub body: Rc<MirExpr>,
}

#[derive(Debug, Clone)]
pub struct ForExpr {
    pub element: Identifier,
    pub expr: Rc<MirExpr>,
    pub body: Rc<MirExpr>,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond: Rc<MirExpr>,
    pub body: Rc<MirExpr>,
    pub else_if: Option<Rc<MirExpr>>,
}

#[derive(Debug, Clone)]
pub struct StructExpr {
    pub struct_type: Rc<Type>,
    pub fields: Vec<(usize, Rc<MirExpr>)>,
}

#[derive(Debug, Clone)]
pub enum MirExprKind {
    Integer(u64),
    Float(OrderedFloat<f64>),
    String(String),
    Char(char),
    Name(EntityRef),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Field(FieldExpr),
    Call(CallExpr),
    Method(MethodExpr),
    Block(BlockExpr),
    Tuple(TupleExpr),
    Loop(LoopExpr),
    While(WhileExpr),
    For(ForExpr),
    If(IfExpr),
    StructExpr(StructExpr),
    SelfLit,
    SelfType,
}

impl NodeType for MirExprKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "Integer",
            Self::Float(_) => "Float",
            Self::String(_) => "String",
            Self::Char(_) => "Char",
            Self::Name(_) => "Name",
            Self::Binary(..) => "Binary",
            Self::Unary(..) => "Unary",
            Self::Field(..) => "Field",
            Self::Call { .. } => "Call",
            Self::Method { .. } => "Method",
            Self::Block(..) => "Block",
            Self::Tuple(..) => "Tuple",
            Self::Loop(..) => "Loop",
            Self::While(..) => "While",
            Self::For { .. } => "For",
            Self::If { .. } => "If",
            Self::StructExpr { .. } => "Struct Expr",
            Self::SelfType => "Self Type",
            Self::SelfLit => "Self Literal",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Expr
    }
}

impl NodeType for MirStmtKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Expr(_) => "Expr Stmt",
            Self::Item(_) => "Item Stmt",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Stmt
    }
}

impl NodeType for MirItemKind {
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

impl NodeType for MirSpecKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Named => "Named",
            Self::Tuple => "Tuple",
            Self::Unit => "Unit",
            Self::SelfType => "Self",
            Self::Infer => "Infer",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Spec
    }
}

// impl NodeType for Ident {
//     fn name(&self) -> &'static str {
//         "Identifier"
//     }
//
//     fn ty(&self) -> AstNodeType {
//         AstNodeType::Ident
//     }
// }

#[derive(Debug, Clone)]
pub enum MirStmtKind {
    Expr(Rc<MirExpr>),
    Item(EntityRef),
}

// the actual type will be the ty() of MirNode
#[derive(Debug, Clone)]
pub enum MirSpecKind {
    Named,
    Tuple,
    Unit,
    SelfType,
    Infer,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub vis: Visibility,
    pub mutable: bool,
    pub name: Identifier,
    pub init: Option<Rc<MirExpr>>,
    pub spec: Option<Rc<MirSpec>>,
}

#[derive(Debug, Clone)]
pub struct Structure {
    pub vis: Visibility,
    pub name: Identifier,
    pub fields: Vec<Rc<MirField>>,
    pub methods: Vec<Rc<MirItem>>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub vis: Visibility,
    pub name: Identifier,
    pub params: Vec<Rc<MirParam>>,
    pub ret: Rc<MirSpec>,
    pub body: Rc<MirExpr>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Identifier,
    pub spec: Option<Rc<MirSpec>>,
    pub init: Option<Rc<MirExpr>>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub vis: Visibility,
    pub name: Identifier,
    pub spec: Option<Rc<MirSpec>>,
    pub init: Option<Rc<MirExpr>>,
}

#[derive(Debug, Clone)]
pub enum MirItemKind {
    Variable(Variable),
    Struct(Structure),
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct MirNodeBase<Inner> {
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

impl<Inner> MirNodeBase<Inner> {
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

impl<Inner> MirNode for MirNodeBase<Inner> {
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

pub type MirExpr = MirNodeBase<MirExprKind>;
pub type MirStmt = MirNodeBase<MirStmtKind>;
pub type MirSpec = MirNodeBase<MirSpecKind>;
pub type MirItem = MirNodeBase<MirItemKind>;
pub type MirParam = MirNodeBase<Param>;
pub type MirField = MirNodeBase<Field>;

pub type MirExprPtr = Rc<MirExpr>;
pub type MirStmtPtr = Rc<MirStmt>;
pub type MirSpecPtr = Rc<MirSpec>;
pub type MirItemPtr = Rc<MirItem>;
pub type MirParamPtr = Rc<MirParam>;
pub type MirFieldPtr = Rc<MirField>;

impl MirExpr {
    pub fn is_literal(&self) -> bool {
        match self.inner() {
            MirExprKind::Integer(_)
            | MirExprKind::Float(_)
            | MirExprKind::String(_)
            | MirExprKind::Char(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MirFile {
    id: FileId,
    global_expressions: Vec<MirExprPtr>,
    entities: Vec<EntityRef>,
}

impl MirFile {
    pub(crate) fn new(
        id: FileId,
        global_expressions: Vec<MirExprPtr>,
        entities: Vec<EntityRef>,
    ) -> Self {
        Self {
            id,
            global_expressions,
            entities,
        }
    }

    pub fn expressions(&self) -> &[MirExprPtr] {
        self.global_expressions.as_slice()
    }

    pub fn entities(&self) -> &[EntityRef] {
        self.entities.as_slice()
    }
}
