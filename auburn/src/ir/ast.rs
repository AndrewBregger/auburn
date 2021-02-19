use std::convert::TryFrom;
use std::fmt::{Debug, Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};

use ordered_float::OrderedFloat;

use crate::error::Error;
use crate::syntax::{FilePos, Operator, Position, Span};

macro_rules! define_op {
    ($($name:literal => $en:ident), *, $ty:ident) => {
        #[derive(Debug, Clone, Copy, Eq, PartialEq)]
        pub enum $ty {
            $(
                $en,
            )*
        }

        impl Display for $ty {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$en => write!(f, $name),
                    )*
                }
            }
        }
    }
}

define_op!(
    "+" => Plus,
    "-" => Minus,
    "*" => Astrick,
    "/" => Slash,
    "<" => Less,
    ">" => Greater,
    "<=" => LessEq,
    ">=" => GreaterEq,
    "&" => Ampersand,
    "|" => Pipe,
    "%" => Percent,
    "==" => EqualEqual,
    "!=" => BangEqual,
    "<<" => LessLess,
    ">>" => GreaterGreater,
    BinaryOp
);

define_op!(
    "="   => Assign,
    "+="  => PlusAssign,
    "-="  => MinusAssign,
    "*="  => AstriskAssign,
    "/="  => SlashAssign,
    "&="  => AmpersandAssign,
    "|="  => PipeAssign,
    "%="  => PercentAssign,
    "<<=" => LessLessAssign,
    ">>=" => GreaterGreaterAssign,
    AssignmentOp
);

impl BinaryOp {
    pub fn is_cmp(&self) -> bool {
        match self {
            Self::Less
            | Self::Greater
            | Self::LessEq
            | Self::GreaterEq
            | Self::EqualEqual
            | Self::BangEqual => true,
            _ => false,
        }
    }
}

impl TryFrom<Operator> for BinaryOp {
    type Error = Error;

    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        match value {
            Operator::Plus => Ok(Self::Plus),
            Operator::Minus => Ok(Self::Minus),
            Operator::Astrick => Ok(Self::Astrick),
            Operator::Slash => Ok(Self::Slash),
            Operator::Less => Ok(Self::Less),
            Operator::Greater => Ok(Self::Greater),
            Operator::LessEq => Ok(Self::LessEq),
            Operator::GreaterEq => Ok(Self::GreaterEq),
            Operator::Ampersand => Ok(Self::Ampersand),
            Operator::Pipe => Ok(Self::Pipe),
            Operator::Percent => Ok(Self::Percent),
            Operator::LessLess => Ok(Self::LessLess),
            Operator::GreaterGreater => Ok(Self::GreaterGreater),
            Operator::EqualEqual => Ok(Self::EqualEqual),
            Operator::BangEqual => Ok(Self::BangEqual),
            _ => Err(Error::invalid_binary_operator(value)),
        }
    }
}

impl AssignmentOp {
    pub fn get_binary_op(&self) -> Option<BinaryOp> {
        match self {
            Self::Assign => None,
            Self::PlusAssign => Some(BinaryOp::Plus),
            Self::MinusAssign => Some(BinaryOp::Minus),
            Self::AstriskAssign => Some(BinaryOp::Astrick),
            Self::SlashAssign => Some(BinaryOp::Slash),
            Self::AmpersandAssign => Some(BinaryOp::Ampersand),
            Self::PipeAssign => Some(BinaryOp::Pipe),
            Self::PercentAssign => Some(BinaryOp::Percent),
            Self::LessLessAssign => Some(BinaryOp::LessLess),
            Self::GreaterGreaterAssign => Some(BinaryOp::GreaterGreater),
        }
    }
}

impl TryFrom<Operator> for AssignmentOp {
    type Error = Error;

    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        match value {
            Operator::Equal => Ok(Self::Assign),
            Operator::PlusEq => Ok(Self::PlusAssign),
            Operator::MinusEq => Ok(Self::MinusAssign),
            Operator::AstriskEq => Ok(Self::AstriskAssign),
            Operator::SlashEq => Ok(Self::SlashAssign),
            Operator::AmpersandEq => Ok(Self::AmpersandAssign),
            Operator::PipeEq => Ok(Self::PipeAssign),
            Operator::PercentEq => Ok(Self::PercentAssign),
            Operator::LessLessEq => Ok(Self::LessLessAssign),
            Operator::GreaterGreaterEq => Ok(Self::GreaterGreaterAssign),
            _ => Err(Error::invalid_assignment_operator(value)),
        }
    }
}

define_op!(
    "&" => Ampersand,
    "!"  => Bang,
    "-" => Minus,
    UnaryOp
);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AstNodeType {
    Expr,
    Stmt,
    Item,
    Spec,
    Ident,
    Param,
    Field,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident {
    pub value: String,
}

impl From<&str> for Ident {
    fn from(other: &str) -> Self {
        Self {
            value: other.to_string(),
        }
    }
}

impl From<String> for Ident {
    fn from(other: String) -> Self {
        Self { value: other }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StructExprField {
    Bind(Identifier, Box<Expr>),
    Field(Box<Expr>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ExprKind {
    Integer(u64),
    Float(OrderedFloat<f64>),
    String(String),
    Char(char),
    Bool(bool),
    Name(Identifier),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Field(Box<Expr>, Box<Identifier>),
    Call {
        operand: Box<Expr>,
        actual: Vec<Box<Expr>>,
    },
    Method {
        name: Box<Identifier>,
        actual: Vec<Box<Expr>>,
    },
    Block(Vec<Box<Stmt>>),
    Tuple(Vec<Box<Expr>>),
    Loop(Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    For {
        element: Identifier,
        expr: Box<Expr>,
        body: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        body: Box<Expr>,
        else_if: Option<Box<Expr>>,
    },
    StructExpr {
        name: Box<Expr>,
        fields: Vec<StructExprField>,
    },
    SelfLit,
    SelfType,
    Continue,
    Break,
    Return(Box<Expr>),
    Index {
        operand: Box<Expr>,
        index: Box<Expr>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StmtKind {
    Expr(Box<Expr>),
    Item(Box<Item>),
    Assignment {
        op: AssignmentOp,
        lvalue: Box<Expr>,
        rhs: Box<Expr>,
    },
    // temporary until built in print function is implemented
    Echo(Box<Expr>),
    Empty,
}

impl StmtKind {
    pub fn is_empty(&self) -> bool {
        match self {
            StmtKind::Empty => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FunctionBody {
    Block(Box<Expr>),
    Expression(Box<Expr>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Visibility {
    Private,
    Public,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ItemKind {
    Variable {
        vis: Visibility,
        mutable: bool,
        name: Identifier,
        init: Option<Box<Expr>>,
        spec: Option<Box<Spec>>,
    },
    Struct {
        vis: Visibility,
        name: Identifier,
        fields: Vec<Box<Item>>,
    },
    Function {
        vis: Visibility,
        name: Identifier,
        params: Vec<Box<Item>>,
        ret: Box<Spec>,
        body: FunctionBody,
    },
    Param {
        names: Vec<Identifier>,
        spec: Option<Box<Spec>>,
        init: Option<Box<Expr>>,
    },
    SelfParam {
        mutable: bool,
    },
    Field {
        vis: Visibility,
        names: Vec<Identifier>,
        spec: Option<Box<Spec>>,
        init: Option<Box<Expr>>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SpecKind {
    Named(Box<Expr>),
    Tuple(Vec<Box<Spec>>),
    Array(Box<Spec>, Option<Box<Expr>>),
    Unit,
    Infer,
    SelfType,
    Mutable { inner: Box<Spec> },
}

#[derive(Debug, Clone, Copy, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct NodeId(pub usize);

impl NodeId {
    pub(crate) fn next() -> NodeId {
        static TOKEN: AtomicUsize = AtomicUsize::new(0);
        NodeId(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

pub trait Node {
    fn id(&self) -> NodeId;

    fn span(&self) -> Span;

    fn file_pos(&self) -> FilePos;

    fn position(&self) -> Position;

    fn name(&self) -> &'static str;
}

pub trait NodeType: Clone {
    fn name(&self) -> &'static str;
    fn ty(&self) -> AstNodeType;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstNode<Kind> {
    id: NodeId,
    position: Position,
    kind: Kind,
}

impl<Kind: NodeType> AstNode<Kind> {
    pub fn new(kind: Kind) -> Self {
        Self {
            id: NodeId::next(),
            position: Position::default(),
            kind,
        }
    }

    pub fn new_with_position(kind: Kind, position: Position) -> Self {
        Self {
            id: NodeId::next(),
            position,
            kind,
        }
    }

    pub fn kind(&self) -> &Kind {
        &self.kind
    }
}

impl<Kind: NodeType> Node for AstNode<Kind> {
    fn id(&self) -> NodeId {
        self.id
    }

    fn span(&self) -> Span {
        self.position.span()
    }

    fn file_pos(&self) -> FilePos {
        self.position.file_pos()
    }

    fn position(&self) -> Position {
        self.position
    }

    fn name(&self) -> &'static str {
        self.kind.name()
    }
}

impl NodeType for ExprKind {
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
            Self::Break => "Break",
            Self::Continue => "Continue",
            Self::Return(..) => "Return",
            Self::Index { .. } => "Index",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Expr
    }
}

impl NodeType for StmtKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Expr(_) => "Expr Stmt",
            Self::Item(_) => "Item Stmt",
            Self::Empty => "Empty Stmt",
            Self::Assignment { .. } => "Assignment Stmt",
            Self::Echo(_) => "Echo Stmt",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Stmt
    }
}

impl NodeType for ItemKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Variable { .. } => "Variable",
            Self::Function { .. } => "Function",
            Self::Struct { .. } => "Struct",
            Self::Param { .. } => "Param",
            Self::SelfParam { .. } => "SelfParam",
            Self::Field { .. } => "Field",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Item
    }
}

impl NodeType for SpecKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Named(_) => "Named",
            Self::Tuple(_) => "Tuple",
            Self::Unit => "Unit",
            Self::Infer => "Infer",
            Self::SelfType => "Self",
            Self::Array { .. } => "Array",
            Self::Mutable { .. } => "Mut",
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Spec
    }
}

impl NodeType for Ident {
    fn name(&self) -> &'static str {
        "Identifier"
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Ident
    }
}

pub type Expr = AstNode<ExprKind>;
pub type Item = AstNode<ItemKind>;
pub type Stmt = AstNode<StmtKind>;
pub type Spec = AstNode<SpecKind>;
pub type Identifier = AstNode<Ident>;

impl Spec {
    pub fn is_infer(&self) -> bool {
        match self.kind() {
            SpecKind::Infer => true,
            _ => false,
        }
    }
}

impl Item {
    pub fn get_name(&self) -> Option<&Identifier> {
        match self.kind() {
            ItemKind::Variable { name, .. }
            | ItemKind::Struct { name, .. }
            | ItemKind::Function { name, .. } => Some(name),
            ItemKind::Param { .. } | ItemKind::Field { .. } | ItemKind::SelfParam { .. } => None,
        }
    }

    pub fn get_visibility(&self) -> Visibility {
        match self.kind() {
            ItemKind::Variable { vis, .. }
            | ItemKind::Struct { vis, .. }
            | ItemKind::Function { vis, .. }
            | ItemKind::Field { vis, .. } => *vis,
            ItemKind::Param { .. } | ItemKind::SelfParam { .. } => Visibility::Private,
        }
    }
}
