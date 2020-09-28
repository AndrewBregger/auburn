use std::fmt::Debug;
use std::sync::atomic::{AtomicUsize, Ordering};

use ordered_float::OrderedFloat;

use crate::error::Error;
use crate::syntax::{FilePos, Operator, Position, Span};
use std::convert::TryFrom;

macro_rules! define_op {
    ($($name:literal => $en:ident), *, $ty:ident) => {
        #[derive(Debug, Clone, Copy, Eq, PartialEq)]
        pub enum $ty {
            $(
                $en,
            )*
        }

        impl ToString for $ty {
            fn to_string(&self) -> String {
                match self {
                    $(
                        Self::$en => $name.to_string(),
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
}

#[derive(Debug, Clone)]
pub struct Ident {
    value: String,
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

#[derive(Debug, Clone)]
pub enum ExprKind {
    Integer(u64),
    Float(OrderedFloat<f64>),
    String(String),
    Char(char),
    Name(Identifier),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Field(Box<Expr>, Box<Identifier>),
    Call {
        operand: Box<Expr>,
        actual: Vec<Box<Expr>>,
    },
    Method {
        operand: Box<Expr>,
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
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expr(Box<Expr>),
    Item(Box<Item>),
    Empty,
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Block(Box<Expr>),
    Expression(Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
pub enum Visability {
    Private,
    Public,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Variable {
        vis: Visability,
        mutable: bool,
        name: Identifier,
        init: Option<Box<Expr>>,
        spec: Option<Box<Spec>>,
    },
    Struct {
        vis: Visability,
        name: Identifier,
        fields: Vec<Box<Item>>,
    },
    Function {
        vis: Visability,
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
    Field {
        vis: Visability,
        names: Vec<Identifier>,
        spec: Option<Box<Spec>>,
        init: Option<Box<Expr>>,
    },
}

#[derive(Debug, Clone)]
pub enum SpecKind {
    Named(Box<Expr>),
    Tuple(Vec<Box<Spec>>),
    Unit,
    Infer,
}

pub trait Node {
    fn next_id() -> usize {
        static TOKEN: AtomicUsize = AtomicUsize::new(0);
        TOKEN.fetch_add(1, Ordering::SeqCst)
    }

    fn id(&self) -> usize;

    fn span(&self) -> Span;

    fn file_pos(&self) -> FilePos;

    fn position(&self) -> Position;
}

pub trait NodeType: Debug + Clone {
    fn name(&self) -> &'static str;
    fn ty(&self) -> AstNodeType;
}

#[derive(Debug, Clone)]
pub struct AstNode<Kind> {
    id: usize,
    position: Position,
    kind: Kind,
}

impl<Kind: NodeType> AstNode<Kind> {
    pub fn new(kind: Kind) -> Self {
        Self {
            id: Self::next_id(),
            position: Position::default(),
            kind,
        }
    }

    pub fn new_with_position(kind: Kind, position: Position) -> Self {
        Self {
            id: Self::next_id(),
            position,
            kind,
        }
    }

    pub fn kind(&self) -> &Kind {
        &self.kind
    }
}

impl<Kind: NodeType> Node for AstNode<Kind> {
    fn id(&self) -> usize {
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
}

impl NodeType for ExprKind {
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
