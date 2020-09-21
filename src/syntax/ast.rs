use std::fmt::Debug;
use std::sync::atomic::{AtomicUsize, Ordering};

use ordered_float::OrderedFloat;

use crate::syntax::{FilePos, Position, Span, Operator};
use std::convert::TryFrom;
use crate::error::Error;

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
            _ => Err(Error::invalid_binary_operator(value))
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
    Ident
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
        Self {
            value: other
        }
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
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expr(Box<Expr>),
    Item(Box<Item>),
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Variable(Identifier, Option<Box<Expr>>, Option<Box<Spec>>),
}

#[derive(Debug, Clone)]
pub enum SpecKind {
    Named(Box<Expr>),
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

    fn position(&self) -> Position { self.position }
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
        }
    }

    fn ty(&self) -> AstNodeType {
        AstNodeType::Stmt
    }
}

impl NodeType for ItemKind {
    fn name(&self) -> &'static str {
        match self {
            Self::Variable(..) => "Variable",
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
