use crate::syntax::{FilePos, Span};
use std::fmt::Debug;
use std::sync::atomic::{AtomicUsize, Ordering};

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

define_op!(
    "&" => Ampersand,
    "!"  => Bang,
    UnaryOp
);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AstNodeType {
    Expr,
    Stmt,
    Item,
    Spec,
}

#[derive(Debug, Clone)]
pub struct Ident {
    value: String,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Integer(u64),
    Float(f64),
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
}

pub trait NodeType: Debug + Clone {
    fn name(&self) -> &'static str;
    fn ty(&self) -> AstNodeType;
}

#[derive(Debug, Clone)]
pub struct AstNode<Kind> {
    id: usize,
    span: Span,
    position: FilePos,
    kind: Kind,
}

impl<Kind: NodeType> AstNode<Kind> {
    pub fn new(kind: Kind, span: Span, position: FilePos) -> Self {
        Self {
            id: Self::next_id(),
            span,
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
        self.span
    }

    fn file_pos(&self) -> FilePos {
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

pub type Expr = AstNode<ExprKind>;
pub type Item = AstNode<ItemKind>;
pub type Stmt = AstNode<StmtKind>;
pub type Spec = AstNode<SpecKind>;
pub type Identifier = AstNode<Ident>;
