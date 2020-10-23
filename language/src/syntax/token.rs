use crate::syntax::Position;
use std::convert::TryFrom;

use ordered_float::OrderedFloat;
use std::fmt::Formatter;

// auto generate the mapping of keyword to string
macro_rules! string_mapping {
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

        impl TryFrom<&str> for $ty {
            type Error = ();

            fn try_from(other: &str) -> Result<Self, ()> {
                match other {
                    $(
                        $name => Ok(Self::$en),
                    )*
                    _ => Err(())
                }
            }
        }
    }
}

string_mapping!(
    "let" => Let,
    "fn" => Fn,
    "struct" => Struct,
    "pub" => Pub,
    "use" => Use,
    "enum" => Enum,
    "trait" => Trait,
    "extend" => Expand,
    "mut" => Mut,
    "types" => Ty,
    "Type" => TyLit,
    "if" => If,
    "else" => Else,
    "elif" => Elif,
    "while" => While,
    "loop" => Loop,
    "for" => For,
    "in" => In,
    "Self" => SelfType,
    "self" => SelfLit,
    "stack_alloc" => StackAlloc,
    Keyword
);

string_mapping!(
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
    "=" =>  Equal,
    "!=" => BangEqual,
    "!"  => Bang,
    "." => Period,
    ";" => Semicolon,
    ":" => Colon,
    "," => Comma,
    "+="  => PlusEq,
    "-="  => MinusEq,
    "*="  => AstriskEq,
    "/="  => SlashEq,
    "&=" => AmpersandEq,
    "|=" => PipeEq,
    "%=" => PercentEq,
    "<<" => LessLess,
    ">>" => GreaterGreater,
    "<<=" => LessLessEq,
    ">>=" => GreaterGreaterEq,
    Operator
);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Control {
    Paren,
    Bracket,
    Brace,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum PairKind {
    Open,
    Close,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Associative {
    Left,
    Right,
    None,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token<'a> {
    Kw(Keyword),
    Op(Operator),
    Ident(&'a str),
    ControlPair(Control, PairKind),
    Integer(u64),
    Float(OrderedFloat<f64>),
    String(String),
    Comment(String),
    Newline,
    Eof,
}

impl<'a> Token<'a> {
    pub fn is_nl(&self) -> bool {
        *self == Token::Newline
    }

    pub fn is_eof(&self) -> bool {
        *self == Token::Eof
    }

    pub fn is_comment(&self) -> bool {
        match self {
            Self::Comment(_) => true,
            _ => false,
        }
    }

    pub fn is_op(&self) -> bool {
        match self {
            Self::Op(_) => true,
            _ => false,
        }
    }

    pub fn is_ident(&self) -> bool {
        match self {
            Self::Ident(_) => true,
            _ => false,
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            Self::Float(_) | Self::Integer(_) | Self::String(_) => true,
            _ => false,
        }
    }

    pub fn is_keyword(&self) -> bool {
        match self {
            Self::Kw(_) => true,
            _ => false,
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            Self::Op(op) => match op {
                Operator::Slash | Operator::Astrick | Operator::Percent => 13,
                Operator::Plus | Operator::Minus => 12,
                Operator::LessLess | Operator::GreaterGreater => 11,
                Operator::Less | Operator::Greater | Operator::EqualEqual | Operator::BangEqual => 10,
                Operator::Ampersand => 9,
                // Op::Carrot => 8,
                // Op::Tilde => 7,
                Operator::Pipe => 6,
                // and => 5
                // or  => 4
                Operator::Bang => 3,
                Operator::Equal
                | Operator::PlusEq
                | Operator::MinusEq
                | Operator::SlashEq
                | Operator::AstriskEq
                //| Op::AstrickAstrickEq
                | Operator::PercentEq
                | Operator::LessEq
                | Operator::GreaterEq
                | Operator::PipeEq
                | Operator::AmpersandEq
                // | Operator::CarrotEq
                | Operator::LessLessEq
                | Operator::GreaterGreaterEq => 2,
                // Op::Dollar | Op::Question => 1,
                _ => 0,
            },
            _ => 0,
        }
    }

    pub fn associativity(&self) -> Associative {
        match self {
            Self::Op(op) => match op {
                _ => Associative::Left,
            },
            _ => Associative::None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PToken<'a> {
    source: &'a str,
    token: Token<'a>,
    position: Position,
}

impl<'a> PToken<'a> {
    pub fn new(source: &'a str, token: Token<'a>, position: Position) -> Self {
        Self {
            source,
            token,
            position,
        }
    }

    pub fn token(&self) -> &Token<'a> {
        &self.token
    }

    pub fn position(&self) -> Position {
        self.position
    }

    pub fn to_token(self) -> Token<'a> {
        self.token
    }

    pub fn is_eof(&self) -> bool {
        self.token.is_eof()
    }

    pub fn is_comment(&self) -> bool {
        self.token.is_comment()
    }

    pub fn precedence(&self) -> u8 {
        self.token.precedence()
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Kw(kw) => write!(f, "{}", kw.to_string()),
            Token::Op(op) => write!(f, "{}", op.to_string()),
            Token::Ident(val) => write!(f, "{}", val),
            Token::ControlPair(ctrl, kind) => match ctrl {
                Control::Paren => match kind {
                    PairKind::Open => write!(f, "("),
                    PairKind::Close => write!(f, ")"),
                },
                Control::Bracket => match kind {
                    PairKind::Open => write!(f, "{{"),
                    PairKind::Close => write!(f, "}}"),
                },
                Control::Brace => match kind {
                    PairKind::Open => write!(f, "["),
                    PairKind::Close => write!(f, "]"),
                },
            },
            Token::Comment(_) => write!(f, "comment"),
            Token::Integer(val) => write!(f, "{}", val),
            Token::Float(val) => write!(f, "{}", val),
            Token::String(val) => write!(f, "{}", val),
            Token::Newline => write!(f, "newline"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

impl<'a> std::fmt::Display for PToken<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}
