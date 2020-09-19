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
    "type" => Ty,
    "Type" => TyLit,
    "if" => If,
    "else" => Else,
    "elif" => Elif,
    "while" => While,
    "loop" => Loop,
    "for" => For,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token<'a> {
    Kw(Keyword),
    Op(Operator),
    Ident(&'a str),
    Integer(u64),
    Float(OrderedFloat<f64>),
    String(String),
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
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenTreeKind<'a> {
    Pair(Control, Vec<TokenTree<'a>>),
    Token(Token<'a>),
}

impl<'a> TokenTreeKind<'a> {
    pub fn has_eof(&self) -> bool {
        match self {
            Self::Token(token) => token.is_eof(),
            Self::Pair(_, elements) => elements
                .iter()
                .map(|e| e.has_eof())
                .fold(true, |acc, x| acc && x),
        }
    }

    pub fn is_eof(&self) -> bool {
        match self {
            Self::Token(token) => token.is_eof(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TokenTree<'a> {
    level: usize,
    source: &'a str,
    kind: TokenTreeKind<'a>,
    position: Position,
}

impl<'a> TokenTree<'a> {
    pub fn new(level: usize, source: &'a str, kind: TokenTreeKind<'a>, position: Position) -> Self {
        Self {
            level,
            source,
            kind,
            position,
        }
    }

    pub fn position(&self) -> Position {
        self.position
    }

    pub fn kind(&'a self) -> &TokenTreeKind<'a> {
        &self.kind
    }

    pub fn to_kind(self) -> TokenTreeKind<'a> {
        self.kind
    }

    pub fn is_pair(&self) -> bool {
        match self.kind {
            TokenTreeKind::Pair(..) => true,
            _ => false,
        }
    }

    pub fn is_token(&self) -> bool {
        !self.is_pair()
    }

    pub fn has_eof(&self) -> bool {
        self.kind.has_eof()
    }

    pub fn is_eof(&self) -> bool {
        self.kind.is_eof()
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Kw(kw) => write!(f, "{}", kw.to_string()),
            Token::Op(op) => write!(f, "{}", op.to_string()),
            Token::Ident(val) => write!(f, "{}", val),
            Token::Integer(val) => write!(f, "{}", val),
            Token::Float(val) => write!(f, "{}", val),
            Token::String(val) => write!(f, "{}", val),
            Token::Newline => write!(f, "newline"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

fn print_control(f: &mut Formatter<'_>, ctrl: Control, elements: &[TokenTree]) -> std::fmt::Result {
    match ctrl {
        Control::Paren => writeln!(f, "(")?,
        Control::Bracket => writeln!(f, "{{")?,
        Control::Brace => writeln!(f, "[")?,
    };

    for element in elements {
        writeln!(f, "{}", element)?;
    }

    match ctrl {
        Control::Paren => write!(f, ")"),
        Control::Bracket => write!(f, "}}"),
        Control::Brace => write!(f, "]"),
    }
}

impl<'a> std::fmt::Display for TokenTreeKind<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenTreeKind::Pair(ctrl, elements) => print_control(f, *ctrl, elements),
            TokenTreeKind::Token(token) => write!(f, "{}", token),
        }
    }
}

impl<'a> std::fmt::Display for TokenTree<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
