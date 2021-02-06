use std::{
    fmt::{Display, Formatter},
    path::Path,
};

mod parse;
pub mod token;
mod tokenizer;

pub use parse::Parser;
pub use token::{Control, Keyword, Operator, PToken, PairKind, Token};
pub use tokenizer::TokenCursor;

use crate::ir::ast;
use crate::ir::ast::Node;
use crate::syntax::ast::Stmt;
use crate::system::FileId;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct Span(pub usize, pub usize);

impl Span {
    pub fn start(&self) -> usize {
        self.0
    }

    pub fn end(&self) -> usize {
        self.1
    }

    pub fn as_tuple(&self) -> (usize, usize) {
        (self.0, self.1)
    }

    pub fn extended_to(&self, other: Self) -> Self {
        Self(self.0, other.1)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.0, self.1)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct Coord(pub usize, pub usize);

impl Coord {
    pub fn line(&self) -> usize {
        self.0
    }

    pub fn column(&self) -> usize {
        self.1
    }

    pub fn as_tuple(&self) -> (usize, usize) {
        (self.0, self.1)
    }
}

impl Display for Coord {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct FilePos {
    start: Coord,
    end: Coord,
}

impl FilePos {
    pub fn new(start: Coord, end: Coord) -> Self {
        Self { start, end }
    }

    pub fn extended_to(&self, other: Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
}

impl Display for FilePos {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}->{}", self.start, self.end)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct Position {
    span: Span,
    file_pos: FilePos,
    file_id: FileId,
}

impl Position {
    pub fn new(span: Span, file_pos: FilePos, file_id: FileId) -> Self {
        Self {
            span,
            file_pos,
            file_id,
        }
    }

    pub fn extended_to<N: Node>(&self, node: &N) -> Self {
        Self::new(
            self.span.extended_to(node.span()),
            self.file_pos.extended_to(node.file_pos()),
            self.file_id,
        )
    }

    pub fn extended_to_token(&self, token: PToken) -> Self {
        let position = token.position();
        Self::new(
            self.span.extended_to(position.span()),
            self.file_pos.extended_to(position.file_pos()),
            self.file_id,
        )
    }

    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn file_pos(&self) -> FilePos {
        self.file_pos
    }

    pub fn start(&self) -> Coord {
        self.file_pos.start
    }

    pub fn end(&self) -> Coord {
        self.file_pos.end
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Position(span: {}, pos: {}, fid: {})",
            self.span, self.file_pos, self.file_id.0
        )
    }
}

#[derive(Debug, Clone)]
pub struct ParsedFile {
    pub file_name: String,
    pub file_id: FileId,
    // imports: Vec<Box<Import>>,
    pub stmts: Vec<Box<ast::Stmt>>,
}

impl ParsedFile {
    pub fn new(file_name: String, file_id: FileId) -> Self {
        Self {
            file_name,
            file_id,
            // imports: vec![],
            stmts: vec![],
        }
    }

    pub fn stem(&self) -> &str {
        let path = Path::new(self.file_name.as_str());
        path.file_stem()
            .and_then(|f| f.to_str())
            .expect("expected a file")
    }

    pub fn push_stmt(&mut self, stmt: Box<Stmt>) {
        self.stmts.push(stmt)
    }
}
