pub mod ast;
pub mod token;

mod parse;
mod tokenizer;

pub use parse::Parser;
pub use token::{Control, Keyword, Operator, Token, TokenTree, TokenTreeKind};
pub use tokenizer::TokenCursor;

use crate::file::FileId;

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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct FilePos {
    start: Coord,
    end: Coord,
}

impl FilePos {
    pub fn new(start: Coord, end: Coord) -> Self {
        Self { start, end }
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

pub struct ParsedFile {
    file_id: FileId,
    // imports: Vec<Box<Import>>,
    stmts: Vec<Box<ast::Stmt>>,
}

impl ParsedFile {
    pub fn new(file_id: FileId) -> Self {
        Self {
            file_id,
            // imports: Vec::new(),
            stmts: Vec::new(),
        }
    }

    pub fn push_stmt(&mut self, stmt: Box<ast::Stmt>) {
        self.stmts.push(stmt)
    }
}
