mod ast;
mod parse;
mod token;
mod tokenizer;

pub use crate::syntax::token::Keyword;
pub use token::Token;
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

    pub fn start(&self) -> Coord {
        self.file_pos.start
    }

    pub fn end(&self) -> Coord {
        self.file_pos.end
    }
}
