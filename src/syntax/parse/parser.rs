use crate::syntax::token::TokenTree;
use crate::syntax::tokenizer::TokenCursor;

pub struct Parser<'src> {
    cursor: TokenCursor<'src>,
    current: Option<TokenTree<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(cursor: TokenCursor<'src>) -> Self {
        Self {
            cursor,
            current: None,
        }
    }
}
