use crate::error::Error;
use crate::file::File;
use crate::syntax::ast::*;
use crate::syntax::tokenizer::TokenCursor;
use crate::syntax::ParsedFile;
use crate::syntax::{Control, Keyword, Operator, Position, Token, TokenTree, TokenTreeKind};

use std::vec::IntoIter;

struct TreeNode<'src> {
    control: Control,
    children: IntoIter<TokenTree<'src>>,
}

type Restriction = usize;

const DEFAULT: Restriction = 0;
// const : Restriction = 0;
// const : Restriction = 1;
// const : Restriction = 2;
// const : Restriction = 4;
// const : Restriction = 8;
// const : Restriction = 16;

pub struct Parser<'src> {
    restriction: Restriction,
    cursor: TokenCursor<'src>,
    tree_stack: Vec<TreeNode<'src>>,
    current: Option<TokenTree<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(file: &'src File) -> Self {
        Self {
            restriction: DEFAULT,
            cursor: TokenCursor::new(file),
            tree_stack: Vec::new(),
            current: None,
        }
    }

    fn current_position(&self) -> Position {
        self.current
            .as_ref()
            .map_or_else(Default::default, |c| c.position())
    }

    fn push_tree_stack(&mut self, control: Control, children: Vec<TokenTree<'src>>) {
        self.tree_stack.push(TreeNode {
            control,
            children: children.into_iter(),
        });
    }

    pub fn consume(&mut self) -> Result<Option<TokenTree<'src>>, Error> {
        let current = self.current.clone();
        if self.tree_stack.is_empty() {
            if let Some(res) = self.cursor.next() {
                self.current = Some(res?);
            } else {
                self.current = None
            }
            Ok(current)
        } else {
            while let Some(mut top) = self.tree_stack.last_mut() {
                if let Some(token) = top.children.next() {
                    self.current = Some(token);
                    return Ok(current);
                } else {
                    self.tree_stack.pop();
                }
            }
            assert!(self.tree_stack.is_empty());
            if let Some(res) = self.cursor.next() {
                self.current = Some(res?);
            } else {
                self.current = None
            }
            Ok(current)
        }
    }

    fn check_for(&self, token: Token) -> bool {
        if let Some(current) = self.current.as_ref() {
            *current.kind() == TokenTreeKind::Token(token)
        } else {
            false
        }
    }

    fn expect(&mut self, token: Token) -> Result<TokenTree, Error> {
        if self.check_for(token.clone()) {
            Ok(self.consume()?.unwrap())
        } else {
            Err(
                Error::unexpected_token(token, self.current.as_ref().unwrap())
                    .with_position(self.current_position()),
            )
        }
    }

    pub fn parse_file(&mut self, file: &File) -> Result<ParsedFile, Error> {
        let mut parsed_file = ParsedFile::new(file.id());
        while let Some(current) = self.current.clone() {
            if current.is_eof() {
                break;
            }

            let stmt = self.parse_stmt()?;
            parsed_file.push_stmt(stmt);
        }
        Ok(parsed_file)
    }

    pub fn parse_stmt(&mut self) -> Result<Box<Stmt>, Error> {
        unimplemented!()
    }

    pub fn parse_expr(&mut self) -> Result<Box<Expr>, Error> {
        self.parse_expr_with_res(DEFAULT)
    }

    fn parse_expr_with_res(&mut self, res: Restriction) -> Result<Box<Expr>, Error> {
        let res = self.restriction;
        let result = self.parse_assoc_expr(1);
        self.restriction = res;
        result
    }

    fn parse_assoc_expr(&mut self, min_prec: usize) -> Result<Box<Expr>, Error> {
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> Result<Box<Expr>, Error> {
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Box<Expr>, Error> {
        self.parse_bottom()
    }

    fn parse_bottom(&mut self) -> Result<Box<Expr>, Error> {
        let current = self.current.clone();
        if let Some(current) = current {
            let position = current.position();
            match current.to_kind() {
                TokenTreeKind::Pair(ctrl, elements) => {
                    self.push_tree_stack(ctrl, elements);
                    self.consume()?;
                    unimplemented!();
                }
                TokenTreeKind::Token(token) => {
                    self.consume()?;
                    match token {
                        Token::Ident(val) => todo!(),
                        Token::Integer(val) => Ok(Box::new(Expr::new_with_position(
                            ExprKind::Integer(val),
                            position,
                        ))),
                        Token::Float(val) => Ok(Box::new(Expr::new_with_position(
                            ExprKind::Float(val),
                            position,
                        ))),
                        Token::String(val) => Ok(Box::new(Expr::new_with_position(
                            ExprKind::String(val),
                            position,
                        ))),
                        _ => todo!(),
                    }
                }
            }
        } else {
            unreachable!()
        }
    }
}
