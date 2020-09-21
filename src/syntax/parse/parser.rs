use crate::error::Error;
use crate::file::File;
use crate::syntax::ast::*;
use crate::syntax::tokenizer::TokenCursor;
use crate::syntax::ParsedFile;
use crate::syntax::{Control, Keyword, Operator, Position, Token, TokenTree, TokenTreeKind};

use std::vec::IntoIter;
use std::convert::TryFrom;

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
            cursor:  TokenCursor::new(file),
            tree_stack: vec![],
            current: None,
        }
    }

    pub fn current_token(&self) -> &TokenTree<'src> {
        self.current.as_ref().expect("Compiler Error: Parser: current token is None")

    }

    fn current_position(&self) -> Position {
        self.current_token().position()
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
            while let Some(top) = self.tree_stack.last_mut() {
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
        *self.current_token().kind() == TokenTreeKind::Token(token)
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

    fn parse_ident(&mut self) -> Result<Identifier, Error> {
        let current = self.current_token().clone();

        if current.is_token() {
            self.consume()?;
            let token = current.as_token();
            if let Token::Ident(value) = token {
                return Ok(Identifier::new_with_position(Ident::from(*value), current.position()))
            }
        }
        Err(Error::expecting_identifier(&current).with_position(current.position()))
    }

    pub fn parse_stmt(&mut self) -> Result<Box<Stmt>, Error> {
        unimplemented!()
    }

    pub fn parse_expr(&mut self) -> Result<Box<Expr>, Error> {
        self.parse_expr_with_res(DEFAULT)
    }

    fn parse_expr_with_res(&mut self, new_res: Restriction) -> Result<Box<Expr>, Error> {
        let res = self.restriction;
        self.restriction = new_res;
        let result = self.parse_assoc_expr(1);
        self.restriction = res;
        result
    }

    fn parse_assoc_expr(&mut self, min_prec: u8) -> Result<Box<Expr>, Error> {
        let mut expr = self.parse_unary()?;
        println!("parse_assoc_expr: {} -> {}", self.current_token(), self.current_token().precedence());
        // println!("Position: {}", position);
        while self.current_token().precedence() > min_prec {
            let token_tree = self.current_token().clone();
            let token = token_tree.as_token();
            let lhs_position = expr.position();

            if let Token::Op(op) =  token {
                self.consume()?;

                let rhs = self.parse_assoc_expr(token.precedence() + 1)?;
                let position = lhs_position.extended_to(rhs.as_ref());

                match BinaryOp::try_from(op.clone()) {
                    Ok(op) => {
                        let kind = ExprKind::Binary(op, expr.clone(), rhs);
                        expr = Box::new(Expr::new_with_position(kind, position))
                    }
                    Err(e) => {
                        let e = e.with_position(position);
                        return Err(e);
                    }
                }
            }
            else {
                println!("Not An operator")
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Box<Expr>, Error> {
        let current = self.current_token().clone();
        let position = current.position();
        match current.to_kind() {
            TokenTreeKind::Token(token) => match token {
                Token::Op(Operator::Minus) => {
                    self.consume()?;
                    let operand = self.parse_expr()?;
                    let position = position.extended_to(operand.as_ref());
                    Ok(Box::new(Expr::new_with_position(
                        ExprKind::Unary(UnaryOp::Minus, operand),
                        position,
                    )))
                }
                Token::Op(Operator::Ampersand) => {
                    self.consume()?;
                    let operand = self.parse_expr()?;
                    let operand = self.parse_expr()?;
                    let position = position.extended_to(operand.as_ref());
                    Ok(Box::new(Expr::new_with_position(
                        ExprKind::Unary(UnaryOp::Ampersand, operand),
                        position,
                    )))
                }
                Token::Op(Operator::Bang) => {
                    self.consume()?;
                    let operand = self.parse_expr()?;
                    let operand = self.parse_expr()?;
                    let position = position.extended_to(operand.as_ref());
                    Ok(Box::new(Expr::new_with_position(
                        ExprKind::Unary(UnaryOp::Bang, operand),
                        position,
                    )))
                }
                _ => self.parse_primary(),
            },
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Box<Expr>, Error> {
        self.parse_bottom()
    }

    fn parse_bottom(&mut self) -> Result<Box<Expr>, Error> {
        let current = self.current_token().clone();
        println!("parse_bottom {}", current);
        let position = current.position();
        match current.to_kind() {
            TokenTreeKind::Pair(ctrl, elements) => {
                self.push_tree_stack(ctrl, elements);
                self.consume()?;
                unimplemented!();
            }
            TokenTreeKind::Token(token) => {
                match token {
                    Token::Ident(val) => {
                        let ident = self.parse_ident()?;
                        Ok(Box::new(Expr::new_with_position(ExprKind::Name(ident), position)))
                    },
                    Token::Integer(val) => {
                        self.consume()?;
                        Ok(Box::new(Expr::new_with_position(
                            ExprKind::Integer(val),
                            position,
                        )))
                    },
                    Token::Float(val) => {
                        self.consume()?;
                        Ok(Box::new(Expr::new_with_position(
                            ExprKind::Float(val),
                            position,
                        )))
                    },
                    Token::String(val) => {
                        self.consume()?;
                        Ok(Box::new(Expr::new_with_position(
                            ExprKind::String(val),
                            position,
                        )))
                    },
                    _ => todo!("parse_bottom: unexpected token {}", token),
                }
            }
        }
    }
}
