use crate::error::Error;
use crate::file::File;
use crate::syntax::ast::*;
use crate::syntax::tokenizer::TokenCursor;
use crate::syntax::{Control, Keyword, Operator, PToken, Position, Token};
use crate::syntax::{PairKind, ParsedFile};

use crate::syntax::ast::ExprKind::Call;
use crate::syntax::token::Token::ControlPair;
use std::convert::TryFrom;
use std::vec::IntoIter;

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
    current: Option<PToken<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(file: &'src File) -> Self {
        Self {
            restriction: DEFAULT,
            cursor: TokenCursor::new(file),
            current: None,
        }
    }

    pub fn current_token(&self) -> &PToken<'src> {
        self.current
            .as_ref()
            .expect("Compiler Error: Parser: current token is None")
    }

    fn current_position(&self) -> Position {
        self.current_token().position()
    }

    pub fn consume(&mut self) -> Result<Option<PToken<'src>>, Error> {
        let current = self.current.clone();
        if let Some(res) = self.cursor.next() {
            self.current = Some(res?);
        }
        Ok(current)
    }

    fn is_end_of_pair(&self) -> bool {
        self.current == None
    }

    fn check_for(&self, token: Token) -> bool {
        *self.current_token().token() == token
    }

    fn expect(&mut self, token: Token) -> Result<PToken, Error> {
        if self.check_for(token.clone()) {
            Ok(self.consume()?.unwrap())
        } else {
            Err(
                Error::unexpected_token(token, self.current.as_ref().unwrap().token())
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
        let position = current.position();

        self.consume()?;
        let token = current.to_token();
        if let Token::Ident(value) = token {
            return Ok(Identifier::new_with_position(Ident::from(value), position));
        }

        Err(Error::expecting_identifier(&token).with_position(position))
    }

    pub fn parse_stmt(&mut self) -> Result<Box<Stmt>, Error> {
        let expr = self.parse_expr()?;
        let position = expr.position();
        let kind = StmtKind::Expr(expr);
        Ok(Box::new(Stmt::new_with_position(kind, position)))
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
        println!(
            "parse_assoc_expr: {} -> {}",
            self.current_token(),
            self.current_token().precedence()
        );
        // println!("Position: {}", position);
        while self.current_token().precedence() > min_prec {
            let token_tree = self.current_token().clone();
            let token = token_tree.to_token();
            let lhs_position = expr.position();

            if let Token::Op(op) = token {
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
            } else {
                println!("Not An operator")
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Box<Expr>, Error> {
        let current = self.current_token().clone();
        let position = current.position();
        match current.to_token() {
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
        }
    }

    fn parse_primary(&mut self) -> Result<Box<Expr>, Error> {
        let mut operand = self.parse_bottom()?;
        let position = operand.position();

        loop {
            let current = self.current_token().clone();
            match current.token() {
                Token::Op(Operator::Period) => {
                    self.consume()?;
                    let current_token = self.current_token().clone();
                    match current_token.to_token() {
                        Token::Ident(_) => {
                            let name = self.parse_ident()?;

                            if self.check_for(Token::ControlPair(Control::Paren, PairKind::Open)) {
                                let (actual, end_paren) = self.parse_call_actual()?;
                                let position = position.extended_to_token(end_paren);
                                let kind = ExprKind::Method {
                                    operand: operand.clone(),
                                    name: Box::new(name),
                                    actual,
                                };
                                operand = Box::new(Expr::new_with_position(kind, position));
                            } else {
                                let position = position.extended_to(&name);
                                let kind = ExprKind::Field(operand.clone(), Box::new(name));
                                operand = Box::new(Expr::new_with_position(kind, position));
                            }
                        }
                        _ => {
                            break;
                        }
                    }
                }
                Token::ControlPair(Control::Paren, PairKind::Open) => {
                    let (actual, end_paren) = self.parse_call_actual()?;
                    let position = position.extended_to_token(end_paren);
                    let kind = ExprKind::Call {
                        operand: operand.clone(),
                        actual,
                    };
                    operand = Box::new(Expr::new_with_position(kind, position));
                }
                _ => {
                    break;
                }
            }
        }
        Ok(operand)
    }

    fn parse_bottom(&mut self) -> Result<Box<Expr>, Error> {
        let current = self.current_token().clone();
        println!("parse_bottom {}", current);
        let position = current.position();
        match current.to_token() {
            Token::Ident(val) => {
                let ident = self.parse_ident()?;
                Ok(Box::new(Expr::new_with_position(
                    ExprKind::Name(ident),
                    position,
                )))
            }
            Token::Integer(val) => {
                self.consume()?;
                Ok(Box::new(Expr::new_with_position(
                    ExprKind::Integer(val),
                    position,
                )))
            }
            Token::Float(val) => {
                self.consume()?;
                Ok(Box::new(Expr::new_with_position(
                    ExprKind::Float(val),
                    position,
                )))
            }
            Token::String(val) => {
                self.consume()?;
                Ok(Box::new(Expr::new_with_position(
                    ExprKind::String(val),
                    position,
                )))
            }
            Token::ControlPair(Control::Bracket, PairKind::Open) => {
                let open = self.consume()?.unwrap();
                self.allow_newline()?;

                let position = open.position();
                let stmts = self.parse_inner_pair(
                    |p| p.parse_stmt(),
                    Token::Newline,
                    true,
                    Control::Bracket,
                )?;

                let end = self.expect(Token::ControlPair(Control::Bracket, PairKind::Close))?;
                let position = position.extended_to_token(end);

                let kind = ExprKind::Block(stmts);
                Ok(Box::new(Expr::new_with_position(kind, position)))
            }
            Token::ControlPair(Control::Paren, PairKind::Open) => {
                let open = self.consume()?.unwrap();
                self.allow_newline()?;

                let position = open.position();
                let stmts = self.parse_inner_pair(
                    |p| p.parse_expr(),
                    Token::Newline,
                    true,
                    Control::Bracket,
                )?;

                let end = self.expect(Token::ControlPair(Control::Paren, PairKind::Close))?;
                let position = position.extended_to_token(end);

                let kind = ExprKind::Tuple(stmts);
                Ok(Box::new(Expr::new_with_position(kind, position)))
            }
            e => todo!("parse_bottom: unexpected token {}", e),
        }
    }

    fn parse_inner_pair<E, F>(
        &mut self,
        element: F,
        sep: Token,
        allow_trailing: bool,
        end_control: Control,
    ) -> Result<Vec<Box<E>>, Error>
    where
        E: Node,
        F: Fn(&mut Self) -> Result<Box<E>, Error>,
    {
        let mut res = Vec::new();
        let mut expect_following = false;
        loop {
            let at_end = self.check_for(Token::ControlPair(end_control, PairKind::Close));

            if expect_following && !allow_trailing && at_end {
                // error
                panic!();
                break;
            }

            if at_end {
                break;
            }
            expect_following = false;

            let elem = element(self)?;
            res.push(elem);

            if self.check_for(sep.clone()) {
                self.consume()?;
                expect_following = true;
            } else {
                break;
            }
        }
        Ok(res)
    }

    fn parse_call_actual(&mut self) -> Result<(Vec<Box<Expr>>, PToken), Error> {
        self.expect(Token::ControlPair(Control::Paren, PairKind::Open))?;

        let actual = self.parse_inner_pair(
            |p| p.parse_expr(),
            Token::Op(Operator::Comma),
            true,
            Control::Paren,
        )?;

        let close_paren = self.expect(Token::ControlPair(Control::Paren, PairKind::Close))?;

        Ok((actual, close_paren))
    }

    fn allow_newline(&mut self) -> Result<(), Error> {
        while self.check_for(Token::Newline) {
            self.consume()?;
        }
        Ok(())
    }
}
