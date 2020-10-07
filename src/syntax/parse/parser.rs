use crate::error::Error;
use crate::syntax::ast::*;
use crate::syntax::tokenizer::TokenCursor;
use crate::syntax::{Control, Keyword, Operator, PToken, Position, Token};
use crate::syntax::{PairKind, ParsedFile};
use crate::system::File;

use std::convert::TryFrom;

type Restriction = usize;
const DEFAULT: Restriction = 0;
const TYPE_EXPR: Restriction = 1 << 0;
const NO_STRUCT_EXPR: Restriction = 1 << 1;
// const : Restriction = 1;
// const : Restriction = 2;
// const : Restriction = 4;
// const : Restriction = 8;
// const : Restriction = 16;

pub struct Parser<'src> {
    restriction: Restriction,
    file: &'src File,
    cursor: TokenCursor<'src>,
    current: Option<PToken<'src>>,
    peek: Option<PToken<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(file: &'src File) -> Self {
        Self {
            restriction: DEFAULT,
            file,
            cursor: TokenCursor::new(file),
            current: None,
            peek: None,
        }
    }

    pub fn init(&mut self) -> Result<(), Error> {
        self.consume()?;
        self.consume()?;
        Ok(())
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
        self.current = self.peek.clone();

        if let Some(res) = self.cursor.next() {
            self.peek = Some(res?);
        } else {
            self.peek = None;
        }

        Ok(current)
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

    fn expected(&self, token: Token) -> Result<(), Error> {
        if self.check_for(token.clone()) {
            Ok(())
        } else {
            Err(
                Error::unexpected_token(token, self.current.as_ref().unwrap().token())
                    .with_position(self.current_position()),
            )
        }
    }

    fn peek_for(&self, token: Token) -> bool {
        self.peek.as_ref().map(|t| t.token()) == Some(&token)
    }

    fn check_for_res(&self, res: Restriction) -> bool {
        (self.restriction & res) == res
    }

    fn parse_ident(&mut self) -> Result<Identifier, Error> {
        let current = self.current_token().clone();
        let position = current.position();

        self.consume()?;
        let token = current.to_token();
        if let Token::Ident(value) = token {
            Ok(Identifier::new_with_position(Ident::from(value), position))
        } else {
            Err(Error::expecting_identifier(&token).with_position(position))
        }
    }

    //----------------------------------------------------------------------------------------------

    pub fn parse_file(&mut self) -> Result<ParsedFile, Error> {
        let mut parsed_file = ParsedFile::new(self.file.id());
        while let Some(current) = self.current.clone() {
            if current.is_eof() {
                break;
            }

            let stmt = self.parse_stmt()?;
            if !stmt.kind().is_empty() {
                parsed_file.push_stmt(stmt);
            }

            if self.check_for(Token::Newline) {
                self.consume()?;
            } else {
                break;
            }
        }
        Ok(parsed_file)
    }

    pub fn parse_stmt(&mut self) -> Result<Box<Stmt>, Error> {
        let current = self.current_token().clone();
        let position = current.position();
        match current.to_token() {
            Token::Kw(Keyword::Pub)
            | Token::Kw(Keyword::Struct)
            | Token::Kw(Keyword::Fn)
            | Token::Kw(Keyword::Let)
            | Token::Kw(Keyword::Mut) => {
                let item = self.parse_item()?;
                let position = item.position();
                let kind = StmtKind::Item(item);
                Ok(Box::new(Stmt::new_with_position(kind, position)))
            }
            Token::Newline => Ok(Box::new(Stmt::new_with_position(StmtKind::Empty, position))),
            _ => {
                let expr = self.parse_expr()?;
                let position = expr.position();
                let kind = StmtKind::Expr(expr);
                Ok(Box::new(Stmt::new_with_position(kind, position)))
            }
        }
    }

    //----------------------------------------------------------------------------------------------

    fn parse_expr(&mut self) -> Result<Box<Expr>, Error> {
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

        if self.check_for_res(TYPE_EXPR) {
            return Ok(expr);
        }
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
            t @ Token::Op(Operator::Minus) => {
                self.consume()?;
                let operand = self.parse_expr()?;
                let position = position.extended_to(operand.as_ref());
                if self.check_for_res(TYPE_EXPR) {
                    let kind = Error::invalid_type_expression(&t).with_position(position);
                    Err(kind)
                } else {
                    Ok(Box::new(Expr::new_with_position(
                        ExprKind::Unary(UnaryOp::Minus, operand),
                        position,
                    )))
                }
            }
            t @ Token::Op(Operator::Ampersand) => {
                self.consume()?;
                let operand = self.parse_expr()?;
                let position = position.extended_to(operand.as_ref());
                if self.check_for_res(TYPE_EXPR) {
                    let kind = Error::invalid_type_expression(&t).with_position(position);
                    Err(kind)
                } else {
                    Ok(Box::new(Expr::new_with_position(
                        ExprKind::Unary(UnaryOp::Ampersand, operand),
                        position,
                    )))
                }
            }
            t @ Token::Op(Operator::Bang) => {
                self.consume()?;
                let operand = self.parse_expr()?;
                let position = position.extended_to(operand.as_ref());
                if self.check_for_res(TYPE_EXPR) {
                    let kind = Error::invalid_type_expression(&t).with_position(position);
                    Err(kind)
                } else {
                    Ok(Box::new(Expr::new_with_position(
                        ExprKind::Unary(UnaryOp::Bang, operand),
                        position,
                    )))
                }
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
                Token::ControlPair(Control::Bracket, PairKind::Open) => {
                    if self.check_for_res(NO_STRUCT_EXPR) {
                        break;
                        // let err = Error::invalid_context_struct_expr()
                        //     .with_position(self.current_position());
                        // return Err(err);
                    }

                    self.consume()?;

                    let mut fields = vec![];
                    loop {
                        if self.check_for(Token::ControlPair(Control::Bracket, PairKind::Close)) {
                            break;
                        }

                        let field = self.parse_struct_expr_field()?;
                        fields.push(field);

                        if self.check_for(Token::Op(Operator::Comma)) {
                            self.consume()?;
                        } else {
                            break;
                        }
                    }

                    let end = self.expect(Token::ControlPair(Control::Bracket, PairKind::Close))?;

                    operand = Box::new(Expr::new_with_position(
                        ExprKind::StructExpr {
                            name: operand.clone(),
                            fields,
                        },
                        position.extended_to_token(end),
                    ))
                }
                _ => {
                    break;
                }
            }
        }
        Ok(operand)
    }

    fn parse_struct_expr_field(&mut self) -> Result<StructExprField, Error> {
        if self.peek_for(Token::Op(Operator::Colon)) {
            let ident = self.parse_ident()?;
            self.expect(Token::Op(Operator::Colon))?;
            let expr = self.parse_expr()?;
            Ok(StructExprField::Bind(ident, expr))
        } else {
            let expr = self.parse_expr()?;
            Ok(StructExprField::Field(expr))
        }
    }

    fn parse_bottom(&mut self) -> Result<Box<Expr>, Error> {
        let current = self.current_token().clone();
        println!("parse_bottom {}", current);
        let position = current.position();
        match current.to_token() {
            Token::Ident(_) => {
                let ident = self.parse_ident()?;
                Ok(Box::new(Expr::new_with_position(
                    ExprKind::Name(ident),
                    position,
                )))
            }
            Token::Integer(val) => {
                if self.check_for_res(TYPE_EXPR) {
                    let t = Token::Integer(val);
                    let kind = Error::invalid_type_expression(&t).with_position(position);
                    Err(kind)
                } else {
                    self.consume()?;
                    Ok(Box::new(Expr::new_with_position(
                        ExprKind::Integer(val),
                        position,
                    )))
                }
            }
            Token::Float(val) => {
                if self.check_for_res(TYPE_EXPR) {
                    let t = Token::Float(val);
                    let kind = Error::invalid_type_expression(&t).with_position(position);
                    Err(kind)
                } else {
                    self.consume()?;
                    Ok(Box::new(Expr::new_with_position(
                        ExprKind::Float(val),
                        position,
                    )))
                }
            }
            Token::String(val) => {
                if self.check_for_res(TYPE_EXPR) {
                    let t = Token::String(val);
                    let kind = Error::invalid_type_expression(&t).with_position(position);
                    Err(kind)
                } else {
                    self.consume()?;
                    Ok(Box::new(Expr::new_with_position(
                        ExprKind::String(val),
                        position,
                    )))
                }
            }
            t @ Token::ControlPair(Control::Bracket, PairKind::Open) => {
                if self.check_for_res(TYPE_EXPR) {
                    let kind = Error::invalid_type_expression(&t).with_position(position);
                    Err(kind)
                } else {
                    let open = self.consume()?.unwrap();
                    self.allow_newline()?;

                    let position = open.position();
                    let stmts = self.parse_inner_pair(
                        |p| p.parse_stmt(),
                        Token::Newline,
                        true,
                        false,
                        Control::Bracket,
                    )?;

                    let end = self.expect(Token::ControlPair(Control::Bracket, PairKind::Close))?;
                    let position = position.extended_to_token(end);

                    let kind = ExprKind::Block(stmts);
                    Ok(Box::new(Expr::new_with_position(kind, position)))
                }
            }
            t @ Token::ControlPair(Control::Paren, PairKind::Open) => {
                if self.check_for_res(TYPE_EXPR) {
                    let kind = Error::invalid_type_expression(&t).with_position(position);
                    return Err(kind);
                }

                let open = self.consume()?.unwrap();
                self.allow_newline()?;

                let position = open.position();
                let stmts = self.parse_inner_pair(
                    |p| p.parse_expr(),
                    Token::Newline,
                    true,
                    false,
                    Control::Bracket,
                )?;

                let end = self.expect(Token::ControlPair(Control::Paren, PairKind::Close))?;
                let position = position.extended_to_token(end);

                let kind = ExprKind::Tuple(stmts);
                Ok(Box::new(Expr::new_with_position(kind, position)))
            }
            Token::Kw(Keyword::SelfType) => {
                let position = self.current_position();
                self.consume()?;
                Ok(Box::new(Expr::new_with_position(
                    ExprKind::SelfType,
                    position,
                )))
            }
            Token::Kw(Keyword::SelfLit) => {
                let position = self.current_position();
                self.consume()?;
                Ok(Box::new(Expr::new_with_position(
                    ExprKind::SelfLit,
                    position,
                )))
            }
            t @ Token::Kw(Keyword::If)
            | t @ Token::Kw(Keyword::While)
            | t @ Token::Kw(Keyword::Loop)
            | t @ Token::Kw(Keyword::For) => self.parse_branching(t),
            e => todo!("parse_bottom: unexpected token {}", e),
        }
    }

    fn parse_inner_pair<E, F>(
        &mut self,
        element: F,
        sep: Token,
        allow_trailing: bool,
        remove_newlines: bool,
        end_control: Control,
    ) -> Result<Vec<Box<E>>, Error>
    where
        F: Fn(&mut Self) -> Result<Box<E>, Error>,
    {
        let mut res = vec![];
        let mut expect_following = false;
        loop {
            let at_end = self.check_for(Token::ControlPair(end_control, PairKind::Close));

            if expect_following && !allow_trailing && at_end {
                // error
                panic!();
                // break;
            }

            if at_end {
                break;
            }

            let elem = element(self)?;
            res.push(elem);

            if self.check_for(sep.clone()) {
                self.consume()?;
                expect_following = true;
                if remove_newlines {
                    self.allow_newline()?;
                }
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
            true,
            Control::Paren,
        )?;

        let close_paren = self.expect(Token::ControlPair(Control::Paren, PairKind::Close))?;

        Ok((actual, close_paren))
    }

    fn parse_branching(&mut self, token: Token) -> Result<Box<Expr>, Error> {
        if self.check_for_res(TYPE_EXPR) {
            let kind =
                Error::invalid_type_expression(&token).with_position(self.current_position());
            Err(kind)
        } else {
            let position = self.current_position();
            self.consume()?;
            match token {
                Token::Kw(Keyword::Loop) => self.parse_loop(position),
                Token::Kw(Keyword::While) => self.parse_while(position),
                Token::Kw(Keyword::For) => self.parse_for(position),
                Token::Kw(Keyword::If) => self.parse_if(position),
                _ => unreachable!(),
            }
        }
    }

    fn parse_loop(&mut self, position: Position) -> Result<Box<Expr>, Error> {
        self.expected(Token::ControlPair(Control::Bracket, PairKind::Open))?;
        self.allow_newline()?;

        let expr = self.parse_expr()?;

        let end = self.expect(Token::ControlPair(Control::Bracket, PairKind::Close))?;
        Ok(Box::new(Expr::new_with_position(
            ExprKind::Loop(expr),
            position.extended_to_token(end),
        )))
    }

    fn parse_while(&mut self, position: Position) -> Result<Box<Expr>, Error> {
        let cond = self.parse_expr()?;
        self.expected(Token::ControlPair(Control::Bracket, PairKind::Open))?;
        self.allow_newline()?;

        let expr = self.parse_expr()?;

        let end = self.expect(Token::ControlPair(Control::Bracket, PairKind::Close))?;
        Ok(Box::new(Expr::new_with_position(
            ExprKind::While(cond, expr),
            position.extended_to_token(end),
        )))
    }

    fn parse_for(&mut self, position: Position) -> Result<Box<Expr>, Error> {
        let element = self.parse_ident()?;

        self.expect(Token::Kw(Keyword::In))?;

        let expr = self.parse_expr_with_res(NO_STRUCT_EXPR)?;

        self.expected(Token::ControlPair(Control::Bracket, PairKind::Open))?;
        self.allow_newline()?;

        let body = self.parse_expr()?;

        let end = self.expect(Token::ControlPair(Control::Bracket, PairKind::Close))?;
        Ok(Box::new(Expr::new_with_position(
            ExprKind::For {
                element,
                expr,
                body,
            },
            position.extended_to_token(end),
        )))
    }

    fn parse_if(&mut self, position: Position) -> Result<Box<Expr>, Error> {
        let cond = self.parse_expr()?;

        self.expected(Token::ControlPair(Control::Bracket, PairKind::Open))?;
        self.allow_newline()?;

        let body = self.parse_expr()?;

        self.expect(Token::ControlPair(Control::Bracket, PairKind::Close))?;

        let else_if = if self.check_for(Token::Kw(Keyword::Elif)) {
            let position = self.current_position();
            self.consume()?;
            Some(self.parse_if(position)?)
        } else if self.check_for(Token::Kw(Keyword::Else)) {
            self.consume()?;
            Some(self.parse_expr()?)
        } else {
            None
        };

        let position = if let Some(expr) = else_if.as_ref() {
            position.extended_to(expr.as_ref())
        } else {
            position
        };

        Ok(Box::new(Expr::new_with_position(
            ExprKind::If {
                cond,
                body,
                else_if,
            },
            position,
        )))
    }

    fn allow_newline(&mut self) -> Result<(), Error> {
        while self.check_for(Token::Newline) {
            self.consume()?;
        }
        Ok(())
    }

    //----------------------------------------------------------------------------------------------

    fn parse_spec(&mut self) -> Result<Box<Spec>, Error> {
        let current = self.current_token();
        let position = current.position();
        println!("parse_spec: {}", current);
        match current.token() {
            Token::Ident(_) => {
                std::mem::forget(current);
                let expr = self.parse_expr_with_res(TYPE_EXPR | NO_STRUCT_EXPR)?;
                let position = expr.position();
                let kind = SpecKind::Named(expr);
                Ok(Box::new(Spec::new_with_position(kind, position)))
            }
            Token::ControlPair(Control::Paren, PairKind::Open) => {
                self.consume()?;
                let inner = self.parse_inner_pair(
                    |p| p.parse_spec(),
                    Token::Op(Operator::Comma),
                    true,
                    true,
                    Control::Paren,
                )?;

                let end = self.expect(Token::ControlPair(Control::Paren, PairKind::Close))?;
                Ok(Box::new(Spec::new_with_position(
                    SpecKind::Tuple(inner),
                    position.extended_to_token(end),
                )))
            }
            Token::Kw(Keyword::SelfType) => {
                self.consume()?;
                Ok(Box::new(Spec::new_with_position(
                    SpecKind::SelfType,
                    position,
                )))
            }
            _ => Ok(Box::new(Spec::new_with_position(SpecKind::Infer, position))),
        }
    }

    //----------------------------------------------------------------------------------------------

    fn parse_item(&mut self) -> Result<Box<Item>, Error> {
        let vis = self.parse_possible_vis()?;
        let current = self.current_token();
        match current.token() {
            Token::Kw(Keyword::Struct) => {
                std::mem::forget(current);
                self.parse_struct(vis)
            }
            Token::Kw(Keyword::Fn) => {
                std::mem::forget(current);
                self.parse_function(vis)
            }
            Token::Kw(Keyword::Let) | Token::Kw(Keyword::Mut) => {
                std::mem::forget(current);
                self.parse_variable(vis)
            }
            _ => panic!(),
        }
    }

    fn parse_possible_vis(&mut self) -> Result<Visibility, Error> {
        if self.check_for(Token::Kw(Keyword::Pub)) {
            self.consume()?;
            Ok(Visibility::Public)
        } else {
            Ok(Visibility::Private)
        }
    }

    fn parse_struct(&mut self, vis: Visibility) -> Result<Box<Item>, Error> {
        let position = self.current_position();
        self.expect(Token::Kw(Keyword::Struct))?;

        let name = self.parse_ident()?;

        self.expect(Token::ControlPair(Control::Bracket, PairKind::Open))?;
        self.allow_newline()?;

        let fields = self.parse_inner_pair(
            |p| {
                p.allow_newline()?;
                p.parse_field()
            },
            Token::Newline,
            true,
            true,
            Control::Bracket,
        )?;

        let end = self.expect(Token::ControlPair(Control::Bracket, PairKind::Close))?;

        Ok(Box::new(Item::new_with_position(
            ItemKind::Struct { name, vis, fields },
            position.extended_to_token(end),
        )))
    }

    fn parse_function(&mut self, vis: Visibility) -> Result<Box<Item>, Error> {
        let position = self.current_position();
        self.expect(Token::Kw(Keyword::Fn))?;
        let name = self.parse_ident()?;

        self.expect(Token::ControlPair(Control::Paren, PairKind::Open))?;
        self.allow_newline()?;

        let params = self.parse_inner_pair(
            |p| {
                p.allow_newline()?;
                p.parse_param()
            },
            Token::Op(Operator::Comma),
            false,
            true,
            Control::Paren,
        )?;

        self.expect(Token::ControlPair(Control::Paren, PairKind::Close))?;

        let ret = self.parse_spec()?;
        let (body, position) = if self.check_for(Token::Op(Operator::Equal)) {
            self.consume()?;
            let expr = self.parse_expr()?;
            let position = position.extended_to(expr.as_ref());
            (FunctionBody::Expression(expr), position)
        } else if self.check_for(Token::ControlPair(Control::Bracket, PairKind::Open)) {
            let expr = self.parse_expr()?;
            let position = position.extended_to(expr.as_ref());
            (FunctionBody::Block(expr), position)
        } else {
            return Err(Error::unexpected_token_multi(
                vec![
                    Token::Op(Operator::Equal),
                    Token::ControlPair(Control::Bracket, PairKind::Open),
                ],
                self.current_token().token(),
            )
            .with_position(self.current_position()));
        };

        let kind = ItemKind::Function {
            vis,
            name,
            params,
            ret,
            body,
        };

        Ok(Box::new(Item::new_with_position(kind, position)))
    }

    fn parse_param(&mut self) -> Result<Box<Item>, Error> {
        let (names, spec, init, position) = self.parse_field_param()?;

        Ok(Box::new(Item::new_with_position(
            ItemKind::Param { names, spec, init },
            position,
        )))
    }

    fn parse_field(&mut self) -> Result<Box<Item>, Error> {
        let vis = self.parse_possible_vis()?;
        if self.check_for(Token::Kw(Keyword::Fn)) {
            self.parse_function(vis)
        } else {
            let (names, spec, init, position) = self.parse_field_param()?;
            Ok(Box::new(Item::new_with_position(
                ItemKind::Field {
                    vis,
                    names,
                    spec,
                    init,
                },
                position,
            )))
        }
    }

    fn parse_field_param(
        &mut self,
    ) -> Result<
        (
            Vec<Identifier>,
            Option<Box<Spec>>,
            Option<Box<Expr>>,
            Position,
        ),
        Error,
    > {
        let mut names = vec![];
        let mut position = self.current_position();
        loop {
            let ident = self.parse_ident()?;
            names.push(ident);

            if self.check_for(Token::Op(Operator::Comma)) {
                self.consume()?;
            } else {
                break;
            }
        }

        let spec = if !self.check_for(Token::Op(Operator::Equal)) {
            let spec = self.parse_spec()?;
            position = position.extended_to(spec.as_ref());
            Some(spec)
        } else {
            None
        };

        let init = if self.check_for(Token::Op(Operator::Equal)) {
            self.consume()?;
            let expr = self.parse_expr()?;
            position = position.extended_to(expr.as_ref());
            Some(expr)
        } else {
            None
        };

        Ok((names, spec, init, position))
    }

    fn parse_variable(&mut self, vis: Visibility) -> Result<Box<Item>, Error> {
        let mutable = self.check_for(Token::Kw(Keyword::Mut));

        if !self.check_for(Token::Kw(Keyword::Mut)) && !self.check_for(Token::Kw(Keyword::Let)) {
            panic!(
                "Compiler Error: calling parse_variable on invalid leading token: {}",
                self.current_token()
            );
        }

        self.consume()?;

        let mut position = self.current_position();

        let name = self.parse_ident()?;

        let spec = if !self.check_for(Token::Op(Operator::Equal)) {
            let spec = self.parse_spec()?;
            position = position.extended_to(spec.as_ref());
            Some(spec)
        } else {
            None
        };

        let init = if self.check_for(Token::Op(Operator::Equal)) {
            self.consume()?;
            let expr = self.parse_expr()?;
            position = position.extended_to(expr.as_ref());
            Some(expr)
        } else {
            None
        };

        Ok(Box::new(Item::new_with_position(
            ItemKind::Variable {
                vis,
                mutable,
                name,
                init,
                spec,
            },
            position,
        )))
    }
}
