use std::convert::TryFrom;
use std::str::Chars;

use crate::error::Error;
use crate::syntax::token::{Control, Keyword, Operator, PToken, Token};
use crate::syntax::{Coord, FilePos, PairKind, Position, Span};
use crate::system::{File, FileId};

pub struct Lexer<'src> {
    /// the file to be processed
    file: &'src File,
    /// an iterator over the characters in the string.
    chars: Chars<'src>,
    /// current character
    ch: Option<char>,
    /// the accumulated span of the current scan
    span: Span,
    // the file position
    file_pos: FilePos,
}

impl<'src> Lexer<'src> {
    pub fn new(file: &'src File) -> Self {
        let mut lexer = Self {
            file,
            chars: file.content().chars(),
            ch: None,
            span: Span(0, 0),
            file_pos: FilePos::default(),
        };
        lexer.init();

        lexer
    }

    pub fn file(&self) -> FileId {
        self.file.id()
    }

    fn init(&mut self) {
        self.ch = self.chars.next();

        self.file_pos = FilePos::new(Coord(1, 1), Coord(1, 1));
        self.span.1 = 0;
    }

    fn advance(&mut self) {
        if self.ch.is_none() {
            return;
        }

        // handle new line
        if self.ch == Some('\n') {
            self.file_pos.end.0 += 1;
            self.file_pos.end.1 = 0;
        }

        // advance the span to include the current character.
        if let Some(ch) = self.ch {
            self.span.1 += ch.len_utf8();
        }

        self.ch = self.chars.next();
        self.file_pos.end.1 += 1;
    }

    fn complete_token(&mut self, token: Token<'src>) -> PToken<'src> {
        let span = self.span;
        let file_pos = FilePos::new(self.file_pos.start, self.file_pos.end);

        let source = if token.is_nl() {
            "\\n"
        } else {
            self.examine_span()
        };
        self.file_pos.start = self.file_pos.end;
        self.span.0 = self.span.1;
        PToken::new(source, token, Position::new(span, file_pos, self.file()))
    }

    fn skip_whitespace(&mut self) {
        // move past all of the whitespace
        let whitespace_check = |ch: char| (ch.is_whitespace() || ch == '\r') && ch != '\n';
        if self.check(whitespace_check) {
            while self.check(whitespace_check) {
                self.advance();
            }

            // update the spans and locations to the beginning of the next token.
            self.file_pos.start = self.file_pos.end;
            self.span.0 = self.span.1;
        }
    }

    /// generic checking function to a given predicate.
    fn check<P>(&self, p: P) -> bool
    where
        P: Fn(char) -> bool,
    {
        self.ch.map_or(false, p)
    }
    /// check whether the current character is a specific character
    fn check_for(&self, ch: char) -> bool {
        self.check(|c| c == ch)
    }

    /// retrieves the sub string of the current span
    fn examine_span(&self) -> &'src str {
        &self.file.content()[self.span.0..self.span.1]
    }

    /// If is use this interface, I do not need the Error token.
    pub fn next(&mut self) -> Result<PToken<'src>, Error> {
        self.scan()
    }

    fn scan_ident_or_keyword(&mut self) -> Result<Token<'src>, Error> {
        //let is_unicode = |ch: char| (!ch.is_control() && !ch.is_whitespace() && !ch.is_alphanumeric());
        let is_unicode = |_ch: char| false;
        while self.check(|ch| ch.is_alphanumeric() || ch == '_' || is_unicode(ch)) {
            self.advance()
        }

        let token_str = self.examine_span();

        let kind = match Keyword::try_from(token_str) {
            Ok(kw) => Token::Kw(kw),
            _ => Token::Ident(token_str),
        };
        Ok(kind)
        // Ok(self.complete_token(kind))
    }

    fn scan_number(&mut self) -> Result<Token<'src>, Error> {
        while self.check(char::is_numeric) {
            self.advance();
        }

        let mut is_float = false;

        if self.check_for('.') {
            self.advance();
            if self.check(|e| e.is_numeric() || e == 'e' || e == 'E') {
                is_float = true;
                self.advance();
            } else {
                let value = self.examine_span();
                let value = value.parse::<u64>().unwrap();
                let kind = Token::Integer(value);

                return Ok(kind);
            }

            while self.check(char::is_numeric) {
                self.advance();
            }
        }

        if self.check_for('e') || self.check_for('E') {
            is_float = true;
            self.advance();

            if self.check_for('-') || self.check_for('+') {
                self.advance();
            }

            while self.check(char::is_numeric) {
                self.advance();
            }
        }

        let value = self.examine_span();

        if is_float {
            let value = value.parse::<f64>().unwrap();
            let kind = Token::Float(value.into());
            Ok(kind)
        } else {
            let value = value.parse::<u64>().unwrap();
            let kind = Token::Integer(value);
            Ok(kind)
        }
    }

    fn validate_escape(&mut self) -> Result<char, Error> {
        assert!(self.check_for('\\'));
        self.advance();

        if self.ch.is_none() {
            // let (line, column) = self.coords.start();
            return Err(Error::unexpected_eof().with_position(Position::new(
                self.span,
                self.file_pos,
                self.file(),
            )));
        }

        let val = match self.ch.unwrap() {
            'n' => 10u8,
            't' => 9u8,
            '\\' => '\\' as u8,
            _ => {
                return Err(Error::unknown_escape_character(self.ch.unwrap())
                    .with_position(Position::new(self.span, self.file_pos, self.file())));
            }
        };
        Ok(val as char)
    }

    fn scan_string(&mut self) -> Result<Token<'src>, Error> {
        let _start_index = self.span.0 + 1;
        let mut value = String::new();

        while self.ch.is_some() && !self.check_for('"') {
            let ch = if self.check_for('\\') {
                self.validate_escape()?
            } else {
                self.ch.unwrap()
            };

            value.push(ch);

            self.advance();
        }

        if self.ch.is_none() {
            return Err(Error::expected_quotation().with_position(Position::new(self.span, self.file_pos, self.file())));
        }

        self.advance();

        let kind = Token::String(value);

        Ok(kind)
    }

    fn scan(&mut self) -> Result<PToken<'src>, Error> {
        self.skip_whitespace();
        if self.ch.is_none() {
            Ok(self.complete_token(Token::Eof))
        } else if self.check(char::is_alphabetic) {
            let t = self.scan_ident_or_keyword()?;
            Ok(self.complete_token(t))
        } else if self.check(char::is_numeric) {
            let t = self.scan_number()?;
            Ok(self.complete_token(t))
        } else {
            let ch = self.ch.unwrap();
            self.advance();

            let kind = match ch {
                '\n' => Token::Newline,
                '{' => Token::ControlPair(Control::Bracket, PairKind::Open),
                '}' => Token::ControlPair(Control::Bracket, PairKind::Close),
                '(' => Token::ControlPair(Control::Paren, PairKind::Open),
                ')' => Token::ControlPair(Control::Paren, PairKind::Close),
                '[' => Token::ControlPair(Control::Brace, PairKind::Open),
                ']' => Token::ControlPair(Control::Brace, PairKind::Close),
                '"' => self.scan_string()?,
                '.' => Token::Op(Operator::Period),
                ',' => Token::Op(Operator::Comma),
                ':' => Token::Op(Operator::Colon),
                ';' => Token::Op(Operator::Semicolon),
                '+' => {
                    if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::PlusEq)
                    } else {
                        Token::Op(Operator::Plus)
                    }
                }
                '-' => {
                    if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::MinusEq)
                    } else {
                        Token::Op(Operator::Minus)
                    }
                }
                '*' => {
                    if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::AstriskEq)
                    } else {
                        Token::Op(Operator::Astrick)
                    }
                }
                '/' => {
                    if self.check_for('/') {
                        while !self.check_for('\n') {
                            self.advance();
                        }
                        let value = self.examine_span();
                        Token::Comment(value.to_owned())
                    } else if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::SlashEq)
                    } else {
                        Token::Op(Operator::Slash)
                    }
                }
                '&' => {
                    if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::AmpersandEq)
                    } else {
                        Token::Op(Operator::Ampersand)
                    }
                }
                '|' => {
                    if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::PipeEq)
                    } else {
                        Token::Op(Operator::Pipe)
                    }
                }
                '%' => {
                    if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::PercentEq)
                    } else {
                        Token::Op(Operator::Percent)
                    }
                }

                '=' => {
                    if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::EqualEqual)
                    } else {
                        Token::Op(Operator::Equal)
                    }
                }
                '<' => {
                    if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::LessEq)
                    } else if self.check_for('<') {
                        self.advance();

                        if self.check_for('=') {
                            self.advance();
                            Token::Op(Operator::LessLessEq)
                        } else {
                            Token::Op(Operator::LessLess)
                        }
                    } else {
                        Token::Op(Operator::Less)
                    }
                }
                '>' => {
                    if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::GreaterEq)
                    } else if self.check_for('>') {
                        self.advance();

                        if self.check_for('=') {
                            self.advance();
                            Token::Op(Operator::GreaterGreaterEq)
                        } else {
                            Token::Op(Operator::GreaterGreater)
                        }
                    } else {
                        Token::Op(Operator::Greater)
                    }
                }
                '!' => {
                    if self.check_for('=') {
                        self.advance();
                        Token::Op(Operator::BangEqual)
                    } else {
                        Token::Op(Operator::Bang)
                    }
                }
                _ => {
                    return Err(Error::invalid_character(ch).with_position(Position::new(
                        self.span,
                        self.file_pos,
                        self.file(),
                    )));
                }
            };

            Ok(self.complete_token(kind))
        }
    }
}

pub struct TokenCursor<'src> {
    lexer: Lexer<'src>,
    is_eof: bool,
}

impl<'src> TokenCursor<'src> {
    pub fn new(file: &'src File) -> Self {
        Self {
            lexer: Lexer::new(file),
            is_eof: false,
        }
    }

    pub fn file(&self) -> FileId {
        self.lexer.file()
    }
}

impl<'src> std::iter::Iterator for TokenCursor<'src> {
    type Item = Result<PToken<'src>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_eof {
            None
        } else {
            let token = self.lexer.next();
            if token.is_ok() {
                if token.as_ref().unwrap().is_eof() {
                    self.is_eof = true;
                }
            }
            Some(token)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::token::{Operator, PToken};
    use crate::syntax::tokenizer::Lexer;
    use crate::syntax::{Coord, FilePos, Keyword, Position, Span, Token};
    use crate::system::{File, FileId};

    #[test]
    fn test_simple_op_tokenizer() {
        let input = "+";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            tokenizer.scan().unwrap(),
            PToken::new(
                "+",
                Token::Op(Operator::Plus),
                Position::new(
                    Span(0, 1),
                    FilePos::new(Coord(1, 1), Coord(1, 2)),
                    FileId(0),
                ),
            )
        );
    }

    #[test]
    fn test_complex_op_tokenizer() {
        let input = "<<";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            tokenizer.scan().unwrap(),
            PToken::new(
                "<<",
                Token::Op(Operator::LessLess),
                Position::new(
                    Span(0, 2),
                    FilePos::new(Coord(1, 1), Coord(1, 3)),
                    FileId(0),
                ),
            )
        );
    }

    #[test]
    fn test_compound_op_tokenizer() {
        let input = ">>=";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            tokenizer.scan().unwrap(),
            PToken::new(
                ">>=",
                Token::Op(Operator::GreaterGreaterEq),
                Position::new(
                    Span(0, 3),
                    FilePos::new(Coord(1, 1), Coord(1, 4)),
                    FileId(0),
                ),
            )
        );
    }

    #[test]
    fn test_integer_tokenizer() {
        let input = "123";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            tokenizer.scan().unwrap(),
            PToken::new(
                "123",
                Token::Integer(123),
                Position::new(
                    Span(0, 3),
                    FilePos::new(Coord(1, 1), Coord(1, 4)),
                    FileId(0),
                ),
            )
        );
    }

    #[test]
    fn test_float_tokenizer() {
        let input = "1.3";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            tokenizer.scan().unwrap(),
            PToken::new(
                "1.3",
                Token::Float(1.3.into()),
                Position::new(
                    Span(0, 3),
                    FilePos::new(Coord(1, 1), Coord(1, 4)),
                    FileId(0),
                ),
            )
        );
    }

    #[test]
    fn test_complex_float_tokenizer() {
        let input = "1.3e-5";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            tokenizer.scan().unwrap(),
            PToken::new(
                "1.3e-5",
                Token::Float(0.000013.into()),
                Position::new(
                    Span(0, 6),
                    FilePos::new(Coord(1, 1), Coord(1, 7)),
                    FileId(0),
                ),
            )
        );
    }

    #[test]
    fn test_string_tokenizer() {
        let input = "\"test\"";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            tokenizer.scan().unwrap(),
            PToken::new(
                "\"test\"",
                Token::String("test".to_string()),
                Position::new(
                    Span(0, 6),
                    FilePos::new(Coord(1, 1), Coord(1, 7)),
                    FileId(0),
                ),
            )
        );
    }

    #[test]
    fn test_ident_tokenizer() {
        let input = ">>=";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            tokenizer.scan().unwrap(),
            PToken::new(
                ">>=",
                Token::Op(Operator::GreaterGreaterEq),
                Position::new(
                    Span(0, 3),
                    FilePos::new(Coord(1, 1), Coord(1, 4)),
                    FileId(0),
                ),
            )
        );
    }

    #[test]
    fn test_keyword_tokenizer() {
        let input = "let";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            tokenizer.scan().unwrap(),
            PToken::new(
                "let",
                Token::Kw(Keyword::Let),
                Position::new(
                    Span(0, 3),
                    FilePos::new(Coord(1, 1), Coord(1, 4)),
                    FileId(0),
                ),
            )
        );
    }

    #[test]
    fn test_new_line_tokenizer() {
        let input = "\n";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            tokenizer.scan().unwrap(),
            PToken::new(
                "\\n",
                Token::Newline,
                Position::new(
                    Span(0, 1),
                    FilePos::new(Coord(1, 1), Coord(2, 1)),
                    FileId(0),
                ),
            )
        );
    }

    #[test]
    fn multi_tokens_tokenizer() {
        let input = "+.";
        let input = File::raw_test(input.to_string());
        let mut tokenizer = Lexer::new(&input);
        assert_eq!(
            [tokenizer.scan().unwrap(), tokenizer.scan().unwrap()],
            [
                PToken::new(
                    "+",
                    Token::Op(Operator::Plus),
                    Position::new(
                        Span(0, 1),
                        FilePos::new(Coord(1, 1), Coord(1, 2)),
                        FileId(0),
                    ),
                ),
                PToken::new(
                    ".",
                    Token::Op(Operator::Period),
                    Position::new(
                        Span(1, 2),
                        FilePos::new(Coord(1, 2), Coord(1, 3)),
                        FileId(0),
                    ),
                )
            ]
        );
    }

    // #[test]
    // fn hierarchy_tokenizer() {
    //     let input = "{+.}";
    //     let input = File::raw_test(input.to_string());
    //     let mut tokenizer = Lexer::new(&input);
    //     assert_eq!(
    //         tokenizer.
    //         [
    //             PToken::new(
    //                 "{",
    //                 Token::ControlPair(Control::Bracket, PairKind::Open),
    //                 Position::new(
    //                     Span(0, 1),
    //                     FilePos::new(Coord(1, 1), Coord(1, 2)),
    //                     FileId(0)
    //                 )
    //             ),
    //             PToken::new(
    //                 "+",
    //                 Token::Op(Operator::Plus),
    //                 Position::new(
    //                     Span(1, 2),
    //                     FilePos::new(Coord(1, 2), Coord(1, 3)),
    //                     FileId(0)
    //                 )
    //             ),
    //             PToken::new(
    //                     ".",
    //                     Token::Op(Operator::Period),
    //                     Position::new(
    //                         Span(2, 3),
    //                         FilePos::new(Coord(1, 3), Coord(1, 4)),
    //                         FileId(0)
    //                     )
    //             ),
    //             PToken::new(
    //                 "{",
    //                 Token::ControlPair(Control::Bracket, PairKind::Open),
    //                 Position::new(
    //                     Span(3, 4),
    //                     FilePos::new(Coord(1, 4), Coord(1, 5)),
    //                     FileId(0)
    //                 )
    //             ),
    //         ]
    //     );
    // }
}
