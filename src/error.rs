use crate::syntax::{Position, Token, TokenTree, Operator};

#[derive(thiserror::Error, Debug, Clone)]
pub enum ErrorKind {
    // lex errors
    #[error("invalid character: '{}'", ch)]
    InvalidCharacter { ch: char },

    // #[error("expected operator, found: '{}'", found.text())]
    // ExpectedOperator { found: OwnedToken },
    #[error("unknown escaped character: '{0}'")]
    UnknownEscapedCharacter(char),

    #[error("unexpected end of file")]
    UnexpectedEOF,

    #[error("Uneven pair")]
    UnevenPair,

    #[error("unexpected token: expected '{0}' found '{1}'")]
    ExpectedToken(String, String),

    #[error("operator is not a valid binary operator: '{}'", op.to_string())]
    InvalidBinaryOp {
        op: Operator,
    },

    #[error("expecting an identifier, found '{0}'")]
    ExpectingIdentifier(String),

    // #[error("expecting keyword '{}' found '{}'", expected.to_string(), found.text())]
    // ExpectedKeyword {
    //     expected: Keyword,
    //     found: OwnedToken,
    // },
    //
    // #[error("expecting operator '{}' found '{}'", expected.to_string(), found.text())]
    // ExpectedSpecificOperator {
    //     expected: Operator,
    //     found: OwnedToken,
    // },
    //
    // #[error("expecting identifier found '{}'", found.text())]
    // ExpectedIdentifier { found: OwnedToken },

    // #[error("expected newline found '{}'", found.text())]
    // ExpectedNewline { found: OwnedToken },

    // #[error("expected expression following '{}', found '{}'", following.text(), found.text())]
    // ExpectedExpression {
    //     following: OwnedToken,
    //     found: OwnedToken,
    // },

    // #[error("expected type specification following '{}', found '{}'", following.text(), found.text())]
    // ExpectedTypeSpec {
    //     following: OwnedToken,
    //     found: OwnedToken,
    // },
    //
    // #[error("unable to determine type of {}, missing type or initializing expression", if *is_param { "parameter" } else { "local"})]
    // InvalidLocalItem { is_param: bool },
    //
    // #[error("'self' must be the first parameter")]
    // InvalidSelfParam,
    //
    // #[error("invalid context for type parameters")]
    // InvalidTypeParameter,
    //
    #[error("Other: {0}")]
    Other(String),
}

#[derive(Debug, Clone)]
pub struct Error {
    kind: ErrorKind,
    position: Position,
}

impl Error {
    pub fn new(kind: ErrorKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn pos(&self) -> &Position {
        &self.position
    }
}

impl<'src> Error {
    // fn expected_operator(found: &Token<'src>) -> Self {
    //     Self::ExpectedOperator {
    //         found: found.to_owned(),
    //     }
    // }

    pub fn with_position(self, position: Position) -> Self {
        Self {
            position,
            kind: self.kind,
        }
    }

    pub fn unknown_escape_character(ch: char) -> Self {
        Self {
            position: Position::default(),
            kind: ErrorKind::UnknownEscapedCharacter(ch),
        }
    }

    pub fn unexpected_eof() -> Self {
        Self {
            position: Position::default(),
            kind: ErrorKind::UnexpectedEOF,
        }
    }

    pub fn invalid_character(ch: char) -> Self {
        Self {
            position: Position::default(),
            kind: ErrorKind::InvalidCharacter { ch },
        }
    }

    pub fn uneven_pairs() -> Self {
        Self {
            position: Position::default(),
            kind: ErrorKind::UnevenPair,
        }
    }

    pub fn unexpected_token(expected: Token, found: &TokenTree) -> Self {
        Self {
            position: Position::default(),
            kind: ErrorKind::ExpectedToken(format!("{}", expected), format!("{}", found)),
        }
    }

    pub fn invalid_binary_operator(op: Operator) -> Self {
        Self {
            position: Position::default(),
            kind: ErrorKind::InvalidBinaryOp { op },
        }
    }

    pub fn expecting_identifier(token: &TokenTree) -> Self {
        Self {
            position: Position::default(),
            kind: ErrorKind::ExpectingIdentifier(token.to_string()),
        }
    }

    // fn execpted_keyword(expected: token::Kw, token: &Token) -> Self {
    //     Self::ExpectedKeyword {
    //         expected,
    //         found: token.to_owned(),
    //     }
    // }
    //
    // fn execpted_specific_operator(expected: token::Op, token: &Token) -> Self {
    //     Self::ExpectedSpecificOperator {
    //         expected,
    //         found: token.to_owned(),
    //     }
    // }
    //
    // fn expected_identifier(token: &Token) -> Self {
    //     Self::ExpectedIdentifer {
    //         found: token.to_owned(),
    //     }
    // }
    //
    // fn expected_newline(token: &Token) -> Self {
    //     Self::ExpectedNewline {
    //         found: token.to_owned(),
    //     }
    // }
    //
    // fn expected_expression(following: &Token, found: &Token) -> Self {
    //     Self::ExpectedExpression {
    //         following: following.to_owned(),
    //         found: found.to_owned(),
    //     }
    // }
    //
    // fn expected_typespec(following: &Token, found: &Token) -> Self {
    //     Self::ExpectedTypeSpec {
    //         following: following.to_owned(),
    //         found: found.to_owned(),
    //     }
    // }
    //
    // fn invalid_local_item(is_param: bool) -> Self {
    //     Self::InvalidLocalItem { is_param }
    // }
    //
    // fn invalid_self_param() -> Self {
    //     Self::InvalidSelfParam
    // }
    //
    // fn invalid_type_parameter() -> Self {
    //     Self::InvalidTypeParameter
    // }

    pub fn other(err: String) -> Self {
        Self {
            position: Position::default(),
            kind: ErrorKind::Other(err),
        }
    }
}
