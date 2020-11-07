use std::fmt::{Display, Formatter};

use crate::syntax::ast::{BinaryOp, UnaryOp};
use crate::syntax::{Operator, Position, Token};
use crate::types::Type;

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

    #[error("unexpected token: expected '{}', found '{}'", expected.join(", "), found)]
    UnexpectedTokenMulti {
        expected: Vec<String>,
        found: String,
    },

    #[error("operator is not a valid binary operator: '{}'", op)]
    InvalidBinaryOp { op: Operator },

    #[error("operator is not a valid assignment operator: '{}'", op)]
    InvalidAssignmentOp { op: Operator },

    #[error("expecting an identifier, found '{0}'")]
    ExpectingIdentifier(String),

    #[error("invalid token '{0}' in types expression")]
    InvalidTypeExpression(String),

    #[error("invalid context for structure expression")]
    InvalidStructInContext,

    #[error(
        "incompatible types for operator '{}': left '{}', right '{}'",
        op,
        left,
        right
    )]
    IncompatibleBinaryOpTypes {
        op: BinaryOp,
        left: Type,
        right: Type,
    },

    #[error("incompatible types for operator '{}' for type '{}'", op, expr)]
    IncompatibleUnaryOpTypes { op: UnaryOp, expr: Type },

    #[error("use of undeclared identifier '{0}'")]
    UndeclaredIdentifier(String),

    #[error("declaration of duplicate name '{0}'")]
    DuplicateName(String),

    #[error("incompatible types, expected '{}' and found '{}'", left, right)]
    IncompatibleTypes { left: Type, right: Type },

    #[error("must have initialization expression or type annotation")]
    InvalidLocalItem,

    #[error("type must be a struct, found '{}'", ty)]
    MustBeStruct { ty: Type },

    #[error(
        "undefined name '{}' in name binding of struct expression of type '{}'",
        name,
        ty
    )]
    UndefinedFieldInStructBinding { name: String, ty: Type },

    #[error("invalid 'self' in non-associative function")]
    InvalidSelfInFunction,

    #[error("invalid 'self' declared in non-associative function")]
    InvalidSelfDeclaredInFunction,

    #[error("unexpected 'self' parameter in function")]
    UnexpectedSelfParameter,

    #[error("invalid 'Self' type is context")]
    InvalidSelfTypeInContext,

    #[error("invalid 'self' expression")]
    InvalidSelfExpression,

    #[error(
        "unable to access field '{}' of {} '{}'",
        field,
        entity_type,
        struct_type
    )]
    InaccessableSubEntity {
        entity_type: String,
        struct_type: Type,
        field: String,
    },

    #[error("struct '{}' does not have {} '{}'", struct_type, entity_type, field)]
    UnknownSubEntity {
        entity_type: String,
        struct_type: Type,
        field: String,
    },

    #[error("attempting to call an invalid type '{}'", ty)]
    InvalidCallOnType { ty: Type },

    #[error(
        "attempting to call function expecting {} parameters with {} parameters",
        expected,
        found
    )]
    InvalidActuals { expected: usize, found: usize },

    #[error(
        "use of undeclared {} '{}' of type '{}'",
        entity_type,
        name,
        struct_type
    )]
    UndeclaredStructEntity {
        entity_type: String,
        name: String,
        struct_type: Type,
    },

    #[error("invalid use of associated function '{}' with 'self' receiver", name)]
    InvalidAssociatedFunctionReceiver { name: String },

    #[error("associated function doesn't take receiver '{}'", name)]
    AssociatedFunctionInvalidReceiver { name: String },

    #[error("invalid lvalue")]
    InvalidLValue,

    #[error("attempting to mutable '{}' which is not mutable", name)]
    ImmutableEntity { name: String },

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

    // #[error("expected types specification following '{}', found '{}'", following.text(), found.text())]
    // ExpectedTypeSpec {
    //     following: OwnedToken,
    //     found: OwnedToken,
    // },
    //
    // #[error("unable to determine types of {}, missing types or initializing expression", if *is_param { "parameter" } else { "local"})]
    // InvalidLocalItem { is_param: bool },
    //
    // #[error("'self' must be the first parameter")]
    // InvalidSelfParam,
    //
    // #[error("invalid context for types parameters")]
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

    fn new_default(kind: ErrorKind) -> Self {
        Self {
            position: Position::default(),
            kind,
        }
    }

    pub fn with_position(self, position: Position) -> Self {
        Self {
            position,
            kind: self.kind,
        }
    }

    pub fn unknown_escape_character(ch: char) -> Self {
        Self::new_default(ErrorKind::UnknownEscapedCharacter(ch))
    }

    pub fn unexpected_eof() -> Self {
        Self::new_default(ErrorKind::UnexpectedEOF)
    }

    pub fn invalid_character(ch: char) -> Self {
        Self::new_default(ErrorKind::InvalidCharacter { ch })
    }

    pub fn uneven_pairs() -> Self {
        Self::new_default(ErrorKind::UnevenPair)
    }

    pub fn unexpected_token(expected: Token, found: &Token) -> Self {
        Self::new_default(ErrorKind::ExpectedToken(
            format!("{}", expected),
            format!("{}", found),
        ))
    }

    pub fn unexpected_token_multi(expected: Vec<Token>, found: &Token) -> Self {
        Self::new_default(ErrorKind::UnexpectedTokenMulti {
            expected: expected.iter().map(|t| format!("{}", t)).collect(),
            found: format!("{}", found),
        })
    }

    pub fn invalid_binary_operator(op: Operator) -> Self {
        Self::new_default(ErrorKind::InvalidBinaryOp { op })
    }

    pub fn invalid_assignment_operator(op: Operator) -> Self {
        Self::new_default(ErrorKind::InvalidAssignmentOp { op })
    }

    pub fn expecting_identifier(token: &Token) -> Self {
        Self::new_default(ErrorKind::ExpectingIdentifier(token.to_string()))
    }

    pub fn invalid_type_expression(token: &Token) -> Self {
        Self::new_default(ErrorKind::InvalidTypeExpression(format!("{}", token)))
    }

    pub fn invalid_context_struct_expr() -> Self {
        Self::new_default(ErrorKind::InvalidStructInContext)
    }

    pub fn incompatible_operands_for_binary_op(op: BinaryOp, left: &Type, right: &Type) -> Self {
        Self::new_default(ErrorKind::IncompatibleBinaryOpTypes {
            op,
            left: left.clone(),
            right: right.clone(),
        })
    }

    pub fn incompatible_operands_for_unary_op(op: UnaryOp, expr: &Type) -> Self {
        Self::new_default(ErrorKind::IncompatibleUnaryOpTypes {
            op,
            expr: expr.clone(),
        })
    }

    pub fn undeclared_identifier(name: String) -> Self {
        Self::new_default(ErrorKind::UndeclaredIdentifier(name))
    }

    pub fn duplicate_name(name: String) -> Self {
        Self::new_default(ErrorKind::DuplicateName(name))
    }

    pub fn incompatible_types(expected: &Type, found: &Type) -> Self {
        Self::new_default(ErrorKind::IncompatibleTypes {
            left: expected.clone(),
            right: found.clone(),
        })
    }

    pub fn invalid_local_item() -> Self {
        Self::new_default(ErrorKind::InvalidLocalItem)
    }

    pub fn expected_struct_type(ty: &Type) -> Self {
        Self::new_default(ErrorKind::MustBeStruct { ty: ty.clone() })
    }

    pub fn undeclared_field_in_struct_binding(name: &str, ty: &Type) -> Self {
        Self::new_default(ErrorKind::UndefinedFieldInStructBinding {
            name: name.to_owned(),
            ty: ty.clone(),
        })
    }

    pub fn invalid_self_in_function() -> Self {
        Self::new_default(ErrorKind::InvalidSelfInFunction)
    }

    pub fn invalid_self_declared_in_function() -> Self {
        Self::new_default(ErrorKind::InvalidSelfDeclaredInFunction)
    }

    pub fn unexpected_self_parameter() -> Self {
        Self::new_default(ErrorKind::UnexpectedSelfParameter)
    }

    pub fn invalid_self_type_in_context() -> Self {
        Self::new_default(ErrorKind::InvalidSelfTypeInContext)
    }

    pub fn invalid_self_expression() -> Self {
        Self::new_default(ErrorKind::InvalidSelfExpression)
    }

    pub fn unknown_subfield(entity_type: &str, struct_type: &Type, field: String) -> Self {
        Self::new_default(ErrorKind::UnknownSubEntity {
            entity_type: entity_type.to_owned(),
            struct_type: struct_type.clone(),
            field,
        })
    }

    pub fn inaccessible_subentity(entity_type: &str, struct_type: &Type, field: String) -> Self {
        Self::new_default(ErrorKind::InaccessableSubEntity {
            entity_type: entity_type.to_owned(),
            struct_type: struct_type.clone(),
            field,
        })
    }

    pub fn invalid_call_on_type(ty: &Type) -> Self {
        Self::new_default(ErrorKind::InvalidCallOnType { ty: ty.clone() })
    }

    pub fn invalid_actuals(expected: usize, found: usize) -> Self {
        Self::new_default(ErrorKind::InvalidActuals { expected, found })
    }

    pub fn unknown_subentity(entity_type: &str, name: &str, struct_type: &Type) -> Self {
        Self::new_default(ErrorKind::UndeclaredStructEntity {
            entity_type: entity_type.to_owned(),
            name: name.to_owned(),
            struct_type: struct_type.clone(),
        })
    }

    pub fn invalid_associated_function_receiver(name: &str) -> Self {
        Self::new_default(ErrorKind::InvalidAssociatedFunctionReceiver {
            name: name.to_owned(),
        })
    }

    pub fn associated_function_invalid_receiver(name: &str) -> Self {
        Self::new_default(ErrorKind::AssociatedFunctionInvalidReceiver {
            name: name.to_owned(),
        })
    }

    pub fn invalid_lvalue() -> Self {
        Self::new_default(ErrorKind::InvalidLValue)
    }

    pub fn immutable_entity(name: &str) -> Self {
        Self::new_default(ErrorKind::ImmutableEntity {
            name: name.to_owned(),
        })
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
        Self::new_default(ErrorKind::Other(err))
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
