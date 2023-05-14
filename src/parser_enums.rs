// Logical Or is the lowest precedence

// Logical And is the next lowest precedence

use crate::{
    lexer_enums::Token,
    utils::{ParseError, RustCcError},
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EqualityOp {
    Equals,
    NotEquals,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RelationOp {
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AdditiveOp {
    Addition,
    Subtraction,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MultiplicativeOp {
    Multiply,
    Divide,
}

impl TryFrom<Token> for MultiplicativeOp {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Star => Ok(MultiplicativeOp::Multiply),
            Token::Slash => Ok(MultiplicativeOp::Divide),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Negation,
    LogicalNegation,
    BitwiseComplement,
}
