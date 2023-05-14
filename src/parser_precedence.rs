use crate::{
    lexer_enums::Token,
    utils::{ParseError, RustCcError},
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level1Op {
    PostfixIncrement,
    PostfixDecrement,
    ArraySubscript,
    StructureAccess,
    StructureAccessThroughPointer,
    CompoundLiteral,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level2Op {
    PrefixIncrement,
    PrefixDecrement,
    LogicalNot,
    BitwiseNot,
    Cast,
    Dereference,
    AddressOf,
    SizeOf,
    Align,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level3Op {
    Multiplication,
    Division,
    Remainder,
}

impl TryFrom<Token> for Level3Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Star => Ok(Self::Multiplication),
            Token::Slash => Ok(Self::Division),
            Token::PercentSign => Ok(Self::Remainder),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level4Op {
    Addition,
    Subtraction,
}

impl TryFrom<Token> for Level4Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Plus => Ok(Self::Addition),
            Token::Minus => Ok(Self::Subtraction),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level5Op {
    BitwiseLeftShift,
    BitwiseRightShift,
}

impl TryFrom<Token> for Level5Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::BitwiseLeftShift => Ok(Self::BitwiseLeftShift),
            Token::BitwiseRightShift => Ok(Self::BitwiseRightShift),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level6Op {
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
}

impl TryFrom<Token> for Level6Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::LessThan => Ok(Self::LessThan),
            Token::LessThanEquals => Ok(Self::LessThanEquals),
            Token::GreaterThan => Ok(Self::GreaterThan),
            Token::GreaterThanEquals => Ok(Self::GreaterThanEquals),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level7Op {
    Equals,
    NotEquals,
}

impl TryFrom<Token> for Level7Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::DoubleEquals => Ok(Self::Equals),
            Token::NotEquals => Ok(Self::NotEquals),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level8Op {
    BitwiseAnd,
}

impl TryFrom<Token> for Level8Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::SingleAnd => Ok(Self::BitwiseAnd),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level9Op {
    BitwiseXor,
}

impl TryFrom<Token> for Level9Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::BitwiseXor => Ok(Self::BitwiseXor),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level10Op {
    BitwiseOr,
}

impl TryFrom<Token> for Level10Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::BitwiseOr => Ok(Self::BitwiseOr),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level11Op {
    LogicalAnd,
}

impl TryFrom<Token> for Level11Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::LogicalAnd => Ok(Self::LogicalAnd),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level12Op {
    LogicalOr,
}

impl TryFrom<Token> for Level12Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::LogicalOr => Ok(Self::LogicalOr),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level13Op {
    TernaryConditional,
}

impl TryFrom<Token> for Level13Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            // Token::TernaryConditional => Ok(Self::TernaryConditional),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level14Op {
    SimpleAssignment,
    AssignmentBySum,
    AssignmentByDifference,
    AssignmentByProduct,
    AssignmentByQuotient,
    AssignmentByRemainder,
    AssignmentByBitwiseLeftShift,
    AssignmentByBitwiseRightShift,
    AssignmentByBitwiseAnd,
    AssignmentByBitwiseXor,
    AssignmentByBitwiseOr,
}

impl TryFrom<Token> for Level14Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::SingleEquals => Ok(Self::SimpleAssignment),
            Token::PlusEquals => Ok(Self::AssignmentBySum),
            Token::MinusEquals => Ok(Self::AssignmentByDifference),
            Token::StarEquals => Ok(Self::AssignmentByProduct),
            Token::SlashEquals => Ok(Self::AssignmentByQuotient),
            Token::PercentEquals => Ok(Self::AssignmentByRemainder),
            Token::BitwiseLeftShiftEquals => Ok(Self::AssignmentByBitwiseLeftShift),
            Token::BitwiseRightShiftEquals => Ok(Self::AssignmentByBitwiseRightShift),
            Token::BitwiseAndEquals => Ok(Self::AssignmentByBitwiseAnd),
            Token::BitwiseXorEquals => Ok(Self::AssignmentByBitwiseXor),
            Token::BitwiseOrEquals => Ok(Self::AssignmentByBitwiseOr),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level15Op {
    Comma,
}
