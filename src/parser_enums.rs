// Logical Or is the lowest precedence

// Logical And is the next lowest precedence

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EqualityOp {
    Equals,
    NotEquals,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RelationOp {
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AdditiveOp {
    Addition,
    Subtraction,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MultiplicativeOp {
    Multiply,
    Divide,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Negation,
    LogicalNegation,
    BitwiseComplement,
}
