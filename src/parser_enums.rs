// TODO: do something with bin op precedence
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AdditiveOp {
    Addition,
    Subtraction,
}

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
