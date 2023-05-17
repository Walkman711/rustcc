use crate::parser_types::{Level6Op, Level7Op};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
// TODO: make a trait s.t. we can generate x86 and ARM
pub enum Arch {
    x86,
    ARM,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cond {
    Equals,
    NotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    Always,
}

impl std::fmt::Display for Cond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Equals => write!(f, "eq"),
            Self::NotEquals => write!(f, "ne"),
            Self::LessThan => write!(f, "lt"),
            Self::LessThanEquals => write!(f, "le"),
            Self::GreaterThan => write!(f, "gt"),
            Self::GreaterThanEquals => write!(f, "ge"),
            Self::Always => write!(f, "al"),
        }
    }
}

impl From<Level6Op> for Cond {
    fn from(value: Level6Op) -> Self {
        match value {
            Level6Op::LessThan => Self::LessThan,
            Level6Op::LessThanEquals => Self::LessThanEquals,
            Level6Op::GreaterThan => Self::GreaterThan,
            Level6Op::GreaterThanEquals => Self::GreaterThanEquals,
        }
    }
}

impl From<Level7Op> for Cond {
    fn from(value: Level7Op) -> Self {
        match value {
            Level7Op::Equals => Self::Equals,
            Level7Op::NotEquals => Self::NotEquals,
        }
    }
}
