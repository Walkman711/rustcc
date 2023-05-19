use crate::parser_types::{Level6Op, Level7Op};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
// TODO: make a trait s.t. we can generate x86 and ARM
pub enum Arch {
    x86,
    ARM,
}

pub enum Mnemonic {
    Or,
    Xor,
    And,
    BitwiseLeftShift,
    BitwiseRightShift,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Mnemonic {
    pub fn for_arch(&self, arch: Arch) -> &str {
        match arch {
            Arch::x86 => todo!("implement x86"),
            Arch::ARM => match self {
                Mnemonic::Xor => "eor",
                Mnemonic::And => "and",
                Mnemonic::Or => "orr",
                Mnemonic::BitwiseLeftShift => todo!(),
                Mnemonic::BitwiseRightShift => todo!(),
                Mnemonic::Add => "add",
                Mnemonic::Subtract => "sub",
                Mnemonic::Multiply => "mul",
                Mnemonic::Divide => "sdiv",
            },
        }
    }
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
