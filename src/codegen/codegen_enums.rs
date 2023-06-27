use crate::parsing::ops::{
    Level10Op, Level3Op, Level4Op, Level5Op, Level6Op, Level7Op, Level8Op, Level9Op,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum Arch {
    x86,
    ARM,
    RISCV,
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
    Neg,
    BitwiseNot,
    Mod,
}

impl Mnemonic {
    pub fn for_arch(&self, arch: Arch) -> &str {
        match arch {
            Arch::x86 => match self {
                Mnemonic::Or => "or",
                Mnemonic::Xor => "xor",
                Mnemonic::And => "and",
                Mnemonic::BitwiseLeftShift => todo!(),
                Mnemonic::BitwiseRightShift => todo!(),
                Mnemonic::Add => "add",
                Mnemonic::Subtract => "sub",
                Mnemonic::Multiply => "imul",
                Mnemonic::Divide => "idiv",
                Mnemonic::Neg => "neg",
                Mnemonic::BitwiseNot => "not",
                Mnemonic::Mod => todo!(),
            },
            Arch::ARM => match self {
                Mnemonic::Or => "orr",
                Mnemonic::And => "and",
                Mnemonic::Xor => "eor",
                Mnemonic::BitwiseLeftShift => todo!(),
                Mnemonic::BitwiseRightShift => todo!(),
                Mnemonic::Add => "add",
                Mnemonic::Subtract => "sub",
                Mnemonic::Multiply => "mul",
                Mnemonic::Divide => "sdiv",
                Mnemonic::Neg => "neg",
                Mnemonic::BitwiseNot => "mvn",
                Mnemonic::Mod => todo!(),
            },
            Arch::RISCV => match self {
                Mnemonic::Or => "ori",
                Mnemonic::Xor => "xori",
                Mnemonic::And => "andi",
                Mnemonic::BitwiseLeftShift => "slliw",
                Mnemonic::BitwiseRightShift => "sraiw",
                Mnemonic::Add => "addiw",
                Mnemonic::Subtract => "subw",
                Mnemonic::Multiply => "mulw",
                Mnemonic::Divide => "divw",
                Mnemonic::Neg => "negw",
                Mnemonic::BitwiseNot => "not",
                Mnemonic::Mod => todo!(),
            },
        }
    }
}

impl From<Level10Op> for Mnemonic {
    fn from(_value: Level10Op) -> Self {
        Mnemonic::Or
    }
}

impl From<Level9Op> for Mnemonic {
    fn from(_value: Level9Op) -> Self {
        Mnemonic::Xor
    }
}

impl From<Level8Op> for Mnemonic {
    fn from(_value: Level8Op) -> Self {
        Mnemonic::And
    }
}

impl From<Level5Op> for Mnemonic {
    fn from(value: Level5Op) -> Self {
        match value {
            Level5Op::BitwiseLeftShift => Mnemonic::BitwiseLeftShift,
            Level5Op::BitwiseRightShift => Mnemonic::BitwiseRightShift,
        }
    }
}

impl From<Level4Op> for Mnemonic {
    fn from(value: Level4Op) -> Self {
        match value {
            Level4Op::Addition => Mnemonic::Add,
            Level4Op::Subtraction => Mnemonic::Subtract,
        }
    }
}

impl From<Level3Op> for Mnemonic {
    fn from(value: Level3Op) -> Self {
        match value {
            Level3Op::Multiplication => Mnemonic::Multiply,
            Level3Op::Division => Mnemonic::Divide,
            Level3Op::Remainder => Mnemonic::Mod,
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

impl Cond {
    pub fn for_arch(&self, arch: Arch) -> &str {
        match arch {
            Arch::x86 => match self {
                Cond::Equals => "e",
                Cond::NotEquals => "ne",
                Cond::LessThan => "l",
                Cond::LessThanEquals => "le",
                Cond::GreaterThan => "g",
                Cond::GreaterThanEquals => "ge",
                Cond::Always => "mp", // Okay, this one makes me chuckle
            },
            Arch::ARM => match self {
                Self::Equals => "eq",
                Self::NotEquals => "ne",
                Self::LessThan => "lt",
                Self::LessThanEquals => "le",
                Self::GreaterThan => "gt",
                Self::GreaterThanEquals => "ge",
                Self::Always => "al",
            },
            Arch::RISCV => match self {
                Cond::Equals => "eq",
                Cond::NotEquals => "ne",
                Cond::LessThan => "lt",
                Cond::LessThanEquals => "le",
                Cond::GreaterThan => "gt",
                Cond::GreaterThanEquals => "ge",
                Cond::Always => "",
            },
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
