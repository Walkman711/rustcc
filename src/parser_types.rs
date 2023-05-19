use crate::{
    impl_disp_for_exp,
    lexer_enums::Token,
    utils::{ParseError, RustCcError},
};

#[derive(Clone, Debug)]
pub enum Program {
    Func(Function),
}

pub trait PrettyPrinter {
    fn pretty_print(&self, indentation_level: usize);
}

impl PrettyPrinter for Program {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        println!("{tabs}PROGRAM");
        match self {
            Program::Func(function) => {
                function.pretty_print(indentation_level + 1);
            }
        }
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Program::Func(function) => writeln!(f, "{function}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    Fun(String, Vec<BlockItem>),
}

impl PrettyPrinter for Function {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        match self {
            Function::Fun(func_name, block_items) => {
                println!("{tabs}FUNCTION: {func_name}()");
                println!("{tabs}{{");
                for block_item in block_items {
                    block_item.pretty_print(indentation_level + 1);
                }
                println!("{tabs}}}");
            }
        }
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Fun(func_name, block_items) => {
                writeln!(f, "FUNCTION: {func_name}()")?;
                writeln!(f, "{{")?;
                for block_item in block_items {
                    write!(f, "{block_item}")?;
                }
                writeln!(f, "}}")?;
            }
        }
        Ok(())
    }
}

pub type Declaration = (String, Option<Level15Exp>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Return(Option<Level15Exp>),
    Exp(Level15Exp),
    If(Level15Exp, Box<Statement>, Option<Box<Statement>>),
    Compound(Vec<BlockItem>),
}

impl PrettyPrinter for Statement {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        match self {
            Statement::Return(exp_opt) => match exp_opt {
                Some(exp) => println!("{tabs}RETVRN {exp}"),
                None => println!("{tabs}RETVRN 0 (omitted)"),
            },
            Statement::Exp(exp) => {
                println!("{tabs}EXP: {exp}")
            }
            Statement::If(exp, pred, else_opt) => match else_opt {
                Some(else_stmt) => {
                    println!("{tabs}IF: ({exp}) {{");
                    println!("{tabs}\t{pred}");
                    println!("{tabs}}} else {{");
                    println!("{tabs}\t{else_stmt}");
                    println!("{tabs}}}");
                }
                None => {
                    println!("{tabs}IF: ({exp}) {{");
                    println!("{tabs}\t{pred}");
                    println!("{tabs}}}");
                }
            },
            Statement::Compound(block_items) => {
                println!("{tabs}{{");
                for block_item in block_items {
                    block_item.pretty_print(indentation_level + 1);
                }
                println!("{tabs}}}");
            }
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Return(exp_opt) => match exp_opt {
                Some(exp) => writeln!(f, "RETVRN {exp}"),
                None => writeln!(f, "RETVRN 0 (omitted)"),
            },
            Statement::Exp(exp) => {
                writeln!(f, "EXP: {exp}")
            }
            Statement::If(exp, pred, else_opt) => match else_opt {
                Some(else_stmt) => writeln!(
                    f,
                    "IF: {exp} {{\n\t{pred}\n\t}} else {{\n\t{else_stmt}\n\t}}"
                ),
                None => writeln!(f, "IF: {exp} {{\n\t{pred}\n\t}}"),
            },
            Statement::Compound(block_items) => {
                writeln!(f, "{{")?;
                for block_item in block_items {
                    write!(f, "{block_item}")?;
                }
                writeln!(f, "}}")?;
                Ok(())
            }
        }
    }
}

impl Statement {
    pub fn has_return(&self) -> bool {
        match self {
            Statement::Return(_) => true,
            Statement::Exp(_) => false,
            Statement::If(_, pred, else_opt) => {
                let pred_has_ret = pred.has_return();
                if let Some(else_stmt) = else_opt {
                    let else_stmt_has_ret = else_stmt.has_return();
                    pred_has_ret && else_stmt_has_ret
                } else {
                    pred_has_ret
                }
            }
            Statement::Compound(bis) => bis.iter().any(|bi| bi.has_return()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BlockItem {
    Stmt(Statement),
    Declare(Declaration),
}

impl BlockItem {
    pub fn has_return(&self) -> bool {
        match self {
            BlockItem::Stmt(s) => s.has_return(),
            BlockItem::Declare(_) => false,
        }
    }
}

impl PrettyPrinter for BlockItem {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        if let BlockItem::Stmt(s) = self {
            s.pretty_print(indentation_level);
        } else {
            println!("{tabs}{}", self);
        }
    }
}

impl std::fmt::Display for BlockItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockItem::Stmt(s) => writeln!(f, "{s}"),
            BlockItem::Declare((id, exp_opt)) => match exp_opt {
                Some(exp) => writeln!(f, "INITIALIZE {id} = {exp}"),
                None => writeln!(f, "DECLARE {id}"),
            },
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level15Exp(pub (Level14Exp, Vec<(Level15Op, Level14Exp)>));
// Variable assignment - lvalue cannot be an expression
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Level14Exp {
    SimpleAssignment(String, Box<Level15Exp>),
    NonAssignment(Level13Exp),
}

impl std::fmt::Display for Level14Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level14Exp::SimpleAssignment(id, exp) => write!(f, "{id} = {exp}"),
            Level14Exp::NonAssignment(exp) => write!(f, "{exp}"),
        }
    }
}

// #[derive(Clone, Debug, Eq, PartialEq)]
// pub struct Level13Exp(pub (Level12Exp, Vec<(Level13Op, Level12Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Level13Exp {
    Ternary(Level12Exp, Box<Level15Exp>, Box<Level13Exp>),
    NoTernary(Level12Exp),
}

impl std::fmt::Display for Level13Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level13Exp::Ternary(exp, pred, else_exp) => write!(f, "{exp} ? {pred} : {else_exp}"),
            Level13Exp::NoTernary(exp) => write!(f, "{exp}"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level12Exp(pub (Level11Exp, Vec<(Level12Op, Level11Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level11Exp(pub (Level10Exp, Vec<(Level11Op, Level10Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level10Exp(pub (Level9Exp, Vec<(Level10Op, Level9Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level9Exp(pub (Level8Exp, Vec<(Level9Op, Level8Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level8Exp(pub (Level7Exp, Vec<(Level8Op, Level7Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level7Exp(pub (Level6Exp, Vec<(Level7Op, Level6Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level6Exp(pub (Level5Exp, Vec<(Level6Op, Level5Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level5Exp(pub (Level4Exp, Vec<(Level5Op, Level4Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level4Exp(pub (Level3Exp, Vec<(Level4Op, Level3Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Level3Exp(pub (Level2Exp, Vec<(Level3Op, Level2Exp)>));

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Level2Exp {
    Const(u64),
    Var(String),
    Unary(Level2Op, Box<Level2Exp>),
    ParenExp(Box<Level15Exp>),
}

impl std::fmt::Display for Level2Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level2Exp::Const(c) => write!(f, "{c} "),
            Level2Exp::Var(v) => write!(f, "{v} "),
            Level2Exp::Unary(op, exp) => write!(f, "{op} {exp}"),
            Level2Exp::ParenExp(exp) => write!(f, "{exp}"),
        }
    }
}

impl_disp_for_exp!(Level15Exp);
impl_disp_for_exp!(Level12Exp);
impl_disp_for_exp!(Level11Exp);
impl_disp_for_exp!(Level10Exp);
impl_disp_for_exp!(Level9Exp);
impl_disp_for_exp!(Level8Exp);
impl_disp_for_exp!(Level7Exp);
impl_disp_for_exp!(Level6Exp);
impl_disp_for_exp!(Level5Exp);
impl_disp_for_exp!(Level4Exp);
impl_disp_for_exp!(Level3Exp);

// TODO: Level1 Expressions involve structs, arrays, and pointers, so save that for later

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
pub enum Level15Op {
    Comma,
}

impl TryFrom<Token> for Level15Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Comma => Ok(Self::Comma),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
pub enum Level13Op {
    TernaryConditional,
}

impl TryFrom<Token> for Level13Op {
    type Error = RustCcError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::QuestionMark => Ok(Self::TernaryConditional),
            _ => Err(RustCcError::ParseError(ParseError::PeekFailed)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
pub enum Level2Op {
    PrefixIncrement,
    PrefixDecrement,
    UnaryPlus,
    UnaryMinus,
    LogicalNot,
    BitwiseNot,
    Cast,
    Dereference,
    AddressOf,
    SizeOf,
    Align,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, strum_macros::Display)]
pub enum Level1Op {
    PostfixIncrement,
    PostfixDecrement,
    ArraySubscript,
    StructureAccess,
    StructureAccessThroughPointer,
    CompoundLiteral,
}

#[macro_export]
macro_rules! impl_disp_for_exp {
    ($exp: ident) => {
        impl std::fmt::Display for $exp {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0 .0)?;
                for (op, exp) in &self.0 .1 {
                    write!(f, "{op} {exp}")?;
                }
                Ok(())
            }
        }
    };
}
