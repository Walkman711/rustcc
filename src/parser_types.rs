use crate::{define_exp_level, ops::*};

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

define_exp_level!(Level15Exp, Level15Op, Level14Exp);
define_exp_level!(Level12Exp, Level12Op, Level11Exp);
define_exp_level!(Level11Exp, Level11Op, Level10Exp);
define_exp_level!(Level10Exp, Level10Op, Level9Exp);
define_exp_level!(Level9Exp, Level9Op, Level8Exp);
define_exp_level!(Level8Exp, Level8Op, Level7Exp);
define_exp_level!(Level7Exp, Level7Op, Level6Exp);
define_exp_level!(Level6Exp, Level6Op, Level5Exp);
define_exp_level!(Level5Exp, Level5Op, Level4Exp);
define_exp_level!(Level4Exp, Level4Op, Level3Exp);
define_exp_level!(Level3Exp, Level3Op, Level2Exp);

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
pub enum Level2Exp {
    Const(u64),
    Var(String),
    Unary(Level2Op, Box<Level2Exp>),
    ParenExp(Box<Level15Exp>),
}

impl std::fmt::Display for Level2Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level2Exp::Const(c) => write!(f, "{c}"),
            Level2Exp::Var(v) => write!(f, "{v}"),
            Level2Exp::Unary(op, exp) => write!(f, "{op} {exp}"),
            Level2Exp::ParenExp(exp) => write!(f, "{exp}"),
        }
    }
}

// TODO: Level1 Expressions involve structs, arrays, and pointers, so save that for later

#[macro_export]
macro_rules! define_exp_level {
    ($exp: ident, $op: ty, $lower_level_exp: ty) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $exp(pub ($lower_level_exp, Vec<($op, $lower_level_exp)>));

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
