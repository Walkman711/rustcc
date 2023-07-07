use super::ops::*;
use crate::{
    codegen::function_map::FunctionMap,
    define_exp_level,
    utils::{
        scoped_map::ScopedMap,
        types::{BasicType, IntegerType, ReturnType, VariableType},
    },
};

pub trait PrettyPrinter {
    fn pretty_print(&self, indentation_level: usize);
}

#[derive(Debug)]
pub struct Program(pub Vec<TopLevelItem>);

impl PrettyPrinter for Program {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        println!("{tabs}PROGRAM");
        for top_level_item in &self.0 {
            top_level_item.pretty_print(indentation_level + 1);
        }
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for top_level_item in &self.0 {
            writeln!(f, "{top_level_item}")?;
        }
        Ok(())
    }
}
#[derive(Clone, Debug)]
pub enum GlobalVar {
    Declaration(String),
    Definition(String, i64),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Param {
    pub var_type: VariableType,
    pub id: String,
}

impl PrettyPrinter for GlobalVar {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        match self {
            GlobalVar::Declaration(id) => println!("{tabs}GLOBAL DECLARATION: {id}"),
            GlobalVar::Definition(id, val) => println!("{tabs}GLOBAL DEFINITION: {id} = {val}"),
        }
    }
}

impl std::fmt::Display for GlobalVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GlobalVar::Declaration(id) => writeln!(f, "GLOBAL DECLARATION: {id}"),
            GlobalVar::Definition(id, val) => writeln!(f, "GLOBAL DEFINITION: {id} = {val}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TopLevelItem {
    Fun(Function),
    Var(GlobalVar),
}

impl PrettyPrinter for TopLevelItem {
    fn pretty_print(&self, indentation_level: usize) {
        match self {
            TopLevelItem::Fun(fun) => fun.pretty_print(indentation_level),
            TopLevelItem::Var(var) => var.pretty_print(indentation_level),
        }
    }
}

impl std::fmt::Display for TopLevelItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevelItem::Fun(fun) => writeln!(f, "{fun}"),
            TopLevelItem::Var(var) => writeln!(f, "{var}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    Definition(String, ReturnType, Vec<Param>, Vec<BlockItem>),
    Declaration(String, ReturnType, Vec<Param>),
}

impl PrettyPrinter for Function {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        match self {
            Function::Definition(func_name, ret, params, block_items) => {
                print!("{tabs}FUNCTION DEFINITION: {func_name} -> {ret:?} (");

                for param in params {
                    print!("{}, ", param.id)
                }
                println!(")");
                println!("{tabs}{{");
                for block_item in block_items {
                    block_item.pretty_print(indentation_level + 1);
                }
                println!("{tabs}}}");
            }
            Function::Declaration(func_name, ret, params) => {
                print!("{tabs}FUNCTION DECLARATION: {func_name} -> {ret:?} (");

                for param in params {
                    print!("{}, ", param.id)
                }
                println!(");");
            }
        }
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Definition(func_name, ret, params, block_items) => {
                write!(f, "FUNCTION: {func_name} -> {ret:?} (")?;

                for param in params {
                    write!(f, "{}, ", param.id)?;
                }
                writeln!(f, ")")?;
                writeln!(f, "{{")?;
                for block_item in block_items {
                    writeln!(f, "{block_item}")?;
                }
                writeln!(f, "}}")?;
            }
            Function::Declaration(func_name, ret, params) => {
                write!(f, "FUNCTION DECLARATION: {func_name} -> {ret:?} (")?;

                for param in params {
                    write!(f, "{}, ", param.id)?;
                }
                writeln!(f, ");")?;
            }
        }
        Ok(())
    }
}

pub type Declaration = (String, VariableType, Option<Level15Exp>);

#[derive(Clone, Debug, Eq, PartialEq)]
// labeled,
// x compound,
// x exp,
// selection (IF & SWITCH),
// x iteration,
// jump (GOTO, CONTINUE, BREAK, RETURN)
pub enum Statement {
    Label(LabeledStatement),
    Compound(Vec<BlockItem>),
    Exp(Option<Level15Exp>),
    Select(SelectionStatement),
    Iter(IterationStatement),
    Jmp(JumpStatement),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LabeledStatement {
    Label(String, Box<Statement>),
    Case(Level15Exp, Box<Statement>),
    Default(Box<Statement>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SelectionStatement {
    If(Level15Exp, Box<Statement>, Option<Box<Statement>>),
    Switch(Level15Exp, Box<Statement>),
}

impl PrettyPrinter for SelectionStatement {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        match self {
            SelectionStatement::If(exp, pred, else_opt) => match else_opt {
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
            SelectionStatement::Switch(..) => todo!("switch pretty print"),
        }
    }
}

impl std::fmt::Display for SelectionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SelectionStatement::If(exp, pred, else_opt) => match else_opt {
                Some(else_stmt) => writeln!(
                    f,
                    "IF: {exp} {{\n\t{pred}\n\t}} else {{\n\t{else_stmt}\n\t}}"
                ),
                None => writeln!(f, "IF: {exp} {{\n\t{pred}\n\t}}"),
            },
            SelectionStatement::Switch(..) => todo!("switch disp"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IterationStatement {
    For(
        Option<Level15Exp>,
        Option<Level15Exp>,
        Option<Level15Exp>,
        Box<Statement>,
    ),
    ForDecl(
        Declaration,
        Option<Level15Exp>,
        Option<Level15Exp>,
        Box<Statement>,
    ),
    While(Level15Exp, Box<Statement>),
    DoWhile(Box<Statement>, Level15Exp),
}

impl std::fmt::Display for IterationStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IterationStatement::For(..) => {
                writeln!(f, "FOR todo")
            }
            IterationStatement::ForDecl(..) => {
                writeln!(f, "FORDecl todo")
            }
            IterationStatement::While(exp, stmt) => {
                writeln!(f, "WHILE ({exp}) {{\n\t{stmt}\n}}")
            }
            IterationStatement::DoWhile(exp, stmt) => {
                writeln!(f, "DO {{\n\t{stmt}\n}} WHILE ({exp})")
            }
        }
    }
}

impl PrettyPrinter for IterationStatement {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        match self {
            IterationStatement::While(exp, stmt) => {
                println!("{tabs}WHILE ({exp}) {{");
                stmt.pretty_print(indentation_level + 1);
                println!("{tabs}}}");
            }
            IterationStatement::DoWhile(stmt, exp) => {
                println!("{tabs}DO {{");
                stmt.pretty_print(indentation_level + 1);
                println!("{tabs}}} WHILE ({exp})");
            }
            IterationStatement::For(exp1, exp2, exp3, body) => {
                print!("{tabs}FOR (");
                if let Some(exp) = exp1 {
                    print!("{exp}; ");
                } else {
                    print!("; ")
                }

                if let Some(exp) = exp2 {
                    print!("{exp}; ");
                } else {
                    print!("; ")
                }

                if let Some(exp) = exp3 {
                    print!("{exp};");
                }

                println!(") {{");
                body.pretty_print(indentation_level + 1);
                println!("{tabs}}}")
            }
            IterationStatement::ForDecl(decl, exp2, exp3, body) => {
                print!("{tabs}FOR ({} = ...; ", decl.0,);

                if let Some(exp) = exp2 {
                    print!("{exp}; ");
                } else {
                    print!("; ")
                }

                if let Some(exp) = exp3 {
                    print!("{exp}");
                }

                println!(") {{");
                body.pretty_print(indentation_level + 1);
                println!("{tabs}}}")
            }
        }
    }
}

impl PrettyPrinter for Statement {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        match self {
            Statement::Label(_lbl_stmt) => todo!("label stmt pretty print"),
            Statement::Exp(exp_opt) => {
                if let Some(exp) = exp_opt {
                    println!("{tabs}EXP: {exp}");
                }
            }
            Statement::Select(select_stmt) => select_stmt.pretty_print(indentation_level),
            Statement::Compound(block_items) => {
                println!("{tabs}{{");
                for block_item in block_items {
                    block_item.pretty_print(indentation_level + 1);
                }
                println!("{tabs}}}");
            }
            Statement::Iter(iter_stmt) => iter_stmt.pretty_print(indentation_level),
            Statement::Jmp(jmp_stmt) => jmp_stmt.pretty_print(indentation_level),
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Label(_lbl_stmt) => todo!("label stmt pretty print"),
            Statement::Exp(exp_opt) => match exp_opt {
                Some(exp) => writeln!(f, "EXP: {exp}"),
                None => writeln!(f, "NULL EXP"),
            },
            Statement::Select(select_stmt) => writeln!(f, "{select_stmt}"),
            Statement::Compound(block_items) => {
                writeln!(f, "{{")?;
                for block_item in block_items {
                    write!(f, "{block_item}")?;
                }
                writeln!(f, "}}")?;
                Ok(())
            }
            Statement::Iter(iter_stmt) => writeln!(f, "{iter_stmt}"),
            Statement::Jmp(jmp_stmt) => writeln!(f, "{jmp_stmt}"),
        }
    }
}

impl Statement {
    fn has_return(&self) -> bool {
        match self {
            Statement::Select(select_stmt) => match select_stmt {
                SelectionStatement::If(_, pred, else_opt) => {
                    let pred_has_ret = pred.has_return();
                    if let Some(else_stmt) = else_opt {
                        let else_stmt_has_ret = else_stmt.has_return();
                        pred_has_ret && else_stmt_has_ret
                    } else {
                        pred_has_ret
                    }
                }
                SelectionStatement::Switch(_exp, stmt) => stmt.has_return(),
            },
            Statement::Compound(bis) => bis.iter().any(|bi| bi.has_return()),
            Statement::Exp(_) => false,
            Statement::Iter(iter_stmt) => match iter_stmt {
                IterationStatement::For(_, _, _, stmt) => stmt.has_return(),
                IterationStatement::ForDecl(_, _, _, stmt) => stmt.has_return(),
                IterationStatement::While(_, stmt) => stmt.has_return(),
                IterationStatement::DoWhile(stmt, _) => stmt.has_return(),
            },
            Statement::Jmp(jmp_stmt) => match jmp_stmt {
                JumpStatement::Break => false,
                JumpStatement::Continue => false,
                JumpStatement::Return(_) => true,
                JumpStatement::Goto(_) => false,
            },
            Statement::Label(label) => match label {
                LabeledStatement::Label(_, stmt)
                | LabeledStatement::Case(_, stmt)
                | LabeledStatement::Default(stmt) => stmt.has_return(),
            },
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum JumpStatement {
    Break,
    Continue,
    Return(Option<Level15Exp>),
    Goto(String),
}

impl std::fmt::Display for JumpStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JumpStatement::Break => writeln!(f, "BREAK"),
            JumpStatement::Continue => writeln!(f, "CONTINUE"),
            JumpStatement::Return(exp_opt) => match exp_opt {
                Some(exp) => writeln!(f, "RETVRN {exp}"),
                None => writeln!(f, "RETVRN 0 (omitted)"),
            },
            JumpStatement::Goto(label) => writeln!(f, "GOTO {label}"),
        }
    }
}

impl PrettyPrinter for JumpStatement {
    fn pretty_print(&self, indentation_level: usize) {
        let tabs = "\t".repeat(indentation_level);
        println!("{tabs}{}", self)
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
            BlockItem::Declare((id, vt, exp_opt)) => match exp_opt {
                Some(exp) => writeln!(f, "INITIALIZE {vt:?} {id} = {exp}"),
                None => writeln!(f, "DECLARE {vt:?} {id}"),
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
    // TODO: this is wrong, it's level 1, but how to write that?
    FunctionCall(String, Vec<Level15Exp>),
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
            Level2Exp::FunctionCall(fun_name, args) => {
                write!(f, "{fun_name}(")?;
                for arg in args {
                    write!(f, "{arg}, ")?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

pub trait GetExpressionType {
    fn exp_type(&self, fn_map: &FunctionMap, scope_map: &ScopedMap) -> ReturnType;
}

impl GetExpressionType for Level14Exp {
    fn exp_type(&self, fn_map: &FunctionMap, scope_map: &ScopedMap) -> ReturnType {
        match self {
            Level14Exp::SimpleAssignment(_, exp) => exp.exp_type(fn_map, scope_map),
            Level14Exp::NonAssignment(exp) => exp.exp_type(fn_map, scope_map),
        }
    }
}

impl GetExpressionType for Level13Exp {
    fn exp_type(&self, fn_map: &FunctionMap, scope_map: &ScopedMap) -> ReturnType {
        match self {
            Level13Exp::Ternary(_, if_exp, else_exp) => {
                let if_exp_ret = if_exp.exp_type(fn_map, scope_map);
                let else_exp_ret = else_exp.exp_type(fn_map, scope_map);
                assert_eq!(if_exp_ret, else_exp_ret);
                if_exp_ret
            }
            Level13Exp::NoTernary(exp) => exp.exp_type(fn_map, scope_map),
        }
    }
}

impl GetExpressionType for Level2Exp {
    fn exp_type(&self, fn_map: &FunctionMap, scope_map: &ScopedMap) -> ReturnType {
        match self {
            Level2Exp::FunctionCall(fn_name, _) => fn_map.ret_type(fn_name),
            Level2Exp::Const(_) => ReturnType::NonVoid(BasicType::Int(IntegerType::Int).into()),
            Level2Exp::Var(id) => ReturnType::NonVoid(scope_map.get_var(id).unwrap().var_type),
            Level2Exp::Unary(op, exp) => match op {
                Level2Op::SizeOf => ReturnType::NonVoid(BasicType::Int(IntegerType::Int).into()),
                _ => exp.exp_type(fn_map, scope_map),
            },
            Level2Exp::ParenExp(exp) => exp.exp_type(fn_map, scope_map),
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
                    write!(f, " {op} {exp}")?;
                }
                Ok(())
            }
        }

        impl GetExpressionType for $exp {
            fn exp_type(&self, fn_map: &FunctionMap, scope_map: &ScopedMap) -> ReturnType {
                let mut ret_type = self.0 .0.exp_type(fn_map, scope_map);
                for (_op, rh_exp) in &self.0 .1 {
                    let rh_ret_type = rh_exp.exp_type(fn_map, scope_map);
                    ret_type = ret_type.binop(rh_ret_type);
                }
                ret_type
            }
        }
    };
}
