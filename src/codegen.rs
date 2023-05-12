use crate::parser::{Expression, Function, Program, Statement, UnaryOp};

pub fn generate_asm(prog: Program) {
    match prog {
        Program::Func(func) => match func {
            Function::Fun(identifier, stmt) => {
                println!(".global _{identifier}");
                println!(".align 2");
                println!();
                println!("_{identifier}:");
                generate_stmt_asm(stmt);
            }
        },
    }
}

fn generate_stmt_asm(stmt: Statement) {
    match stmt {
        Statement::Return(exp) => {
            generate_expression_asm(exp);
        }
    }
    println!("\tret");
}

// TODO: better printing of assembly so that it's evenly spaced
fn generate_expression_asm(exp: Expression) {
    match exp {
        Expression::Const(u) => {
            println!("\tmov  w0, {u}");
        }
        Expression::UnOp(op, nested_exp) => {
            generate_expression_asm(*nested_exp);
            match op {
                UnaryOp::Negation => println!("\tneg  w0, w0"),
                UnaryOp::LogicalNegation => {
                    println!("\tcmp  w0, wzr");
                    println!("\tcset w0, eq");
                    println!("\tuxtb w0, w0")
                }
                UnaryOp::BitwiseComplement => println!("\tmvn  w0, w0"),
            }
        }
    };
}
