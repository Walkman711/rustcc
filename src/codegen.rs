use std::fs::File;
use std::io::Write;

use crate::parser::{Expression, Function, Program, Statement, UnaryOp};

pub fn generate_asm(prog: Program, asm_filename: &str) {
    let mut asm_file = File::create(asm_filename).unwrap();
    match prog {
        Program::Func(func) => match func {
            Function::Fun(identifier, stmt) => {
                writeln!(&mut asm_file, ".global _{identifier}").unwrap();
                writeln!(&mut asm_file, ".align 2").unwrap();
                writeln!(&mut asm_file,).unwrap();
                writeln!(&mut asm_file, "_{identifier}:").unwrap();
                generate_stmt_asm(stmt, &mut asm_file);
            }
        },
    }
}

fn generate_stmt_asm(stmt: Statement, asm_file: &mut File) {
    match stmt {
        Statement::Return(exp) => {
            generate_expression_asm(exp, asm_file);
        }
    }
    writeln!(asm_file, "\tret").unwrap();
}

// TODO: better printing of assembly so that it's evenly spaced
fn generate_expression_asm(exp: Expression, asm_file: &mut File) {
    match exp {
        Expression::Const(u) => {
            writeln!(asm_file, "\tmov  w0, {u}").unwrap();
        }
        Expression::UnOp(op, nested_exp) => {
            generate_expression_asm(*nested_exp, asm_file);
            match op {
                UnaryOp::Negation => writeln!(asm_file, "\tneg  w0, w0").unwrap(),
                UnaryOp::LogicalNegation => {
                    writeln!(asm_file, "\tcmp  w0, wzr").unwrap();
                    writeln!(asm_file, "\tcset w0, eq").unwrap();
                    writeln!(asm_file, "\tuxtb w0, w0").unwrap();
                }
                UnaryOp::BitwiseComplement => writeln!(asm_file, "\tmvn  w0, w0").unwrap(),
            }
        }
    };
}
