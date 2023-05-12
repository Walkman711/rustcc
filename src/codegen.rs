use std::fs::File;
use std::io::Write;

use crate::parser::{Expression, Factor, Function, Program, Statement, Term, UnaryOp};

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
    // FIX: handle trailing terms
    let term = exp.0;
    generate_term_asm(term, asm_file);

    // match exp {

    // }
    // match exp {
    //     Expression::Binary(_, _, _) => todo!(),
    //     Expression::Fact(f) => generate_factor_asm(f, asm_file),
    // };
}

fn generate_term_asm(t: Term, asm_file: &mut File) {
    // TODO: handle trailing terms
    let factor = t.0;
    generate_factor_asm(factor, asm_file);
}

fn generate_factor_asm(f: Factor, asm_file: &mut File) {
    match f {
        Factor::Const(u) => {
            writeln!(asm_file, "\tmov  w0, {u}").unwrap();
        }
        Factor::Unary(op, factor) => {
            generate_factor_asm(*factor, asm_file);
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
        Factor::ParenExp(_) => todo!(),
    }
}
