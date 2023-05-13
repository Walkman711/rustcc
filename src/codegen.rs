use std::fs::File;
use std::io::Write;

use crate::parser::{Expression, Factor, Function, Program, Statement, Term, UnaryOp};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum Arch {
    x86,
    ARM,
}

pub struct AsmGenerator {
    asm_file: File,
}

impl AsmGenerator {
    pub fn new(asm_filename: &str) -> Self {
        Self {
            asm_file: File::create(asm_filename).unwrap(),
        }
    }

    fn write_fn_header(&mut self, identifier: &str) {
        writeln!(self.asm_file, ".global _{identifier}").unwrap();
        writeln!(self.asm_file, ".align 2").unwrap();
        writeln!(self.asm_file, "\n").unwrap();
        writeln!(self.asm_file, "_{identifier}:").unwrap();
    }

    fn write_inst(&mut self, inst: &str) {
        writeln!(self.asm_file, "\t{inst}").unwrap();
    }
}

impl AsmGenerator {
    pub fn generate_asm(&mut self, prog: Program) {
        match prog {
            Program::Func(func) => match func {
                Function::Fun(identifier, stmt) => {
                    self.write_fn_header(&identifier);
                    self.generate_stmt_asm(stmt);
                }
            },
        }
    }

    fn generate_stmt_asm(&mut self, stmt: Statement) {
        match stmt {
            Statement::Return(exp) => {
                self.generate_expression_asm(exp);
            }
        }
        self.write_inst("ret");
    }

    // TODO: better printing of assembly so that it's evenly spaced
    fn generate_expression_asm(&mut self, exp: Expression) {
        // FIX: handle trailing terms
        let term = exp.0;
        self.generate_term_asm(term);

        // match exp {

        // }
        // match exp {
        //     Expression::Binary(_, _, _) => todo!(),
        //     Expression::Fact(f) => generate_factor_asm(f, asm_file),
        // };
    }

    fn generate_term_asm(&mut self, t: Term) {
        // FIX: handle trailing terms
        let factor = t.0;
        self.generate_factor_asm(factor);
    }

    fn generate_factor_asm(&mut self, f: Factor) {
        match f {
            Factor::Const(u) => {
                self.write_inst(&format!("mov  w0, {u}"));
            }
            Factor::Unary(op, factor) => {
                self.generate_factor_asm(*factor);
                match op {
                    UnaryOp::Negation => self.write_inst("neg  w0, w0"),
                    UnaryOp::LogicalNegation => {
                        self.write_inst("cmp  w0, wzr");
                        self.write_inst("cset w0, eq");
                        self.write_inst("utxb w0, w0");
                    }
                    UnaryOp::BitwiseComplement => self.write_inst("mvn  w0, w0"),
                }
            }
            Factor::ParenExp(_) => todo!(),
        }
    }
}
