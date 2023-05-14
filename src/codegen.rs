use std::fs::File;
use std::io::Write;

use crate::parser::{Expression, Factor, Function, Program, Statement, Term};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
// TODO: make a trait s.t. we can generate x86 and ARM
pub enum Arch {
    x86,
    ARM,
}

pub struct AsmGenerator {
    asm_file: File,
    sp: usize,
}

const WRITELN_EXPECT: &str = "writeln! failed to write instruction to file.";

// TODO: better printing of assembly so that it's evenly spaced
// FIX: how much to subtract stack pointer by? Oh wait, can just add that at the top
impl AsmGenerator {
    pub fn new(asm_filename: &str) -> Self {
        Self {
            asm_file: File::create(asm_filename).expect("Failed to create output .s file."),
            sp: 0,
        }
    }

    fn write_fn_header(&mut self, identifier: &str) {
        writeln!(self.asm_file, ".global _{identifier}").expect(WRITELN_EXPECT);
        writeln!(self.asm_file, ".align 2").expect(WRITELN_EXPECT);
        writeln!(self.asm_file, "\n").expect(WRITELN_EXPECT);
        writeln!(self.asm_file, "_{identifier}:").expect(WRITELN_EXPECT);
    }

    fn write_inst(&mut self, inst: &str) {
        writeln!(self.asm_file, "\t{inst}").expect(WRITELN_EXPECT);
    }

    fn push_stack(&mut self) {
        self.sp += 4;
        self.write_inst(&format!("str w0, [sp, {}]", self.sp));
    }

    fn pop_stack_into_w1(&mut self) {
        self.write_inst(&format!("ldr w1, [sp, {}]", self.sp));
        self.sp -= 4;
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
                // self.generate_expression_asm(exp);
            }
        }
        self.write_inst("ret");
    }

    /*
    fn generate_expression_asm(&mut self, exp: Expression) {
        let (first_term, trailing_terms) = exp;
        self.generate_term_asm(first_term);

        for (op, term) in trailing_terms {
            self.push_stack();
            self.generate_term_asm(term);
            self.pop_stack_into_w1();
            match op {
                AddSubtract::Addition => self.write_inst("add w0, w0, w1"),
                AddSubtract::Subtraction => self.write_inst("sub w0, w1, w0"),
            }
        }
    }

    fn generate_term_asm(&mut self, term: Term) {
        let (first_factor, trailing_factors) = term;
        self.generate_factor_asm(first_factor);

        for (op, factor) in trailing_factors {
            self.push_stack();
            self.generate_factor_asm(factor);
            self.pop_stack_into_w1();
            match op {
                MultiplyDivide::Multiply => self.write_inst("mul w0, w0, w1"),
                MultiplyDivide::Divide => self.write_inst("sdiv w0, w1, w0"),
            }
        }
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
                        self.write_inst("uxtb w0, w0");
                    }
                    UnaryOp::BitwiseComplement => self.write_inst("mvn  w0, w0"),
                }
            }
            Factor::ParenExp(exp) => self.generate_expression_asm(*exp),
        }
    }
    */
}
