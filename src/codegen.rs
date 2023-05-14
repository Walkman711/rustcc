use std::{fs::File, io::Write};

use crate::{
    parser::{
        AdditiveExpression, EqualityExpression, Expression, Factor, Function, LogicalAndExpression,
        Program, RelationalExpression, Statement, Term,
    },
    parser_enums::{AdditiveOp, EqualityOp, MultiplicativeOp, RelationOp, UnaryOp},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
// TODO: make a trait s.t. we can generate x86 and ARM
pub enum Arch {
    x86,
    ARM,
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

impl From<EqualityOp> for Cond {
    fn from(value: EqualityOp) -> Self {
        match value {
            EqualityOp::Equals => Self::Equals,
            EqualityOp::NotEquals => Self::NotEquals,
        }
    }
}

impl From<RelationOp> for Cond {
    fn from(value: RelationOp) -> Self {
        match value {
            RelationOp::LessThan => Self::LessThan,
            RelationOp::LessThanEquals => Self::LessThanEquals,
            RelationOp::GreaterThan => Self::GreaterThan,
            RelationOp::GreaterThanEquals => Self::GreaterThanEquals,
        }
    }
}

pub struct AsmGenerator {
    asm_file: File,
    sp: usize,
    curr_jmp_label: usize,
}

const WRITELN_EXPECT: &str = "writeln! failed to write instruction to file.";

// TODO: better printing of assembly so that it's evenly spaced
// FIX: how much to subtract stack pointer by? Oh wait, can just add that at the top
impl AsmGenerator {
    pub fn new(asm_filename: &str) -> Self {
        Self {
            asm_file: File::create(asm_filename).expect("Failed to create output .s file."),
            sp: 0,
            curr_jmp_label: 0,
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

    fn write_jmp_label(&mut self, lbl: usize) {
        writeln!(self.asm_file, ".L{lbl}:").expect(WRITELN_EXPECT);
    }

    fn write_branch_inst(&mut self, cond: Cond, lbl: usize) {
        self.write_inst(&format!("b{cond} .L{lbl}"));
    }

    fn push_stack(&mut self) {
        self.sp += 4;
        self.write_inst(&format!("str  w0, [sp, {}]", self.sp));
    }

    fn pop_stack_into_w1(&mut self) {
        self.write_inst(&format!("ldr  w1, [sp, {}]", self.sp));
        self.sp -= 4;
    }

    fn logical_comparison(&mut self, reg_a: &str, reg_b: &str, cond: Cond) {
        // Compare registers
        self.write_inst(&format!("cmp  {reg_a}, {reg_b}"));
        // set lower byte of reg_a based on if cond is satisfied
        self.write_inst(&format!("cset w0, {cond}"));
        // zero-pad reg_a since cset only sets the lower byte
        self.write_inst(&format!("uxtb w0, w0"));
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
                self.generate_exp_asm(exp);
            }
        }
        self.write_inst("ret");
    }

    fn generate_exp_asm(&mut self, exp: Expression) {
        let short_circuit_label = self.curr_jmp_label;
        let exit_label = self.curr_jmp_label + 1;
        self.curr_jmp_label += 2;

        let (first_logical_and_exp, trailing_logical_and_exps) = exp;
        let print_jmp_insts = !trailing_logical_and_exps.is_empty();

        self.generate_logical_and_exp_asm(first_logical_and_exp);

        for logical_and_exp in trailing_logical_and_exps {
            self.write_inst("cmp  w0, wzr");
            self.write_branch_inst(Cond::NotEquals, short_circuit_label);

            self.generate_logical_and_exp_asm(logical_and_exp);
        }

        if print_jmp_insts {
            self.write_inst("cmp  w0, wzr");
            self.write_branch_inst(Cond::NotEquals, short_circuit_label);

            self.write_inst("mov  w0, wzr");
            self.write_branch_inst(Cond::Always, exit_label);

            self.write_jmp_label(short_circuit_label);
            self.write_inst("mov  w0, 1");

            self.write_jmp_label(exit_label);
        }
    }

    fn generate_logical_and_exp_asm(&mut self, logical_and_exp: LogicalAndExpression) {
        let short_circuit_label = self.curr_jmp_label;
        let success_label = self.curr_jmp_label + 1;
        self.curr_jmp_label += 2;

        let (first_equality_exp, trailing_equality_exps) = logical_and_exp;
        let print_jmp_insts = !trailing_equality_exps.is_empty();

        self.generate_equality_exp_asm(first_equality_exp);

        for equality_exp in trailing_equality_exps {
            self.write_inst("cmp  w0, wzr");
            self.write_branch_inst(Cond::Equals, short_circuit_label);

            self.generate_equality_exp_asm(equality_exp);
        }

        if print_jmp_insts {
            // Get the last term checked
            self.write_inst("cmp  w0, wzr");
            self.write_branch_inst(Cond::Equals, short_circuit_label);

            self.write_inst("mov  w0, 1");
            self.write_branch_inst(Cond::Always, success_label);

            self.write_jmp_label(short_circuit_label);
            self.write_inst("mov  w0, wzr");

            self.write_jmp_label(success_label);
        }
    }

    fn generate_equality_exp_asm(&mut self, equality_exp: EqualityExpression) {
        let (first_relational_exp, trailing_relational_exps) = equality_exp;
        self.generate_relational_exp_asm(first_relational_exp);

        for (op, relational_exp) in trailing_relational_exps {
            self.push_stack();
            self.generate_relational_exp_asm(relational_exp);
            self.pop_stack_into_w1();
            let cond: Cond = op.into();
            match op {
                EqualityOp::Equals => self.logical_comparison("w1", "w0", cond),
                EqualityOp::NotEquals => self.logical_comparison("w1", "w0", cond),
            }
        }
    }

    fn generate_relational_exp_asm(&mut self, relational_exp: RelationalExpression) {
        let (first_additive_exp, trailing_additive_exps) = relational_exp;
        self.generate_additive_exp_asm(first_additive_exp);

        for (op, additive_exp) in trailing_additive_exps {
            self.push_stack();
            self.generate_additive_exp_asm(additive_exp);
            self.pop_stack_into_w1();
            let cond: Cond = op.into();
            match op {
                RelationOp::LessThan => self.logical_comparison("w1", "w0", cond),
                RelationOp::LessThanEquals => self.logical_comparison("w1", "w0", cond),
                RelationOp::GreaterThan => self.logical_comparison("w1", "w0", cond),
                RelationOp::GreaterThanEquals => self.logical_comparison("w1", "w0", cond),
            }
        }
    }

    fn generate_additive_exp_asm(&mut self, additive_exp: AdditiveExpression) {
        let (first_term, trailing_terms) = additive_exp;
        self.generate_term_asm(first_term);

        for (op, term) in trailing_terms {
            self.push_stack();
            self.generate_term_asm(term);
            self.pop_stack_into_w1();
            match op {
                AdditiveOp::Addition => self.write_inst("add w0, w0, w1"),
                AdditiveOp::Subtraction => self.write_inst("sub w0, w1, w0"),
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
                MultiplicativeOp::Multiply => self.write_inst("mul w0, w0, w1"),
                MultiplicativeOp::Divide => self.write_inst("sdiv w0, w1, w0"),
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
            Factor::ParenExp(exp) => self.generate_exp_asm(*exp),
        }
    }
}
