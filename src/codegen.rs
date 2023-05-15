use std::{fs::File, io::Write};

use crate::{
    codegen_enums::Cond,
    parser::{
        AdditiveExpression, EqualityExpression, Expression, Factor, Function, LogicalAndExpression,
        Program, RelationalExpression, Statement, Term,
    },
    parser_enums::{AdditiveOp, EqualityOp, MultiplicativeOp, RelationOp, UnaryOp},
    parser_precedence::*,
};

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
        writeln!(self.asm_file).expect(WRITELN_EXPECT);
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

impl AsmGenerator {
    fn generate_l15_asm(&mut self, l15: Level15Exp) {
        let (first_l14_exp, trailing_l14_exps) = l15;
        self.generate_l14_asm(first_l14_exp);

        for (op, l14_exp) in trailing_l14_exps {
            self.push_stack();
            self.generate_l14_asm(l14_exp);
            self.pop_stack_into_w1();
            match op {
                Level15Op::Comma => todo!("codegen ,"),
            }
        }
    }

    fn generate_l14_asm(&mut self, l14: Level14Exp) {
        let (first_l13_exp, trailing_l13_exps) = l14;
        self.generate_l13_asm(first_l13_exp);

        for (op, l13_exp) in trailing_l13_exps {
            self.push_stack();
            self.generate_l13_asm(l13_exp);
            self.pop_stack_into_w1();
            match op {
                _ => todo!("codegen assignment ops"),
            }
        }
    }

    fn generate_l13_asm(&mut self, l13: Level13Exp) {
        let (first_l12_exp, trailing_l12_exps) = l13;
        self.generate_l12_asm(first_l12_exp);

        for (op, l12_exp) in trailing_l12_exps {
            self.push_stack();
            self.generate_l12_asm(l12_exp);
            self.pop_stack_into_w1();
            match op {
                Level13Op::TernaryConditional => todo!("TernaryConditional"),
            }
        }
    }

    fn generate_l12_asm(&mut self, l12: Level12Exp) {
        let short_circuit_label = self.curr_jmp_label;
        let exit_label = self.curr_jmp_label + 1;
        self.curr_jmp_label += 2;

        let (first_l11_exp, trailing_l11_exps) = l12;
        let print_jmp_insts = !trailing_l11_exps.is_empty();

        self.generate_l11_asm(first_l11_exp);

        for (_op, l11_exp) in trailing_l11_exps {
            self.write_inst("cmp  w0, wzr");
            self.write_branch_inst(Cond::NotEquals, short_circuit_label);

            self.generate_l11_asm(l11_exp);
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

    fn generate_l11_asm(&mut self, l11: Level11Exp) {
        let short_circuit_label = self.curr_jmp_label;
        let success_label = self.curr_jmp_label + 1;
        self.curr_jmp_label += 2;

        let (first_l10_exp, trailing_l10_exps) = l11;
        let print_jmp_insts = !trailing_l10_exps.is_empty();

        self.generate_l10_asm(first_l10_exp);

        for (_op, l10_exp) in trailing_l10_exps {
            self.write_inst("cmp  w0, wzr");
            self.write_branch_inst(Cond::Equals, short_circuit_label);

            self.generate_l10_asm(l10_exp);
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

    fn generate_l10_asm(&mut self, l10: Level10Exp) {
        let (first_l9_exp, trailing_l9_exps) = l10;
        self.generate_l9_asm(first_l9_exp);

        for (op, l9_exp) in trailing_l9_exps {
            self.push_stack();
            self.generate_l9_asm(l9_exp);
            self.pop_stack_into_w1();
            match op {
                Level10Op::BitwiseOr => todo!("Codegen |"),
            }
        }
    }

    fn generate_l9_asm(&mut self, l9: Level9Exp) {
        let (first_l8_exp, trailing_l8_exps) = l9;
        self.generate_l8_asm(first_l8_exp);

        for (op, l8_exp) in trailing_l8_exps {
            self.push_stack();
            self.generate_l8_asm(l8_exp);
            self.pop_stack_into_w1();
            match op {
                Level9Op::BitwiseXor => todo!("Codegen ^"),
            }
        }
    }

    fn generate_l8_asm(&mut self, l8: Level8Exp) {
        let (first_l7_exp, trailing_l7_exps) = l8;
        self.generate_l7_asm(first_l7_exp);

        for (op, l7_exp) in trailing_l7_exps {
            self.push_stack();
            self.generate_l7_asm(l7_exp);
            self.pop_stack_into_w1();
            match op {
                Level8Op::BitwiseAnd => todo!("Codegen & (bitwise and)"),
            }
        }
    }

    fn generate_l7_asm(&mut self, l7: Level7Exp) {
        let (first_l6_exp, trailing_l6_exps) = l7;
        self.generate_l6_asm(first_l6_exp);

        for (op, l6_exp) in trailing_l6_exps {
            self.push_stack();
            self.generate_l6_asm(l6_exp);
            self.pop_stack_into_w1();
            let cond: Cond = op.into();
            match op {
                Level7Op::Equals => self.logical_comparison("w1", "w0", cond),
                Level7Op::NotEquals => self.logical_comparison("w1", "w0", cond),
            }
        }
    }

    fn generate_l6_asm(&mut self, l6: Level6Exp) {
        let (first_l5_exp, trailing_l5_exps) = l6;
        self.generate_l5_asm(first_l5_exp);

        for (op, l5_exp) in trailing_l5_exps {
            self.push_stack();
            self.generate_l5_asm(l5_exp);
            self.pop_stack_into_w1();
            let cond: Cond = op.into();
            match op {
                Level6Op::LessThan => self.logical_comparison("w1", "w0", cond),
                Level6Op::LessThanEquals => self.logical_comparison("w1", "w0", cond),
                Level6Op::GreaterThan => self.logical_comparison("w1", "w0", cond),
                Level6Op::GreaterThanEquals => self.logical_comparison("w1", "w0", cond),
            }
        }
    }

    fn generate_l5_asm(&mut self, l5: Level5Exp) {
        let (first_l4_exp, trailing_l4_exps) = l5;
        self.generate_l4_asm(first_l4_exp);

        for (op, l4_exp) in trailing_l4_exps {
            self.push_stack();
            self.generate_l4_asm(l4_exp);
            self.pop_stack_into_w1();
            match op {
                Level5Op::BitwiseLeftShift => todo!("Codegen <<"),
                Level5Op::BitwiseRightShift => todo!("Codegen >>"),
            }
        }
    }

    fn generate_l4_asm(&mut self, l4: Level4Exp) {
        let (first_l3_exp, trailing_l3_exps) = l4;
        self.generate_l3_asm(first_l3_exp);

        for (op, l3_exp) in trailing_l3_exps {
            self.push_stack();
            self.generate_l3_asm(l3_exp);
            self.pop_stack_into_w1();
            match op {
                Level4Op::Addition => self.write_inst("add w0, w0, w1"),
                Level4Op::Subtraction => self.write_inst("sub w0, w1, w0"),
            }
        }
    }

    fn generate_l3_asm(&mut self, l3: Level3Exp) {
        let (first_l2_exp, trailing_l2_exps) = l3;
        self.generate_l2_asm(first_l2_exp);

        for (op, l2_exp) in trailing_l2_exps {
            self.push_stack();
            self.generate_l2_asm(l2_exp);
            self.pop_stack_into_w1();
            match op {
                Level3Op::Multiplication => self.write_inst("mul w0, w0, w1"),
                Level3Op::Division => self.write_inst("sdiv w0, w1, w0"),
                Level3Op::Remainder => todo!("Codegen %"),
            }
        }
    }

    fn generate_l2_asm(&mut self, l2: Level2Exp) {
        match l2 {
            Level2Exp::Const(u) => {
                self.write_inst(&format!("mov  w0, {u}"));
            }
            Level2Exp::Unary(op, factor) => {
                self.generate_l2_asm(*factor);
                match op {
                    Level2Op::PrefixIncrement => todo!(),
                    Level2Op::PrefixDecrement => todo!(),
                    Level2Op::UnaryPlus => todo!(),
                    Level2Op::UnaryMinus => self.write_inst("neg  w0, w0"),
                    Level2Op::LogicalNot => {
                        self.write_inst("cmp  w0, wzr");
                        self.write_inst("cset w0, eq");
                        self.write_inst("uxtb w0, w0");
                    }
                    Level2Op::BitwiseNot => self.write_inst("mvn  w0, w0"),
                    Level2Op::Cast => todo!(),
                    Level2Op::Dereference => todo!(),
                    Level2Op::AddressOf => todo!(),
                    Level2Op::SizeOf => todo!(),
                    Level2Op::Align => todo!(),
                }
            }
            Level2Exp::ParenExp(exp) => self.generate_l15_asm(*exp),
        }
    }
}
