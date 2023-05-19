use std::{fs::File, io::Write};

use crate::{
    codegen_enums::{Arch, Cond, Mnemonic},
    parser_types::*,
};

pub trait AsmGenerator {
    const PRIMARY_REGISTER: &'static str;
    const BACKUP_REGISTER: &'static str;
    const DEFAULT_ARGS: &'static str;

    fn write_to_buffer(&mut self, s: String);
    fn get_buffer(&self) -> &[String];
    fn write_to_file(&mut self, asm_filename: &str) {
        let mut asm_file = File::create(asm_filename).expect("Failed to create output .s file.");
        for line in self.get_buffer() {
            writeln!(asm_file, "{line}").expect("writeln! failed to write instruction to file.");
        }
    }

    fn write_inst(&mut self, inst: &str) {
        self.write_to_buffer(format!("\t{inst}"));
    }

    fn write_mnemonic(&mut self, mnemonic: Mnemonic) {
        self.write_inst(&format!(
            "{:4} {}",
            mnemonic.for_arch(self.get_arch()),
            Self::DEFAULT_ARGS
        ));
    }

    fn get_arch(&self) -> Arch;

    fn write_fn_header(&mut self, identifier: &str);
    fn fn_prologue(&mut self, identifier: &str);
    fn fn_epilogue(&mut self);
    fn ret(&mut self);

    fn save_to_stack(&mut self, stack_offset: usize) {
        self.write_inst(&format!("str  w0, [sp, {stack_offset}]"));
    }

    fn push_stack(&mut self);
    fn pop_stack_into_backup(&mut self);

    fn logical_comparison(&mut self, reg_a: &str, reg_b: &str, cond: Cond);
    fn get_next_jmp_label(&mut self) -> usize;
    fn write_jmp_label(&mut self, lbl: usize);
    fn write_branch_inst(&mut self, cond: Cond, lbl: usize);

    fn cmp_primary_with_zero(&mut self);

    fn gen_block_item_asm(&mut self, block_item: BlockItem);

    fn mov_into_primary(&mut self, val: &str) {
        self.write_inst(&format!("mov  {}, {val}", Self::PRIMARY_REGISTER));
    }

    fn gen_asm(&mut self, asm_filename: &str, prog: Program) {
        match prog {
            Program::Func(func) => match func {
                Function::Fun(identifier, block_items) => {
                    self.fn_prologue(&identifier);
                    self.gen_block_asm(block_items);
                    self.ret();
                }
            },
        };

        self.write_to_file(asm_filename);
    }

    fn gen_block_asm(&mut self, block_items: Vec<BlockItem>) {
        for block_item in block_items {
            self.gen_block_item_asm(block_item);
        }
    }

    fn gen_stmt_asm(&mut self, stmt: Statement) {
        match stmt {
            Statement::Return(exp_opt) => match exp_opt {
                Some(exp) => self.gen_l15_asm(exp),
                // C standard states that int fns without a return value return 0 by default
                None => self.mov_into_primary("0"),
            },
            Statement::Exp(exp) => self.gen_l15_asm(exp),
            Statement::If(exp, predicate, else_opt) => {
                let else_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();

                // Evaluate exp and store in w0
                self.gen_l15_asm(exp);

                // If the exp == 0, trigger the else case to minimize the jump
                // instructions we need
                self.cmp_primary_with_zero();

                self.write_branch_inst(Cond::Equals, else_label);

                // Execute if the if-exp is nonzero
                self.gen_stmt_asm(*predicate);
                self.write_branch_inst(Cond::Always, exit_label);

                // If we don't have an else case, jumping to this label
                // will just cause us to fall through to the exit label
                self.write_jmp_label(else_label);

                if let Some(else_stmt) = else_opt {
                    self.gen_stmt_asm(*else_stmt);
                }
                self.write_jmp_label(exit_label);
            }
            Statement::Compound(block_items) => self.gen_block_asm(block_items),
        }
    }

    fn gen_l14_asm(&mut self, l14: Level14Exp);
    fn gen_l2_asm(&mut self, l2: Level2Exp);

    fn gen_l15_asm(&mut self, l15: Level15Exp) {
        let (first_l14_exp, trailing_l14_exps) = l15.0;
        self.gen_l14_asm(first_l14_exp);

        for (op, l14_exp) in trailing_l14_exps {
            self.push_stack();
            self.gen_l14_asm(l14_exp);
            self.pop_stack_into_backup();
            match op {
                Level15Op::Comma => todo!("codegen ,"),
            }
        }
    }

    fn gen_l13_asm(&mut self, l13: Level13Exp) {
        match l13 {
            Level13Exp::Ternary(exp, pred_exp, else_exp) => {
                let else_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();

                self.gen_l12_asm(exp);

                // If the exp == 0, trigger the else case to minimize the jump
                // instructions we need
                self.cmp_primary_with_zero();

                self.write_branch_inst(Cond::Equals, else_label);

                // Execute if the if-exp is nonzero
                self.gen_l15_asm(*pred_exp);
                self.write_branch_inst(Cond::Always, exit_label);

                self.write_jmp_label(else_label);
                self.gen_l13_asm(*else_exp);

                self.write_jmp_label(exit_label);
            }
            Level13Exp::NoTernary(l12) => self.gen_l12_asm(l12),
        }
    }

    fn gen_l12_asm(&mut self, l12: Level12Exp) {
        let short_circuit_label = self.get_next_jmp_label();
        let exit_label = self.get_next_jmp_label();

        let (first_l11_exp, trailing_l11_exps) = l12.0;
        let print_jmp_insts = !trailing_l11_exps.is_empty();

        self.gen_l11_asm(first_l11_exp);

        for (_op, l11_exp) in trailing_l11_exps {
            self.cmp_primary_with_zero();
            self.write_branch_inst(Cond::NotEquals, short_circuit_label);

            self.gen_l11_asm(l11_exp);
        }

        if print_jmp_insts {
            self.cmp_primary_with_zero();
            self.write_branch_inst(Cond::NotEquals, short_circuit_label);

            self.mov_into_primary("0");
            self.write_branch_inst(Cond::Always, exit_label);

            self.write_jmp_label(short_circuit_label);
            self.mov_into_primary("1");

            self.write_jmp_label(exit_label);
        }
    }

    fn gen_l11_asm(&mut self, l11: Level11Exp) {
        let short_circuit_label = self.get_next_jmp_label();
        let success_label = self.get_next_jmp_label();

        let (first_l10_exp, trailing_l10_exps) = l11.0;
        let print_jmp_insts = !trailing_l10_exps.is_empty();

        self.gen_l10_asm(first_l10_exp);

        for (_op, l10_exp) in trailing_l10_exps {
            self.cmp_primary_with_zero();
            self.write_branch_inst(Cond::Equals, short_circuit_label);

            self.gen_l10_asm(l10_exp);
        }

        if print_jmp_insts {
            // Get the last term checked
            self.cmp_primary_with_zero();
            self.write_branch_inst(Cond::Equals, short_circuit_label);

            self.mov_into_primary("1");
            self.write_branch_inst(Cond::Always, success_label);

            self.write_jmp_label(short_circuit_label);
            self.mov_into_primary("0");

            self.write_jmp_label(success_label);
        }
    }

    fn gen_l10_asm(&mut self, l10: Level10Exp) {
        let (first_l9_exp, trailing_l9_exps) = l10.0;
        self.gen_l9_asm(first_l9_exp);

        for (_op, l9_exp) in trailing_l9_exps {
            self.push_stack();
            self.gen_l9_asm(l9_exp);
            self.pop_stack_into_backup();
            self.write_mnemonic(Mnemonic::Or);
        }
    }

    fn gen_l9_asm(&mut self, l9: Level9Exp) {
        let (first_l8_exp, trailing_l8_exps) = l9.0;
        self.gen_l8_asm(first_l8_exp);

        for (_op, l8_exp) in trailing_l8_exps {
            self.push_stack();
            self.gen_l8_asm(l8_exp);
            self.pop_stack_into_backup();
            self.write_mnemonic(Mnemonic::Xor);
        }
    }

    fn gen_l8_asm(&mut self, l8: Level8Exp) {
        let (first_l7_exp, trailing_l7_exps) = l8.0;
        self.gen_l7_asm(first_l7_exp);

        for (_op, l7_exp) in trailing_l7_exps {
            self.push_stack();
            self.gen_l7_asm(l7_exp);
            self.pop_stack_into_backup();
            self.write_mnemonic(Mnemonic::And);
        }
    }

    fn gen_l7_asm(&mut self, l7: Level7Exp) {
        let (first_l6_exp, trailing_l6_exps) = l7.0;
        self.gen_l6_asm(first_l6_exp);

        for (op, l6_exp) in trailing_l6_exps {
            self.push_stack();
            self.gen_l6_asm(l6_exp);
            self.pop_stack_into_backup();
            self.logical_comparison(Self::BACKUP_REGISTER, Self::PRIMARY_REGISTER, op.into());
        }
    }

    fn gen_l6_asm(&mut self, l6: Level6Exp) {
        let (first_l5_exp, trailing_l5_exps) = l6.0;
        self.gen_l5_asm(first_l5_exp);

        for (op, l5_exp) in trailing_l5_exps {
            self.push_stack();
            self.gen_l5_asm(l5_exp);
            self.pop_stack_into_backup();
            self.logical_comparison(Self::BACKUP_REGISTER, Self::PRIMARY_REGISTER, op.into());
        }
    }

    fn gen_l5_asm(&mut self, l5: Level5Exp) {
        let (first_l4_exp, trailing_l4_exps) = l5.0;
        self.gen_l4_asm(first_l4_exp);

        for (op, l4_exp) in trailing_l4_exps {
            self.push_stack();
            self.gen_l4_asm(l4_exp);
            self.pop_stack_into_backup();
            match op {
                Level5Op::BitwiseLeftShift => todo!("Codegen <<"),
                Level5Op::BitwiseRightShift => todo!("Codegen >>"),
            }
        }
    }

    fn gen_l4_asm(&mut self, l4: Level4Exp) {
        let (first_l3_exp, trailing_l3_exps) = l4.0;
        self.gen_l3_asm(first_l3_exp);

        for (op, l3_exp) in trailing_l3_exps {
            self.push_stack();
            self.gen_l3_asm(l3_exp);
            self.pop_stack_into_backup();
            match op {
                Level4Op::Addition => self.write_mnemonic(Mnemonic::Add),
                Level4Op::Subtraction => self.write_mnemonic(Mnemonic::Subtract),
            }
        }
    }

    fn gen_l3_asm(&mut self, l3: Level3Exp) {
        let (first_l2_exp, trailing_l2_exps) = l3.0;
        self.gen_l2_asm(first_l2_exp);

        for (op, l2_exp) in trailing_l2_exps {
            self.push_stack();
            self.gen_l2_asm(l2_exp);
            self.pop_stack_into_backup();
            match op {
                Level3Op::Multiplication => self.write_mnemonic(Mnemonic::Multiply),
                Level3Op::Division => self.write_mnemonic(Mnemonic::Divide),
                Level3Op::Remainder => todo!("Codegen %"),
            }
        }
    }
}
