use std::{fs::File, io::Write};

use crate::{
    codegen_enums::{Arch, Cond, Mnemonic},
    gen_level_asm,
    parser_types::*,
};

pub trait AsmGenerator {
    const PRIMARY_REGISTER: &'static str;
    const BACKUP_REGISTER: &'static str;
    const DEFAULT_ARGS: &'static str;
    const UNARY_ARGS: &'static str;

    fn get_variable(&self, var: &str) -> Option<&usize>;
    fn save_variable(&mut self, var: &str);

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
            "{:5} {}",
            mnemonic.for_arch(self.get_arch()),
            Self::DEFAULT_ARGS
        ));
    }

    fn write_unary_inst(&mut self, mnemonic: Mnemonic) {
        self.write_inst(&format!(
            "{:5} {}",
            mnemonic.for_arch(self.get_arch()),
            Self::UNARY_ARGS
        ));
    }

    fn get_arch(&self) -> Arch;

    fn write_fn_header(&mut self, identifier: &str);
    fn fn_prologue(&mut self, identifier: &str);
    fn fn_epilogue(&mut self);
    fn ret(&mut self);

    fn stack_ptr(&self) -> usize;
    fn increment_stack_ptr(&mut self);
    fn decrement_stack_ptr(&mut self);
    fn load_from_stack(&mut self, reg: &str, stack_offset: usize);
    fn pop_stack_into_backup(&mut self) {
        self.load_from_stack(Self::BACKUP_REGISTER, self.stack_ptr());
        self.decrement_stack_ptr();
    }
    fn push_stack(&mut self) {
        self.increment_stack_ptr();
        self.save_to_stack(self.stack_ptr());
    }
    fn save_to_stack(&mut self, stack_offset: usize) {
        self.write_inst(&format!("str   w0, [sp, {stack_offset}]"));
    }

    fn logical_comparison(&mut self, cond: Cond);
    fn logical_not(&mut self);

    fn get_next_jmp_label(&mut self) -> usize;
    fn write_branch_inst(&mut self, cond: Cond, lbl: usize);
    fn write_jmp_label(&mut self, lbl: usize) {
        self.write_to_buffer(format!(".L{lbl}:"));
    }

    fn cmp_primary_with_zero(&mut self) {
        self.write_inst(&format!("cmp   {}, 0", Self::PRIMARY_REGISTER));
    }

    fn mov_into_primary(&mut self, val: &str) {
        self.write_inst(&format!("mov   {}, {val}", Self::PRIMARY_REGISTER));
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

    fn gen_block_item_asm(&mut self, block_item: BlockItem) {
        match block_item {
            BlockItem::Stmt(s) => self.gen_stmt_asm(s),
            BlockItem::Declare((identifier, exp_opt)) => {
                if self.get_variable(&identifier).is_some() {
                    panic!("tried to initialize variable `{identifier}` multiple times");
                }

                // Store location of variable in stack
                self.save_variable(&identifier);
                if let Some(exp) = exp_opt {
                    self.gen_l15_asm(exp);
                    self.push_stack();
                } else {
                    self.mov_into_primary("0");
                    self.push_stack();
                }
            }
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

    fn gen_l14_asm(&mut self, l14: Level14Exp) {
        match l14 {
            Level14Exp::SimpleAssignment(identifier, l15_exp) => {
                let stack_offset = self
                    .get_variable(&identifier)
                    .unwrap_or_else(|| panic!("`{identifier}` is uninitialized"))
                    .to_owned();
                self.gen_l15_asm(*l15_exp);
                self.save_to_stack(stack_offset);
            }
            Level14Exp::NonAssignment(l13_exp) => {
                self.gen_l13_asm(l13_exp);
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

    gen_level_asm!(gen_l10_asm, Level10Exp, gen_l9_asm, write_mnemonic);
    gen_level_asm!(gen_l9_asm, Level9Exp, gen_l8_asm, write_mnemonic);
    gen_level_asm!(gen_l8_asm, Level8Exp, gen_l7_asm, write_mnemonic);
    gen_level_asm!(gen_l7_asm, Level7Exp, gen_l6_asm, logical_comparison);
    gen_level_asm!(gen_l6_asm, Level6Exp, gen_l5_asm, logical_comparison);
    gen_level_asm!(gen_l5_asm, Level5Exp, gen_l4_asm, write_mnemonic);
    gen_level_asm!(gen_l4_asm, Level4Exp, gen_l3_asm, write_mnemonic);
    gen_level_asm!(gen_l3_asm, Level3Exp, gen_l2_asm, write_mnemonic);

    fn gen_l2_asm(&mut self, l2: Level2Exp) {
        match l2 {
            Level2Exp::Const(u) => self.mov_into_primary(&u.to_string()),
            Level2Exp::Var(identifier) => {
                let stack_offset = self
                    .get_variable(&identifier)
                    .unwrap_or_else(|| panic!("`{identifier}` is uninitialized"))
                    .to_owned();
                self.load_from_stack(Self::PRIMARY_REGISTER, stack_offset);
            }
            Level2Exp::Unary(op, factor) => {
                self.gen_l2_asm(*factor);
                match op {
                    Level2Op::UnaryMinus => self.write_unary_inst(Mnemonic::Neg),
                    Level2Op::LogicalNot => self.logical_not(),
                    Level2Op::BitwiseNot => self.write_unary_inst(Mnemonic::BitwiseNot),
                    _ => todo!("{op} is unimplemented for now"),
                }
            }
            Level2Exp::ParenExp(exp) => self.gen_l15_asm(*exp),
        }
    }
}

#[macro_export]
macro_rules! gen_level_asm {
    ($fn_name: ident, $t: ty, $next_fn: ident, $inst_fn: ident) => {
        fn $fn_name(&mut self, exp: $t) {
            let (first_lower_level_exp, trailing_lower_level_exps) = exp.0;
            self.$next_fn(first_lower_level_exp);

            for (op, lower_level_exp) in trailing_lower_level_exps {
                self.push_stack();
                self.$next_fn(lower_level_exp);
                self.pop_stack_into_backup();
                self.$inst_fn(op.into());
            }
        }
    };
}
