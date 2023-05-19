use std::{collections::HashMap, fs::File, io::Write};

use crate::{codegen_enums::Cond, parser_types::*};

pub struct ArmGenerator {
    sp: usize,
    curr_jmp_label: usize,
    var_map: HashMap<String, usize>,
    buffer: Vec<String>,
}

const WRITELN_EXPECT: &str = "writeln! failed to write instruction to file.";

pub trait AsmGenerator {
    const PRIMARY_REGISTER: &'static str;
    const BACKUP_REGISTER: &'static str;
    const DEFAULT_ARGS: &'static str;

    fn write_to_buffer(&mut self, s: String);
    fn write_to_file(&mut self, asm_filename: &str);
    fn write_inst(&mut self, inst: &str);
    fn write_mnemonic(&mut self, mnemonic: &str);

    fn write_fn_header(&mut self, identifier: &str);
    fn fn_prologue(&mut self, identifier: &str);
    fn fn_epilogue(&mut self);
    fn ret(&mut self);

    fn gen_block_item_asm(&mut self, block_item: BlockItem);

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

    fn save_to_stack(&mut self, stack_offset: usize) {
        self.write_inst(&format!("str  w0, [sp, {stack_offset}]"));
    }

    fn mov_into_primary(&mut self, val: &str) {
        self.write_inst(&format!("mov  {}, {val}", Self::PRIMARY_REGISTER));
    }

    fn cmp_primary_with_zero(&mut self);

    fn gen_l15_asm(&mut self, l15: Level15Exp);
    fn gen_l14_asm(&mut self, l14: Level14Exp);
    fn gen_l13_asm(&mut self, l13: Level13Exp);
    fn gen_l12_asm(&mut self, l12: Level12Exp);
    fn gen_l11_asm(&mut self, l11: Level11Exp);
    fn gen_l10_asm(&mut self, l10: Level10Exp);
    fn gen_l9_asm(&mut self, l9: Level9Exp);
    fn gen_l8_asm(&mut self, l8: Level8Exp);
    fn gen_l7_asm(&mut self, l7: Level7Exp);
    fn gen_l6_asm(&mut self, l6: Level6Exp);
    fn gen_l5_asm(&mut self, l5: Level5Exp);
    fn gen_l4_asm(&mut self, l4: Level4Exp);
    fn gen_l3_asm(&mut self, l3: Level3Exp);
    fn gen_l2_asm(&mut self, l2: Level2Exp);
}

impl Default for ArmGenerator {
    fn default() -> Self {
        Self {
            sp: 0,
            curr_jmp_label: 0,
            var_map: HashMap::new(),
            buffer: vec![],
        }
    }
}

impl AsmGenerator for ArmGenerator {
    const PRIMARY_REGISTER: &'static str = "w0";
    const BACKUP_REGISTER: &'static str = "w1";
    const DEFAULT_ARGS: &'static str = "w0, w1, w0";

    fn write_to_buffer(&mut self, s: String) {
        self.buffer.push(s);
    }

    fn write_to_file(&mut self, asm_filename: &str) {
        let mut asm_file = File::create(asm_filename).expect("Failed to create output .s file.");
        for line in &self.buffer {
            writeln!(asm_file, "{line}").expect(WRITELN_EXPECT);
        }
    }

    fn write_inst(&mut self, inst: &str) {
        self.write_to_buffer(format!("\t{inst}"));
    }

    fn write_mnemonic(&mut self, mnemonic: &str) {
        self.write_inst(&format!("{mnemonic:4} {}", Self::DEFAULT_ARGS));
    }

    fn write_fn_header(&mut self, identifier: &str) {
        self.write_to_buffer(format!(".global _{identifier}"));
        self.write_to_buffer(".align 2".to_string());
        self.write_to_buffer(format!("_{identifier}:"));
    }

    fn fn_prologue(&mut self, identifier: &str) {
        self.write_fn_header(identifier);
        // self.write_inst("push lr");
    }

    fn fn_epilogue(&mut self) {
        // self.write_inst("pop pc");
        // self.write_inst(&format!("str  w0, [sp, {}]", self.sp));
    }

    fn ret(&mut self) {
        self.fn_epilogue();
        self.write_inst("ret");
    }

    fn gen_block_item_asm(&mut self, block_item: BlockItem) {
        match block_item {
            BlockItem::Stmt(s) => self.gen_stmt_asm(s),
            BlockItem::Declare((identifier, exp_opt)) => {
                if self.var_map.contains_key(&identifier) {
                    panic!("tried to initialize variable `{identifier}` multiple times");
                }

                // Store location of variable in stack
                self.var_map.insert(identifier, self.sp + 4);
                if let Some(exp) = exp_opt {
                    self.gen_l15_asm(exp);
                    self.push_stack();
                }
            }
        }
    }

    fn cmp_primary_with_zero(&mut self) {
        self.write_inst(&format!("cmp {}, wzr", Self::PRIMARY_REGISTER));
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
                    .var_map
                    .get(&identifier)
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
                let else_label = self.curr_jmp_label;
                let exit_label = self.curr_jmp_label + 1;
                self.curr_jmp_label += 2;

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
        let short_circuit_label = self.curr_jmp_label;
        let exit_label = self.curr_jmp_label + 1;
        self.curr_jmp_label += 2;

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

            self.mov_into_primary("wzr");
            self.write_branch_inst(Cond::Always, exit_label);

            self.write_jmp_label(short_circuit_label);
            self.mov_into_primary("1");

            self.write_jmp_label(exit_label);
        }
    }

    fn gen_l11_asm(&mut self, l11: Level11Exp) {
        let short_circuit_label = self.curr_jmp_label;
        let success_label = self.curr_jmp_label + 1;
        self.curr_jmp_label += 2;

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
            self.mov_into_primary("wzr");

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
            self.write_mnemonic("orr");
        }
    }

    fn gen_l9_asm(&mut self, l9: Level9Exp) {
        let (first_l8_exp, trailing_l8_exps) = l9.0;
        self.gen_l8_asm(first_l8_exp);

        for (_op, l8_exp) in trailing_l8_exps {
            self.push_stack();
            self.gen_l8_asm(l8_exp);
            self.pop_stack_into_backup();
            self.write_mnemonic("eor");
        }
    }

    fn gen_l8_asm(&mut self, l8: Level8Exp) {
        let (first_l7_exp, trailing_l7_exps) = l8.0;
        self.gen_l7_asm(first_l7_exp);

        for (_op, l7_exp) in trailing_l7_exps {
            self.push_stack();
            self.gen_l7_asm(l7_exp);
            self.pop_stack_into_backup();
            self.write_mnemonic("and");
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
                Level4Op::Addition => self.write_mnemonic("add"),
                Level4Op::Subtraction => self.write_mnemonic("sub"),
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
                Level3Op::Multiplication => self.write_mnemonic("mul"),
                Level3Op::Division => self.write_mnemonic("sdiv"),
                Level3Op::Remainder => todo!("Codegen %"),
            }
        }
    }

    fn gen_l2_asm(&mut self, l2: Level2Exp) {
        match l2 {
            Level2Exp::Const(u) => self.mov_into_primary(&u.to_string()),
            Level2Exp::Var(id) => {
                if let Some(stack_offset) = self.var_map.get(&id) {
                    self.write_inst(&format!("ldr  w0, [sp, {}]", stack_offset));
                } else {
                    panic!("{id} not allocated on stack")
                }
            }
            Level2Exp::Unary(op, factor) => {
                self.gen_l2_asm(*factor);
                match op {
                    Level2Op::PrefixIncrement => todo!(),
                    Level2Op::PrefixDecrement => todo!(),
                    Level2Op::UnaryPlus => todo!("unary plus isn't a no-op, and requires getting into lvalue vs rvalue stuff"),
                    Level2Op::UnaryMinus => self.write_inst("neg  w0, w0"),
                    Level2Op::LogicalNot => {
                        self.cmp_primary_with_zero();
                        self.write_inst("cset w0, eq");
                        self.write_inst("uxtb w0, w0");
                    }
                    Level2Op::BitwiseNot => self.write_inst("mvn  w0, w0"),
                    Level2Op::Cast => todo!("only thinking of ints for now, so put off casting"),
                    Level2Op::Dereference => todo!(),
                    Level2Op::AddressOf => todo!(),
                    Level2Op::SizeOf => todo!(),
                    Level2Op::Align => todo!(),
                }
            }
            Level2Exp::ParenExp(exp) => self.gen_l15_asm(*exp),
        }
    }
}

impl ArmGenerator {
    fn write_jmp_label(&mut self, lbl: usize) {
        self.write_to_buffer(format!(".L{lbl}:"));
    }

    fn write_branch_inst(&mut self, cond: Cond, lbl: usize) {
        self.write_inst(&format!("b{cond} .L{lbl}"));
    }

    fn push_stack(&mut self) {
        self.sp += 4;
        self.save_to_stack(self.sp);
    }

    fn pop_stack_into_backup(&mut self) {
        self.write_inst(&format!(
            "ldr  {}, [sp, {}]",
            Self::BACKUP_REGISTER,
            self.sp
        ));
        self.sp -= 4;
    }

    fn logical_comparison(&mut self, reg_a: &str, reg_b: &str, cond: Cond) {
        // Compare registers
        self.write_inst(&format!("cmp  {reg_a}, {reg_b}"));
        // set lower byte of reg_a based on if cond is satisfied
        self.write_inst(&format!("cset w0, {cond}"));
        // zero-pad reg_a since cset only sets the lower byte
        self.write_inst("uxtb w0, w0");
    }

    fn gen_stmt_asm(&mut self, stmt: Statement) {
        match stmt {
            Statement::Return(exp_opt) => match exp_opt {
                Some(exp) => self.gen_l15_asm(exp),
                // C standard states that int fns without a return value return 0 by default
                None => self.mov_into_primary("wzr"),
            },
            Statement::Exp(exp) => self.gen_l15_asm(exp),
            Statement::If(exp, predicate, else_opt) => {
                let else_label = self.curr_jmp_label;
                let exit_label = self.curr_jmp_label + 1;
                self.curr_jmp_label += 2;

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
}
