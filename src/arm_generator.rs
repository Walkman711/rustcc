use std::collections::HashMap;

use crate::{
    codegen::AsmGenerator,
    codegen_enums::{Arch, Cond},
    parser_types::*,
};

pub struct ArmGenerator {
    sp: usize,
    curr_jmp_label: usize,
    var_map: HashMap<String, usize>,
    buffer: Vec<String>,
    arch: Arch,
}

impl Default for ArmGenerator {
    fn default() -> Self {
        Self {
            sp: 0,
            curr_jmp_label: 0,
            var_map: HashMap::new(),
            buffer: vec![],
            arch: Arch::ARM,
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

    fn get_buffer(&self) -> &[String] {
        &self.buffer
    }

    fn get_arch(&self) -> Arch {
        self.arch
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

    fn get_next_jmp_label(&mut self) -> usize {
        self.curr_jmp_label += 1;
        self.curr_jmp_label
    }

    fn write_jmp_label(&mut self, lbl: usize) {
        self.write_to_buffer(format!(".L{lbl}:"));
    }

    fn write_branch_inst(&mut self, cond: Cond, lbl: usize) {
        self.write_inst(&format!("b{cond} .L{lbl}"));
    }

    fn cmp_primary_with_zero(&mut self) {
        self.write_inst(&format!("cmp {}, wzr", Self::PRIMARY_REGISTER));
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
