use std::collections::HashMap;

use crate::{
    codegen::AsmGenerator,
    codegen_enums::{Arch, Cond},
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
    const UNARY_ARGS: &'static str = "w0, w0";

    fn get_variable(&self, var: &str) -> Option<&usize> {
        self.var_map.get(var)
    }

    fn save_variable(&mut self, var: &str) {
        self.var_map.insert(var.to_owned(), self.stack_ptr() + 4);
    }

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

    fn load_from_stack(&mut self, reg: &str, stack_offset: usize) {
        self.write_inst(&format!("ldr  {reg}, [sp, {stack_offset}]"));
    }

    fn stack_ptr(&self) -> usize {
        self.sp
    }

    fn increment_stack_ptr(&mut self) {
        self.sp += 4;
    }

    fn decrement_stack_ptr(&mut self) {
        self.sp -= 4;
    }

    fn logical_comparison(&mut self, cond: Cond) {
        // Compare registers
        self.write_inst(&format!(
            "cmp  {}, {}",
            Self::BACKUP_REGISTER,
            Self::PRIMARY_REGISTER
        ));
        // set lower byte of reg_a based on if cond is satisfied
        self.write_inst(&format!("cset w0, {}", cond.for_arch(self.get_arch())));
        // zero-pad reg_a since cset only sets the lower byte
        self.write_inst("uxtb w0, w0");
    }

    fn logical_not(&mut self) {
        self.cmp_primary_with_zero();
        self.write_inst("cset w0, eq");
        self.write_inst("uxtb w0, w0");
    }

    fn get_next_jmp_label(&mut self) -> usize {
        self.curr_jmp_label += 1;
        self.curr_jmp_label
    }

    fn write_branch_inst(&mut self, cond: Cond, lbl: usize) {
        self.write_inst(&format!("b{} .L{lbl}", cond.for_arch(self.get_arch())));
    }
}
