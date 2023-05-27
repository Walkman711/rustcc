#![allow(non_camel_case_types)]

use crate::utils::ScopedMap;

use super::{
    codegen::AsmGenerator,
    codegen_enums::{Arch, Cond},
};

pub struct x86Generator {
    sp: usize,
    curr_jmp_label: usize,
    buffer: Vec<String>,
    arch: Arch,
    scoped_map: ScopedMap,
    break_stack: Vec<usize>,
    continue_stack: Vec<usize>,
}

impl Default for x86Generator {
    fn default() -> Self {
        Self {
            sp: 0,
            curr_jmp_label: 0,
            buffer: vec![],
            arch: Arch::x86,
            scoped_map: ScopedMap::default(),
            break_stack: vec![],
            continue_stack: vec![],
        }
    }
}

impl AsmGenerator for x86Generator {
    const PRIMARY_REGISTER: &'static str = "eax";
    const BACKUP_REGISTER: &'static str = "edx";
    const DEFAULT_ARGS: &'static str = "eax, edx";
    const UNARY_ARGS: &'static str = "eax";

    fn get_scoped_map(&self) -> &ScopedMap {
        &self.scoped_map
    }

    fn get_scoped_map_mut(&mut self) -> &mut ScopedMap {
        &mut self.scoped_map
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
        self.write_to_buffer(format!("{identifier}:"));
    }

    fn fn_prologue(&mut self, identifier: &str, num_params: usize) {
        todo!()
        // self.write_fn_header(identifier);
        // self.write_inst("push  rbp");
        // self.write_inst("mov   rbp, rsp");
        // self.write_inst("sub   rsp, 16");
    }

    fn fn_epilogue(&mut self) {
        self.write_inst("pop   rbp");
        // self.write_inst(&format!("str  w0, [sp, {}]", self.sp));
    }

    fn ret(&mut self) {
        self.fn_epilogue();
        self.write_inst("ret");
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

    fn save_to_stack(&mut self, stack_offset: usize) {
        self.write_inst(&format!(
            "mov   DWORD PTR [rbp - {stack_offset}], {}",
            Self::PRIMARY_REGISTER
        ));
    }

    fn load_from_stack(&mut self, reg: &str, stack_offset: usize) {
        self.write_inst(&format!("mov   {reg}, DWORD PTR [rbp - {stack_offset}]"));
    }

    fn logical_comparison(&mut self, cond: Cond) {
        // Compare registers
        self.write_inst(&format!(
            "cmp  {}, {}",
            Self::BACKUP_REGISTER,
            Self::PRIMARY_REGISTER
        ));
        // set lower byte of reg_a based on if cond is satisfied
        self.write_inst(&format!("set{} al", cond.for_arch(self.get_arch())));
        // zero-pad reg_a since cset only sets the lower byte
        self.write_inst("movzx eax, al");
    }

    fn logical_not(&mut self) {
        self.cmp_primary_with_zero();
        self.write_inst("sete  al");
        self.write_inst("movzx eax, al");
    }

    fn get_next_jmp_label(&mut self) -> usize {
        self.curr_jmp_label += 1;
        self.curr_jmp_label
    }

    fn write_branch_inst(&mut self, _cond: Cond, _lbl: usize) {
        self.write_inst("BRANCH INST")
        // self.write_inst(&format!("b{cond} .L{lbl}"));
    }

    fn get_break_stack(&self) -> &Vec<usize> {
        &self.break_stack
    }

    fn get_break_stack_mut(&mut self) -> &mut Vec<usize> {
        &mut self.break_stack
    }

    fn get_continue_stack(&self) -> &Vec<usize> {
        &self.continue_stack
    }

    fn get_continue_stack_mut(&mut self) -> &mut Vec<usize> {
        &mut self.continue_stack
    }

    fn gen_remainder_inst(&mut self) {
        todo!()
    }
}
