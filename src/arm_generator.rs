use crate::{
    codegen::AsmGenerator,
    codegen_enums::{Arch, Cond},
    utils::ScopedMap,
};

pub struct ArmGenerator {
    sp: usize,
    curr_jmp_label: usize,
    scoped_map: ScopedMap,
    buffer: Vec<String>,
    arch: Arch,
    break_stack: Vec<usize>,
    continue_stack: Vec<usize>,
}

impl Default for ArmGenerator {
    fn default() -> Self {
        Self {
            sp: 0,
            curr_jmp_label: 0,
            scoped_map: ScopedMap::default(),
            buffer: vec![],
            arch: Arch::ARM,
            break_stack: vec![],
            continue_stack: vec![],
        }
    }
}

impl AsmGenerator for ArmGenerator {
    const PRIMARY_REGISTER: &'static str = "w0";
    const BACKUP_REGISTER: &'static str = "w1";
    const DEFAULT_ARGS: &'static str = "w0, w1, w0";
    const UNARY_ARGS: &'static str = "w0, w0";

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

    fn save_to_stack(&mut self, stack_offset: usize) {
        self.write_inst(&format!("str   w0, [sp, {stack_offset}]"));
    }

    fn load_from_stack(&mut self, reg: &str, stack_offset: usize) {
        self.write_inst(&format!("ldr   {reg}, [sp, {stack_offset}]"));
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
        // 3 % 2
        // w0 3
        // push
        // w0 2
        // w1 3
        // w2 1
        self.write_inst("sdiv w2, w1, w0");
        self.write_inst("mul w0, w2, w0");
        self.write_inst("sub w0, w1, w0")
    }
}