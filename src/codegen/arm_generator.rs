use super::{
    asm_generator::{AsmGenerator, INT_SIZE},
    codegen_enums::{Arch, Cond},
};

use crate::{
    parsing::parser_types::Function,
    utils::{context::Context, scoped_map::ScopedMap},
};

pub struct ArmGenerator {
    sp: usize,
    curr_jmp_label: usize,
    scoped_map: ScopedMap,
    arch: Arch,
    break_stack: Vec<usize>,
    continue_stack: Vec<usize>,
    fc: Vec<Context>,
}

impl Default for ArmGenerator {
    fn default() -> Self {
        Self {
            sp: 0,
            curr_jmp_label: 0,
            scoped_map: ScopedMap::default(),
            arch: Arch::ARM,
            break_stack: vec![],
            continue_stack: vec![],
            fc: vec![Context::default()],
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

    fn get_arch(&self) -> Arch {
        self.arch
    }

    fn fn_prologue(&mut self) {
        let ctx = self.curr_function_context_mut();

        ctx.prologue.push(format!(".global _{}", ctx.function_name));
        ctx.prologue.push(".align 2".to_string());
        ctx.prologue.push(format!("_{}:", ctx.function_name));

        // ctx.prologue.push(format!(
        //     "stp   x29, x30, [sp, -{}]!",
        //     ctx.get_stack_frame_size()
        // ));
        // ctx.prologue.push("mov   x29, sp".to_string());
    }

    fn fn_epilogue(&mut self) {
        // let stack_offset = self.fc.as_ref().unwrap().get_stack_frame_size();
        // self.write_inst(&format!("ldp   x29, x30, [sp], {stack_offset}"));
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
        self.sp += INT_SIZE;
    }

    fn decrement_stack_ptr(&mut self) {
        self.sp -= INT_SIZE;
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
        self.write_inst("uxtb  w0, w0");
    }

    fn logical_not(&mut self) {
        self.cmp_primary_with_zero();
        self.write_inst("cset  w0, eq");
        self.write_inst("uxtb  w0, w0");
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
        self.write_inst("sdiv  w2, w1, w0");
        self.write_inst("mul   w0, w2, w0");
        self.write_inst("sub   w0, w1, w0")
    }

    fn new_function_context(&mut self, function: &Function) {
        self.fc.push(Context::from(function));
    }

    fn curr_function_context(&self) -> &Context {
        self.fc.last().expect("Will always have a global context.")
    }

    fn curr_function_context_mut(&mut self) -> &mut Context {
        self.fc
            .last_mut()
            .expect("Will always have a global context.")
    }
}
