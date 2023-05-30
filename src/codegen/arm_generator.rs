use super::{
    asm_generator::{AsmGenerator, INT_SIZE},
    codegen_enums::{Arch, Cond},
    context::Context,
    function_map::FunctionMap,
};

use crate::{
    parsing::parser_types::{Function, Program},
    utils::{
        error::RustCcError,
        scoped_map::{ScopedMap, VarLoc},
    },
};

pub struct ArmGenerator {
    sp: usize,
    curr_jmp_label: usize,
    scoped_map: ScopedMap,
    arch: Arch,
    break_stack: Vec<usize>,
    continue_stack: Vec<usize>,
    fc: Vec<Context>,
    fn_map: FunctionMap,
}

impl TryFrom<&Program> for ArmGenerator {
    type Error = RustCcError;

    fn try_from(value: &Program) -> Result<Self, Self::Error> {
        let fn_map = FunctionMap::try_from(value)?;

        Ok(Self {
            sp: 0,
            curr_jmp_label: 0,
            scoped_map: ScopedMap::default(),
            arch: Arch::ARM,
            break_stack: vec![],
            continue_stack: vec![],
            fc: vec![Context::default()],
            fn_map,
        })
    }
}

impl AsmGenerator for ArmGenerator {
    const PRIMARY_REGISTER: &'static str = "w0";
    const BACKUP_REGISTER: &'static str = "w1";
    const DEFAULT_ARGS: &'static str = "w0, w1, w0";
    const UNARY_ARGS: &'static str = "w0, w0";

    fn get_fn_map(&self) -> &FunctionMap {
        &self.fn_map
    }

    fn get_scoped_map(&self) -> &ScopedMap {
        &self.scoped_map
    }

    fn get_scoped_map_mut(&mut self) -> &mut ScopedMap {
        &mut self.scoped_map
    }

    fn get_arch(&self) -> Arch {
        self.arch
    }

    fn fn_epilogue(&mut self) {
        self.write_inst(&format!("ldp   x29, x30, [sp], 16"));
        let stack_offset = self.curr_function_context().get_stack_frame_size();
        self.write_inst(&format!("add   sp, sp, {stack_offset}"));
    }

    fn ret(&mut self) {
        self.fn_epilogue();
        self.write_inst("ret");
    }

    fn save_to_stack(&mut self, stack_offset: usize) {
        // println!("store to stack {stack_offset}");
        self.write_address_inst(&format!("str   w0"), VarLoc::CurrFrame(stack_offset));
    }

    // TODO: change name to load_var()
    fn load_var(&mut self, reg_to_load_into: &str, loc: VarLoc) {
        // println!("load from stack {reg_to_load_into} <- {loc:?}");
        match loc {
            VarLoc::CurrFrame(_) | VarLoc::PrevFrame(_) => {
                self.write_address_inst(&format!("ldr   {reg_to_load_into}"), loc)
            }
            VarLoc::Register(reg_to_load_from) => {
                self.write_inst(&format!("mov   {reg_to_load_into}, w{reg_to_load_from}"))
            }
            VarLoc::Global(id) => {
                self.write_inst(&format!("adrp  x8, _{id}@PAGE"));
                self.write_inst(&format!("ldr   w8, [x8, _{id}@PAGEOFF]"));
                self.write_inst("mov   w0, w8");
            }
        }
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
            "cmp   {}, {}",
            Self::BACKUP_REGISTER,
            Self::PRIMARY_REGISTER
        ));
        // set lower byte of reg_a based on if cond is satisfied
        self.write_inst(&format!("cset  w0, {}", cond.for_arch(self.get_arch())));
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

    fn function_contexts_mut(&mut self) -> &mut [Context] {
        &mut self.fc
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
