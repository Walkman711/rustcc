use super::{
    asm_generator::{AsmGenerator, INT_SIZE},
    codegen_enums::{Arch, Cond},
    context::GlobalContext,
    function_map::FunctionMap,
};

use crate::{
    parsing::parser_types::Program,
    utils::{error::RustCcError, scoped_map::VarLoc},
};

pub struct RiscVGenerator {
    sp: usize,
    fn_map: FunctionMap,
    global_context: GlobalContext,
}

impl TryFrom<&Program> for RiscVGenerator {
    type Error = RustCcError;

    fn try_from(value: &Program) -> Result<Self, Self::Error> {
        let fn_map = FunctionMap::try_from(value)?;

        Ok(Self {
            sp: 0,
            fn_map,
            global_context: GlobalContext::default(),
        })
    }
}

impl AsmGenerator for RiscVGenerator {
    const PRIMARY_REGISTER: &'static str = "x28";

    const BACKUP_REGISTER: &'static str = "x29";

    const RETURN_REGISTER: &'static str = "ra";

    // XXX: incorrect
    const GLOBAL_VAR_REGISTER: &'static str = "x30";

    const DEFAULT_ARGS: &'static str = "x28, x28, x29";

    const UNARY_ARGS: &'static str = "x28, x28";

    fn get_fn_map(&self) -> &FunctionMap {
        &self.fn_map
    }

    fn get_arch(&self) -> Arch {
        Arch::RISCV
    }

    fn global_context(&self) -> &GlobalContext {
        &self.global_context
    }

    fn global_context_mut(&mut self) -> &mut GlobalContext {
        &mut self.global_context
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

    fn save_to_stack(&mut self, stack_offset: usize) {
        self.write_address_inst(
            &format!("sw {}, ", Self::PRIMARY_REGISTER),
            VarLoc::CurrFrame(stack_offset),
        );
    }

    fn load_var(&mut self, reg: &str, loc: VarLoc) {
        todo!()
    }

    fn compare_primary_with_backup(&mut self, cond: Cond) {
        todo!()
    }

    fn logical_not(&mut self) {
        self.cmp_primary_with_zero();
    }

    fn write_branch_inst(&mut self, cond: Cond, lbl: usize) {
        self.write_inst(&format!(
            "b{} .L{lbl}_{}",
            cond.for_arch(self.get_arch()),
            self.curr_ctx().function_name
        ));
    }

    fn cmp_primary_with_zero(&mut self) {
        todo!()
    }

    fn mov_into_primary(&mut self, val: &str) {
        todo!()
    }

    fn gen_remainder_inst(&mut self) {
        todo!()
    }

    fn fn_epilogue(&mut self) {
        todo!()
    }
}
