use super::{
    asm_generator::AsmGenerator,
    codegen_enums::{Arch, Cond},
    context::GlobalContext,
    function_map::FunctionMap,
};

use crate::{
    parsing::parser_types::{Level15Exp, Param, Program},
    utils::{
        error::{RustCcError, RustCcResult},
        scoped_map::VarLoc,
    },
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
            global_context: GlobalContext::new(Arch::RISCV),
        })
    }
}

impl AsmGenerator for RiscVGenerator {
    // TODO: this is what godbolt is doing for 64-bit risc-v but 32 is different
    const PRIMARY_REGISTER: &'static str = "a5";
    const BACKUP_REGISTER: &'static str = "a6";
    const RETURN_REGISTER: &'static str = "ra";

    // XXX: incorrect
    const GLOBAL_VAR_REGISTER: &'static str = "x30";
    const DEFAULT_ARGS: &'static str = "a5, a5, a6";
    const UNARY_ARGS: &'static str = "a5, a5";
    const INT_SIZE: usize = 8;

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
        self.sp += Self::INT_SIZE;
    }

    fn decrement_stack_ptr(&mut self) {
        self.sp -= Self::INT_SIZE;
    }

    fn save_to_stack(&mut self, stack_offset: usize) {
        self.write_address_inst(
            &format!("sw    {}", Self::PRIMARY_REGISTER),
            VarLoc::CurrFrame(stack_offset),
        );
    }

    fn load_var(&mut self, dst_reg: &str, loc: VarLoc) {
        match loc {
            VarLoc::CurrFrame(_) | VarLoc::PrevFrame(_) => {
                self.write_address_inst(&format!("sw    {dst_reg}"), loc);
            }
            VarLoc::Register(reg_to_load_from) => {
                self.write_inst(&format!("mv   {dst_reg}, {reg_to_load_from}"))
            }
            VarLoc::Global(_, _) => todo!("global vars for riscv"),
        }
    }

    fn compare_primary_with_backup(&mut self, _cond: Cond) {
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
        self.write_inst(&format!("mv    {}, {val}", Self::PRIMARY_REGISTER));
    }

    fn gen_remainder_inst(&mut self) {
        todo!()
    }

    fn write_fn_call(&mut self, _fn_name: &str, _args: Vec<Level15Exp>) -> RustCcResult<()> {
        todo!()
    }

    fn move_args_onto_stack(&mut self, _params: &[Param]) -> RustCcResult<()> {
        todo!()
    }
}
