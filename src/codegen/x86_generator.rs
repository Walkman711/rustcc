use super::{
    asm_generator::AsmGenerator,
    codegen_enums::{Arch, Cond},
    context::GlobalContext,
    function_map::FunctionMap,
};

use crate::{
    parsing::parser_types::Program,
    utils::{error::RustCcError, scoped_map::VarLoc},
};

#[allow(non_camel_case_types)]
pub struct x86Generator {
    sp: usize,
    fn_map: FunctionMap,
    global_context: GlobalContext,
}

impl TryFrom<&Program> for x86Generator {
    type Error = RustCcError;

    fn try_from(value: &Program) -> Result<Self, Self::Error> {
        let fn_map = FunctionMap::try_from(value)?;

        Ok(Self {
            sp: 0,
            fn_map,
            global_context: GlobalContext::new(Arch::x86),
        })
    }
}

impl AsmGenerator for x86Generator {
    const PRIMARY_REGISTER: &'static str = "eax";

    const BACKUP_REGISTER: &'static str = "edx";

    const RETURN_REGISTER: &'static str = "eax";

    const GLOBAL_VAR_REGISTER: &'static str = "GLOBAL_VAR_TODO";

    const DEFAULT_ARGS: &'static str = "eax, edx";

    const UNARY_ARGS: &'static str = "eax";

    const INT_SIZE: usize = 8;

    fn get_fn_map(&self) -> &FunctionMap {
        &self.fn_map
    }

    fn get_arch(&self) -> Arch {
        Arch::x86
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
        self.sp += Self::INT_SIZE;
    }

    fn save_to_stack(&mut self, stack_offset: usize) {
        self.write_address_inst(
            &format!("mov   DWORD PTR [rbp-x], {}", Self::PRIMARY_REGISTER),
            VarLoc::CurrFrame(stack_offset),
        )
    }

    fn load_var(&mut self, dst_reg: &str, loc: VarLoc) {
        match loc {
            VarLoc::CurrFrame(_) | VarLoc::PrevFrame(_) => {
                self.write_address_inst(&format!("mov   {dst_reg}, DWORD PTR [rbp-x]"), loc);
            }
            VarLoc::Register(reg_to_load_from) => {
                self.write_inst(&format!("mov   {dst_reg}, {reg_to_load_from}"));
            }
            VarLoc::Global(_, _) => todo!("x86 globals"),
        }
    }

    fn compare_primary_with_backup(&mut self, cond: Cond) {
        todo!()
    }

    fn logical_not(&mut self) {
        todo!()
    }

    fn write_branch_inst(&mut self, cond: Cond, lbl: usize) {
        todo!()
    }

    fn cmp_primary_with_zero(&mut self) {
        todo!()
    }

    fn mov_into_primary(&mut self, val: &str) {
        self.write_inst(&format!("mov   {}, {val}", Self::PRIMARY_REGISTER));
    }

    fn gen_remainder_inst(&mut self) {
        todo!()
    }
}
