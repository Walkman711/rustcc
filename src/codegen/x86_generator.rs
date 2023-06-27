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
            global_context: GlobalContext::default(),
        })
    }
}

impl AsmGenerator for x86Generator {
    const PRIMARY_REGISTER: &'static str;

    const BACKUP_REGISTER: &'static str;

    const GLOBAL_VAR_REGISTER: &'static str;

    const DEFAULT_ARGS: &'static str;

    const UNARY_ARGS: &'static str;

    fn get_fn_map(&self) -> &FunctionMap {
        todo!()
    }

    fn get_arch(&self) -> Arch {
        Arch::x86
    }

    fn fn_epilogue(&mut self) {
        self.write_inst("pop bsp");
    }

    fn stack_ptr(&self) -> usize {
        self.sp
    }

    fn increment_stack_ptr(&mut self) {
        self.sp += INT_SIZE;
    }

    fn decrement_stack_ptr(&mut self) {
        self.sp += INT_SIZE;
    }

    fn save_to_stack(&mut self, stack_offset: usize) {
        todo!()
    }

    fn load_var(&mut self, reg: &str, loc: VarLoc) {
        match loc {
            VarLoc::CurrFrame(_) => todo!(),
            VarLoc::PrevFrame(_) => todo!(),
            VarLoc::Register(_) => todo!(),
            VarLoc::Global(_, _) => todo!(),
        }
    }

    fn logical_comparison(&mut self, cond: Cond) {
        self.cmp_primary_with_zero();
    }

    fn logical_not(&mut self) {
        todo!()
    }

    fn write_branch_inst(&mut self, cond: Cond, lbl: usize) {
        todo!()
    }

    fn gen_remainder_inst(&mut self) {
        todo!()
    }

    fn global_context(&self) -> &GlobalContext {
        &self.global_context
    }

    fn global_context_mut(&mut self) -> &mut GlobalContext {
        &mut self.global_context
    }
}
