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

pub struct ArmGenerator {
    sp: usize,
    fn_map: FunctionMap,
    global_context: GlobalContext,
}

impl TryFrom<&Program> for ArmGenerator {
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

impl AsmGenerator for ArmGenerator {
    const PRIMARY_REGISTER: &'static str = "w0";
    const BACKUP_REGISTER: &'static str = "w1";
    const GLOBAL_VAR_REGISTER: &'static str = "x9";
    const DEFAULT_ARGS: &'static str = "w0, w1, w0";

    const UNARY_ARGS: &'static str = "w0, w0";

    fn get_fn_map(&self) -> &FunctionMap {
        &self.fn_map
    }

    fn get_arch(&self) -> Arch {
        Arch::ARM
    }

    fn fn_epilogue(&mut self) {
        self.write_inst("ldp   x29, x30, [sp], 16");
        let stack_offset = self.curr_function_context().get_stack_frame_size();
        self.write_inst(&format!("add   sp, sp, {stack_offset}"));
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
            &format!("str   {}", Self::PRIMARY_REGISTER),
            VarLoc::CurrFrame(stack_offset),
        );
    }

    fn load_var(&mut self, reg_to_load_into: &str, loc: VarLoc) {
        match loc {
            VarLoc::CurrFrame(_) | VarLoc::PrevFrame(_) => {
                self.write_address_inst(&format!("ldr   {reg_to_load_into}"), loc)
            }
            VarLoc::Register(reg_to_load_from) => {
                self.write_inst(&format!("mov   {reg_to_load_into}, w{reg_to_load_from}"))
            }
            VarLoc::Global(id, offset) => {
                let page = self.curr_function_context().get_page_access();
                self.write_inst(&format!(
                    "adrp  {}, _{id}@{page}",
                    Self::GLOBAL_VAR_REGISTER
                ));
                self.write_inst(&format!(
                    "ldr   {}, [{}, _{id}@{page}OFF]",
                    Self::GLOBAL_VAR_REGISTER,
                    Self::GLOBAL_VAR_REGISTER
                ));
                self.write_address_inst(
                    &format!("str   {}", Self::GLOBAL_VAR_REGISTER),
                    VarLoc::CurrFrame(offset),
                );
                self.load_var(Self::PRIMARY_REGISTER, VarLoc::CurrFrame(offset));
            }
        }
    }

    fn logical_comparison(&mut self, cond: Cond) {
        // Compare registers
        self.write_inst(&format!(
            "cmp   {}, {}",
            Self::BACKUP_REGISTER,
            Self::PRIMARY_REGISTER
        ));
        // Set lower byte of reg_a based on if cond is satisfied
        self.write_inst(&format!(
            "cset  {}, {}",
            Self::PRIMARY_REGISTER,
            cond.for_arch(self.get_arch())
        ));
        // Zero-pad reg_a since cset only sets the lower byte
        self.write_inst(&format!("uxtb  {}", Self::UNARY_ARGS));
    }

    fn logical_not(&mut self) {
        self.cmp_primary_with_zero();
        self.write_inst(&format!("cset  {}, eq", Self::PRIMARY_REGISTER));
        self.write_inst(&format!("uxtb  {}", Self::UNARY_ARGS,));
    }

    fn write_branch_inst(&mut self, cond: Cond, lbl: usize) {
        self.write_inst(&format!(
            "b{} .L{lbl}_{}",
            cond.for_arch(self.get_arch()),
            self.curr_function_context().function_name
        ));
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

    fn global_context(&self) -> &GlobalContext {
        &self.global_context
    }

    fn global_context_mut(&mut self) -> &mut GlobalContext {
        &mut self.global_context
    }
}
