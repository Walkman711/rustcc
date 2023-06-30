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

pub struct ArmGenerator {
    sp: usize,
    fn_map: FunctionMap,
    global_context: GlobalContext,
}

impl ArmGenerator {
    const GLOBAL_VAR_REGISTER: &'static str = "x9";
}

impl TryFrom<&Program> for ArmGenerator {
    type Error = RustCcError;

    fn try_from(value: &Program) -> Result<Self, Self::Error> {
        let fn_map = FunctionMap::try_from(value)?;

        Ok(Self {
            sp: 0,
            fn_map,
            global_context: GlobalContext::new(Arch::ARM),
        })
    }
}

impl AsmGenerator for ArmGenerator {
    // TODO: make this and backup and default args temporary registers
    const PRIMARY_REGISTER: &'static str = "w0";
    const BACKUP_REGISTER: &'static str = "w1";
    const RETURN_REGISTER: &'static str = "w0";
    const DEFAULT_ARGS: &'static str = "w0, w1, w0";
    const UNARY_ARGS: &'static str = "w0, w0";

    const INT_SIZE: usize = 8;

    fn get_fn_map(&self) -> &FunctionMap {
        &self.fn_map
    }

    fn get_arch(&self) -> Arch {
        Arch::ARM
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
            &format!("str   {}", Self::PRIMARY_REGISTER),
            VarLoc::CurrFrame(stack_offset),
        );
    }

    fn assign_to_global(&mut self, id: &str, offset: usize) {
        let page = self.curr_ctx().get_page_access();
        // COMMENT:
        self.write_inst(&format!(
            "adrp  {}, _{id}@{page}",
            Self::GLOBAL_VAR_REGISTER
        ));
        self.write_inst(&format!("mov   w8, {}", Self::PRIMARY_REGISTER));
        self.write_inst(&format!("str   w8, [{}]", Self::GLOBAL_VAR_REGISTER));
        self.save_to_stack(offset);
    }

    fn load_var(&mut self, dst_reg: &str, loc: VarLoc) {
        match loc {
            VarLoc::CurrFrame(_) | VarLoc::PrevFrame(_) => {
                self.write_address_inst(&format!("ldr   {dst_reg}"), loc)
            }
            VarLoc::Register(reg_to_load_from) => {
                self.write_inst(&format!("mov   {dst_reg}, w{reg_to_load_from}"))
            }
            VarLoc::Global(id, offset) => {
                let page = self.curr_ctx().get_page_access();
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

    fn compare_primary_with_backup(&mut self, cond: Cond) {
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
            self.curr_ctx().function_name
        ));
    }

    fn write_fn_call(&mut self, fn_name: &str, args: Vec<Level15Exp>) -> RustCcResult<()> {
        for (reg, arg) in args.iter().enumerate() {
            // HACK: we can only pass 8 args, so just store the args to be passed in w0-w7
            // in w8-w15 until we generate all the expressions
            let tmp_reg = reg + 8;
            self.gen_l15_asm(arg.to_owned())?;
            self.write_inst(&format!("mov   w{tmp_reg}, {}", Self::PRIMARY_REGISTER));
        }

        for reg in 0..args.len() {
            let tmp_reg = reg + 8;
            self.write_inst(&format!("mov   w{reg}, w{tmp_reg}"));
        }

        // FIX: only for ARM
        self.write_inst(&format!("bl    _{fn_name}"));
        Ok(())
    }

    fn move_args_into_curr_frame(&mut self, params: &[Param]) -> RustCcResult<()> {
        let mut var_loc = self.stack_ptr() + Self::INT_SIZE;

        for (reg, param) in params.iter().enumerate() {
            // mov arg from register into primary
            // FIX: this is where the w0 is sneaking into x86 codegen
            self.mov_into_primary(&format!("w{reg}"));

            // Add param to the scope map
            let sm = self.get_scoped_map_mut();
            sm.new_param(&param.id, param.var_type, VarLoc::CurrFrame(var_loc))?;
            var_loc += Self::INT_SIZE;

            // save arg onto stack
            self.push_stack();
        }

        Ok(())
    }

    fn cmp_primary_with_zero(&mut self) {
        self.write_inst(&format!("cmp   {}, 0", Self::PRIMARY_REGISTER));
    }

    fn mov_into_primary(&mut self, val: &str) {
        self.write_inst(&format!("mov   {}, {val}", Self::PRIMARY_REGISTER));
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

    fn define_global(&mut self, id: &str, val: &i64) {
        let gc = self.global_context_mut();
        // set .p2align to 3 so that globals in the data section are 8-byte aligned
        gc.write_defined_global_inst(".p2align 3".to_string());
        gc.write_defined_global_inst(format!(".global _{id}"));
        gc.write_defined_global_inst(format!("_{id}:"));
        gc.write_defined_global_inst(format!("\t.long {val}"));
    }

    fn declare_global(&mut self, id: &str) {
        self.global_context_mut()
            .write_declared_global_inst(format!("\t.comm _{id},4,2"));
    }
}
