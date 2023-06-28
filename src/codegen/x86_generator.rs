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
    const PRIMARY_REGISTER: &'static str = "%eax";

    const BACKUP_REGISTER: &'static str = "%edx";

    const RETURN_REGISTER: &'static str = "%eax";

    const GLOBAL_VAR_REGISTER: &'static str = "GLOBAL_VAR_TODO";

    const DEFAULT_ARGS: &'static str = "%eax, %edx";

    const UNARY_ARGS: &'static str = "%eax";

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
        self.sp -= Self::INT_SIZE;
    }

    fn save_to_stack(&mut self, stack_offset: usize) {
        self.write_inst(&format!(
            "movl {}, -{stack_offset}(%rbp)",
            Self::PRIMARY_REGISTER,
        ))
        // self.write_address_inst(
        //     &format!("movq DWORD PTR [rbp-x], {}", Self::PRIMARY_REGISTER),
        //     VarLoc::CurrFrame(stack_offset),
        // )
    }

    fn load_var(&mut self, dst_reg: &str, loc: VarLoc) {
        match loc {
            VarLoc::CurrFrame(stack_offset) | VarLoc::PrevFrame(stack_offset) => {
                // self.write_address_inst(&format!("movq   {dst_reg}, DWORD PTR [rbp-x]"), loc);
                self.write_inst(&format!("movl -{stack_offset}(%rbp), {dst_reg}"));
            }
            VarLoc::Register(reg_to_load_from) => {
                self.write_inst(&format!("movl   {dst_reg}, {reg_to_load_from}"));
            }
            VarLoc::Global(_, _) => todo!("x86 globals"),
        }
    }

    fn compare_primary_with_backup(&mut self, cond: Cond) {
        self.write_inst(&format!(
            "cmp  {}, {}",
            // Self::BACKUP_REGISTER,
            Self::PRIMARY_REGISTER,
            Self::BACKUP_REGISTER,
        ));
        self.write_inst(&format!("set{} %al", cond.for_arch(self.get_arch())));
        self.write_inst(&format!("movzx %al, {}", Self::PRIMARY_REGISTER));
    }

    fn logical_not(&mut self) {
        self.write_inst(&format!(
            "testl  {}, {}",
            Self::PRIMARY_REGISTER,
            Self::PRIMARY_REGISTER
        ));
        self.write_inst("sete  %al");
        self.write_inst(&format!("movzx %al, {}", Self::PRIMARY_REGISTER));
    }

    fn write_branch_inst(&mut self, cond: Cond, lbl: usize) {
        self.write_inst(&format!(
            "j{} .L{lbl}_{}",
            cond.for_arch(self.get_arch()),
            self.curr_ctx().function_name
        ));
    }

    fn cmp_primary_with_zero(&mut self) {
        // self.write_inst(&format!("cmp   {}, 0", Self::PRIMARY_REGISTER));
        self.write_inst(&format!(
            "test  {}, {}",
            Self::PRIMARY_REGISTER,
            Self::PRIMARY_REGISTER
        ));
    }

    fn mov_into_primary(&mut self, val: &str) {
        let (mov_inst, dollar_sign) = if val.contains('%') {
            ("movq", "")
        } else {
            ("movl", "$")
        };
        self.write_inst(&format!(
            "{mov_inst}  {dollar_sign}{val}, {}",
            Self::PRIMARY_REGISTER
        ));
    }

    fn gen_remainder_inst(&mut self) {
        todo!()
    }

    fn gen_div_inst(&mut self) {
        // TODO: comment why this is the simplest option for now
        self.write_inst("movl %eax, %ecx");
        self.write_inst("movl %edx, %eax");
        self.write_inst("cdq");
        self.write_inst("idiv %ecx");
    }
}
