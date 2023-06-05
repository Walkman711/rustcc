use std::{cmp::max, fs::File};

use crate::{
    gen_level_asm,
    parsing::{ops::*, parser_types::*},
    utils::{
        error::{CodegenError, RustCcError, RustCcResult},
        scoped_map::{ScopedMap, VarLoc},
    },
};

use super::{
    codegen_enums::{Arch, Cond, Mnemonic},
    context::{Context, GlobalContext, Instruction},
    function_map::FunctionMap,
};

pub const INT_SIZE: usize = 8;

pub trait AsmGenerator {
    const PRIMARY_REGISTER: &'static str;
    const BACKUP_REGISTER: &'static str;
    const GLOBAL_VAR_REGISTER: &'static str;
    const DEFAULT_ARGS: &'static str;
    const UNARY_ARGS: &'static str;

    fn get_scoped_map(&self) -> &ScopedMap {
        &self.curr_function_context().scoped_map
    }

    fn get_scoped_map_mut(&mut self) -> &mut ScopedMap {
        &mut self.curr_function_context_mut().scoped_map
    }

    fn get_fn_map(&self) -> &FunctionMap;

    // HACK: trying to see if we can just replace an arbitrary string to set stack offsets
    fn write_to_buffer(&mut self, inst: Instruction) {
        self.curr_function_context_mut().insts.push(inst);
    }

    fn write_to_file(&mut self, asm_filename: &str) {
        let mut asm_file = File::create(asm_filename).expect("Failed to create output .s file.");
        let arch = self.get_arch();
        self.global_context_mut().write_to_file(&mut asm_file, arch);
    }

    fn write_inst(&mut self, inst: &str) {
        self.write_to_buffer(Instruction::NoOffset(inst.to_owned()));
    }

    fn write_address_inst(&mut self, inst: &str, loc: VarLoc) {
        self.write_to_buffer(Instruction::Address(inst.to_owned(), loc));
    }

    fn write_mnemonic(&mut self, mnemonic: Mnemonic) {
        self.write_inst(&format!(
            "{:5} {}",
            mnemonic.for_arch(self.get_arch()),
            Self::DEFAULT_ARGS
        ));
    }

    fn write_unary_inst(&mut self, mnemonic: Mnemonic) {
        self.write_inst(&format!(
            "{:5} {}",
            mnemonic.for_arch(self.get_arch()),
            Self::UNARY_ARGS
        ));
    }

    fn get_arch(&self) -> Arch;

    fn fn_epilogue(&mut self);
    fn ret(&mut self);

    fn stack_ptr(&self) -> usize;
    fn increment_stack_ptr(&mut self);
    fn decrement_stack_ptr(&mut self);
    fn save_to_stack(&mut self, stack_offset: usize);
    fn load_var(&mut self, reg: &str, loc: VarLoc);
    fn pop_stack_into_backup(&mut self) {
        self.load_var(Self::BACKUP_REGISTER, VarLoc::CurrFrame(self.stack_ptr()));
        self.decrement_stack_ptr();
    }
    fn push_stack(&mut self) {
        self.increment_stack_ptr();
        self.save_to_stack(self.stack_ptr());
        let sp = self.stack_ptr();
        let fc = self.curr_function_context_mut();
        fc.max_stack_offset = max(fc.max_stack_offset, sp);
    }

    fn logical_comparison(&mut self, cond: Cond);
    fn logical_not(&mut self);

    fn get_next_jmp_label(&mut self) -> usize {
        self.curr_function_context_mut().get_next_jmp_label()
    }

    fn write_branch_inst(&mut self, cond: Cond, lbl: usize);
    fn write_jmp_label(&mut self, lbl: usize) {
        self.write_to_buffer(Instruction::NoOffset(format!(".L{lbl}:")));
    }

    fn get_break_stack(&self) -> &Vec<usize> {
        &self.curr_function_context().break_stack
    }

    fn get_break_stack_mut(&mut self) -> &mut Vec<usize> {
        &mut self.curr_function_context_mut().break_stack
    }

    fn get_continue_stack(&self) -> &Vec<usize> {
        &self.curr_function_context().continue_stack
    }

    fn get_continue_stack_mut(&mut self) -> &mut Vec<usize> {
        &mut self.curr_function_context_mut().continue_stack
    }

    fn cmp_primary_with_zero(&mut self) {
        self.write_inst(&format!("cmp   {}, 0", Self::PRIMARY_REGISTER));
    }

    fn mov_into_primary(&mut self, val: &str) {
        self.write_inst(&format!("mov   {}, {val}", Self::PRIMARY_REGISTER));
    }

    fn gen_remainder_inst(&mut self);

    fn curr_function_context(&self) -> &Context {
        self.global_context().curr_function_context()
    }

    fn curr_function_context_mut(&mut self) -> &mut Context {
        self.global_context_mut().curr_function_context_mut()
    }
    fn global_context(&self) -> &GlobalContext;
    fn global_context_mut(&mut self) -> &mut GlobalContext;

    fn gen_asm(&mut self, asm_filename: &str, prog: Program) -> RustCcResult<()> {
        let mut offset = INT_SIZE;
        for top_level_item in &prog.0 {
            if let TopLevelItem::Var(GlobalVar::Definition(id, val)) = top_level_item {
                let gc = self.global_context_mut();
                gc.scoped_map
                    .initialize_var(id, VarLoc::Global(id.to_owned(), offset))?;
                offset += INT_SIZE;
                // TODO: pull this and .comm into fns
                gc.write_defined_global_inst(".section __DATA,__data".to_string());
                gc.write_defined_global_inst(".p2align 2".to_string());
                gc.write_defined_global_inst(format!(".global _{id}"));
                gc.write_defined_global_inst(format!("_{id}:"));
                gc.write_defined_global_inst(format!("\t.long {val}"));

                self.increment_stack_ptr();
            }
        }

        for top_level_item in &prog.0 {
            if let TopLevelItem::Var(GlobalVar::Declaration(id)) = top_level_item {
                let gc = self.global_context_mut();
                gc.scoped_map
                    .initialize_var(id, VarLoc::Global(id.to_owned(), offset))?;
                offset += INT_SIZE;
                gc.write_declared_global_inst(format!("\t.comm _{id},4,2"));

                self.increment_stack_ptr();
            }
        }

        for top_level_item in prog.0 {
            let TopLevelItem::Fun(ref function) = top_level_item else {
                continue;
            };

            match function {
                Function::Declaration(_, _) => continue,
                Function::Definition(_id, params, block_items) => {
                    self.global_context_mut().new_function_context(function);

                    self.get_scoped_map_mut().new_scope()?;

                    let mut var_loc = self.stack_ptr() + INT_SIZE;

                    for reg in 0..params.len() {
                        // mov arg from register into primary
                        self.mov_into_primary(&format!("w{reg}"));

                        // Add param to the scope map
                        let sm = self.get_scoped_map_mut();
                        sm.new_param(&params[reg], VarLoc::CurrFrame(var_loc))?;
                        var_loc += INT_SIZE;

                        // save arg onto stack
                        self.push_stack();
                    }

                    // PERF: don't clone this
                    self.gen_block_asm(block_items.to_owned())?;

                    // TODO: do i need to do anything with the size of the scope?
                    let (num_deallocated_vars, globals) = self.get_scoped_map_mut().exit_scope()?;
                    for _ in 0..num_deallocated_vars {
                        self.decrement_stack_ptr();
                    }
                }
            }
        }

        self.write_to_file(asm_filename);

        Ok(())
    }

    fn gen_var_decl_asm(&mut self, decl: Declaration) -> RustCcResult<()> {
        let (id, exp_opt) = decl;
        let var_loc = self.stack_ptr();
        let sm = self.get_scoped_map_mut();
        match exp_opt {
            Some(exp) => {
                sm.initialize_var(&id, VarLoc::CurrFrame(var_loc + INT_SIZE))?;
                self.gen_l15_asm(exp)?;
                self.push_stack();
            }
            None => {
                sm.declare_var(&id, VarLoc::CurrFrame(var_loc + INT_SIZE))?;
                self.mov_into_primary("999");
                self.push_stack();
            }
        }

        Ok(())
    }

    fn gen_block_asm(&mut self, block_items: Vec<BlockItem>) -> RustCcResult<()> {
        {
            self.get_scoped_map_mut().new_scope()?;
        }

        for block_item in block_items {
            match block_item {
                BlockItem::Stmt(s) => self.gen_stmt_asm(s)?,
                BlockItem::Declare(decl) => self.gen_var_decl_asm(decl)?,
            }
        }

        {
            let (variables_to_deallocate, globals_to_save) =
                self.get_scoped_map_mut().exit_scope()?;

            // DRY: make fn, also, where to store??
            for global_loc in globals_to_save {
                self.write_address_inst(
                    &format!("ldr   {}", Self::GLOBAL_VAR_REGISTER),
                    global_loc,
                );
                self.write_inst(&format!("str   w0, [{}]", Self::GLOBAL_VAR_REGISTER));
                // self.write_inst("str   w0, [x8]");
            }

            for _ in 0..variables_to_deallocate {
                self.decrement_stack_ptr();
            }
        }

        Ok(())
    }

    fn gen_stmt_asm(&mut self, stmt: Statement) -> RustCcResult<()> {
        match stmt {
            Statement::Return(exp_opt) => {
                match exp_opt {
                    Some(exp) => {
                        // TODO: this is ugly, should be some way to stop generating ASM
                        self.gen_l15_asm(exp)?;
                    }
                    // C standard states that int fns without a return value return 0 by default
                    None => {
                        self.mov_into_primary("0");
                    }
                }
                self.ret();
            }
            Statement::Exp(exp_opt) => {
                if let Some(exp) = exp_opt {
                    self.gen_l15_asm(exp)?;
                }
            }
            Statement::If(exp, predicate, else_opt) => {
                let else_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();

                // Evaluate exp and store in w0
                self.gen_l15_asm(exp)?;

                // If the exp == 0, trigger the else case to minimize the jump
                // instructions we need
                self.cmp_primary_with_zero();

                self.write_branch_inst(Cond::Equals, else_label);

                // Execute if the if-exp is nonzero
                self.gen_stmt_asm(*predicate)?;
                self.write_branch_inst(Cond::Always, exit_label);

                // If we don't have an else case, jumping to this label
                // will just cause us to fall through to the exit label
                self.write_jmp_label(else_label);

                if let Some(else_stmt) = else_opt {
                    self.gen_stmt_asm(*else_stmt)?;
                }
                self.write_jmp_label(exit_label);
            }
            Statement::Compound(block_items) => self.gen_block_asm(block_items)?,
            Statement::While(exp, stmt) => {
                let continue_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();

                self.get_continue_stack_mut().push(continue_label);
                self.get_break_stack_mut().push(exit_label);

                self.write_jmp_label(continue_label);
                // Evaluate exp and store in w0
                self.gen_l15_asm(exp)?;

                // If the exp == 0, trigger the else case to minimize the jump
                // instructions we need
                self.cmp_primary_with_zero();

                self.write_branch_inst(Cond::Equals, exit_label);

                // Execute if the if-exp is nonzero
                self.gen_stmt_asm(*stmt)?;
                self.write_branch_inst(Cond::Always, continue_label);

                self.write_jmp_label(exit_label);
            }
            Statement::DoWhile(stmt, exp) => {
                let continue_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();

                self.get_continue_stack_mut().push(continue_label);
                self.get_break_stack_mut().push(exit_label);

                // DO stmt
                self.write_jmp_label(continue_label);
                self.gen_stmt_asm(*stmt)?;

                // Evaluate exp and store in w0
                self.gen_l15_asm(exp)?;

                self.cmp_primary_with_zero();
                self.write_branch_inst(Cond::NotEquals, continue_label);
                self.write_jmp_label(exit_label);
            }
            Statement::For(initial_exp, controlling_exp, post_exp, body) => {
                let continue_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();

                self.get_continue_stack_mut().push(continue_label);
                self.get_break_stack_mut().push(exit_label);

                if let Some(exp) = initial_exp {
                    self.gen_l15_asm(exp)?;
                }

                self.write_jmp_label(continue_label);
                // Empty controlling exps evaluate to true
                match controlling_exp {
                    Some(exp) => self.gen_l15_asm(exp)?,
                    None => self.mov_into_primary("1"),
                };

                // Skip to end if primary reg == 0
                self.cmp_primary_with_zero();
                self.write_branch_inst(Cond::Equals, exit_label);

                // Exec stmt
                self.gen_stmt_asm(*body)?;

                // Eval post exp
                if let Some(exp) = post_exp {
                    self.gen_l15_asm(exp)?;
                }

                self.write_branch_inst(Cond::Always, continue_label);

                self.write_jmp_label(exit_label);
            }
            Statement::ForDecl(decl, controlling_exp, post_exp, body) => {
                let start_label = self.get_next_jmp_label();
                let continue_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();
                {
                    self.get_scoped_map_mut().new_scope()?;
                }

                self.get_continue_stack_mut().push(continue_label);
                self.get_break_stack_mut().push(exit_label);

                self.gen_var_decl_asm(decl)?;

                self.write_jmp_label(start_label);

                // Empty controlling exps evaluate to true
                match controlling_exp {
                    Some(exp) => self.gen_l15_asm(exp)?,
                    None => self.mov_into_primary("1"),
                };

                // Skip to end if primary reg == 0
                self.cmp_primary_with_zero();
                self.write_branch_inst(Cond::Equals, exit_label);

                // Exec stmt
                self.gen_stmt_asm(*body)?;

                self.write_jmp_label(continue_label);

                // Eval post exp
                if let Some(exp) = post_exp {
                    self.gen_l15_asm(exp)?;
                }

                self.write_branch_inst(Cond::Always, start_label);

                self.write_jmp_label(exit_label);
                {
                    let (variables_to_deallocate, globals_to_save) =
                        self.get_scoped_map_mut().exit_scope()?;

                    // for global_loc in globals_to_save {
                    //     self.write_address_inst("ldr   x8", global_loc);
                    //     self.write_inst("str   w0, [x8]");
                    //     // self.write_address_inst("ldr   w0", global_loc);
                    //     // self.write_inst("str   w0, [x8]");
                    // }

                    for _ in 0..variables_to_deallocate {
                        self.decrement_stack_ptr();
                    }
                }
            }
            Statement::Break => {
                if let Some(break_label) = self.get_break_stack().last() {
                    self.write_branch_inst(Cond::Always, *break_label);
                } else {
                    Err(RustCcError::CodegenError(CodegenError::UnenclosedBreak))?;
                }
            }
            Statement::Continue => {
                if let Some(continue_label) = self.get_continue_stack().last() {
                    self.write_branch_inst(Cond::Always, *continue_label);
                } else {
                    Err(RustCcError::CodegenError(CodegenError::UnenclosedContinue))?;
                }
            }
        }

        Ok(())
    }

    fn gen_l15_asm(&mut self, l15: Level15Exp) -> RustCcResult<()> {
        let (first_l14_exp, trailing_l14_exps) = l15.0;
        self.gen_l14_asm(first_l14_exp)?;

        for (op, l14_exp) in trailing_l14_exps {
            self.push_stack();
            self.gen_l14_asm(l14_exp)?;
            self.pop_stack_into_backup();
            match op {
                Level15Op::Comma => todo!("codegen ,"),
            }
        }

        Ok(())
    }

    fn gen_l14_asm(&mut self, l14: Level14Exp) -> RustCcResult<()> {
        match l14 {
            Level14Exp::SimpleAssignment(identifier, l15_exp) => {
                let sp = self.stack_ptr();
                let sm = self.get_scoped_map_mut();
                let var_loc = sm.assign_var(&identifier, sp)?;
                self.gen_l15_asm(*l15_exp)?;
                match var_loc {
                    VarLoc::CurrFrame(offset) => self.save_to_stack(offset),
                    VarLoc::PrevFrame(_) => {
                        panic!("tried to assign to 0 var in a prev stack frame")
                    }
                    VarLoc::Register(_) => panic!("tried to assign to a var in register"),
                    // TODO: make x8 a constant somewhere
                    // FIX: doesn't work with more than one global
                    // TODO:
                    // VarLoc::Global(_, None) => self.write_inst("mov   w0, [x8]"),
                    VarLoc::Global(id, offset) => {
                        let page = if self.curr_function_context().function_name == "main" {
                            "PAGE"
                        } else {
                            "GOTPAGE"
                        };
                        self.write_inst(&format!(
                            "adrp  {}, _{id}@{page}",
                            Self::GLOBAL_VAR_REGISTER
                        ));
                        self.write_inst(&format!(
                            "ldr   {}, [{}, _{id}@{page}OFF]",
                            Self::GLOBAL_VAR_REGISTER,
                            Self::GLOBAL_VAR_REGISTER
                        ));
                        self.write_address_inst(&format!("str   x9"), VarLoc::CurrFrame(offset));
                        self.write_inst("mov   w8, w0");
                        self.write_inst(&format!("str   w8, [{}]", Self::GLOBAL_VAR_REGISTER));
                        self.save_to_stack(offset);
                    }
                }
            }
            Level14Exp::NonAssignment(l13_exp) => {
                self.gen_l13_asm(l13_exp)?;
            }
        }

        Ok(())
    }

    fn gen_l13_asm(&mut self, l13: Level13Exp) -> RustCcResult<()> {
        match l13 {
            Level13Exp::Ternary(exp, pred_exp, else_exp) => {
                let else_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();

                self.gen_l12_asm(exp)?;

                // If the exp == 0, trigger the else case to minimize the jump
                // instructions we need
                self.cmp_primary_with_zero();

                self.write_branch_inst(Cond::Equals, else_label);

                // Execute if the if-exp is nonzero
                self.gen_l15_asm(*pred_exp)?;
                self.write_branch_inst(Cond::Always, exit_label);

                self.write_jmp_label(else_label);
                self.gen_l13_asm(*else_exp)?;

                self.write_jmp_label(exit_label);
            }
            Level13Exp::NoTernary(l12) => self.gen_l12_asm(l12)?,
        }

        Ok(())
    }

    fn gen_l12_asm(&mut self, l12: Level12Exp) -> RustCcResult<()> {
        let short_circuit_label = self.get_next_jmp_label();
        let exit_label = self.get_next_jmp_label();

        let (first_l11_exp, trailing_l11_exps) = l12.0;
        let print_jmp_insts = !trailing_l11_exps.is_empty();

        self.gen_l11_asm(first_l11_exp)?;

        for (_op, l11_exp) in trailing_l11_exps {
            self.cmp_primary_with_zero();
            self.write_branch_inst(Cond::NotEquals, short_circuit_label);

            self.gen_l11_asm(l11_exp)?;
        }

        if print_jmp_insts {
            self.cmp_primary_with_zero();
            self.write_branch_inst(Cond::NotEquals, short_circuit_label);

            self.mov_into_primary("0");
            self.write_branch_inst(Cond::Always, exit_label);

            self.write_jmp_label(short_circuit_label);
            self.mov_into_primary("1");

            self.write_jmp_label(exit_label);
        }

        Ok(())
    }

    fn gen_l11_asm(&mut self, l11: Level11Exp) -> RustCcResult<()> {
        let short_circuit_label = self.get_next_jmp_label();
        let success_label = self.get_next_jmp_label();

        let (first_l10_exp, trailing_l10_exps) = l11.0;
        let print_jmp_insts = !trailing_l10_exps.is_empty();

        self.gen_l10_asm(first_l10_exp)?;

        for (_op, l10_exp) in trailing_l10_exps {
            self.cmp_primary_with_zero();
            self.write_branch_inst(Cond::Equals, short_circuit_label);

            self.gen_l10_asm(l10_exp)?;
        }

        if print_jmp_insts {
            // Get the last term checked
            self.cmp_primary_with_zero();
            self.write_branch_inst(Cond::Equals, short_circuit_label);

            self.mov_into_primary("1");
            self.write_branch_inst(Cond::Always, success_label);

            self.write_jmp_label(short_circuit_label);
            self.mov_into_primary("0");

            self.write_jmp_label(success_label);
        }

        Ok(())
    }

    gen_level_asm!(gen_l10_asm, Level10Exp, gen_l9_asm, write_mnemonic);
    gen_level_asm!(gen_l9_asm, Level9Exp, gen_l8_asm, write_mnemonic);
    gen_level_asm!(gen_l8_asm, Level8Exp, gen_l7_asm, write_mnemonic);
    gen_level_asm!(gen_l7_asm, Level7Exp, gen_l6_asm, logical_comparison);
    gen_level_asm!(gen_l6_asm, Level6Exp, gen_l5_asm, logical_comparison);
    gen_level_asm!(gen_l5_asm, Level5Exp, gen_l4_asm, write_mnemonic);
    gen_level_asm!(gen_l4_asm, Level4Exp, gen_l3_asm, write_mnemonic);

    fn gen_l3_asm(&mut self, l3: Level3Exp) -> RustCcResult<()> {
        let (first_lower_level_exp, trailing_lower_level_exps) = l3.0;
        self.gen_l2_asm(first_lower_level_exp)?;

        for (op, lower_level_exp) in trailing_lower_level_exps {
            self.push_stack();
            self.gen_l2_asm(lower_level_exp)?;
            self.pop_stack_into_backup();
            match op {
                Level3Op::Multiplication => self.write_mnemonic(Mnemonic::Multiply),
                Level3Op::Division => self.write_mnemonic(Mnemonic::Divide),
                Level3Op::Remainder => self.gen_remainder_inst(),
            }
        }

        Ok(())
    }

    fn gen_l2_asm(&mut self, l2: Level2Exp) -> RustCcResult<()> {
        match l2 {
            Level2Exp::Const(u) => self.mov_into_primary(&u.to_string()),
            Level2Exp::Var(identifier) => {
                let sm = self.get_scoped_map_mut();
                let var_details = sm.get_var(&identifier)?;
                self.load_var(Self::PRIMARY_REGISTER, var_details.loc);
            }
            Level2Exp::Unary(op, factor) => {
                self.gen_l2_asm(*factor)?;
                match op {
                    Level2Op::UnaryMinus => self.write_unary_inst(Mnemonic::Neg),
                    Level2Op::LogicalNot => self.logical_not(),
                    Level2Op::BitwiseNot => self.write_unary_inst(Mnemonic::BitwiseNot),
                    _ => todo!("{op} is unimplemented for now"),
                }
            }
            Level2Exp::ParenExp(exp) => self.gen_l15_asm(*exp)?,
            Level2Exp::FunctionCall(fn_name, params) => {
                self.get_fn_map().validate_fn_call(&fn_name, params.len())?;
                for (reg, param) in params.iter().enumerate() {
                    // HACK: we can only pass 8 args, so just store the args to be passed in w0-w7
                    // in w8-w15 until we generate all the expressions
                    let tmp_reg = reg + 8;
                    self.gen_l15_asm(param.to_owned())?;
                    self.write_inst(&format!("mov   w{tmp_reg}, w0"));
                }

                for reg in 0..params.len() {
                    let tmp_reg = reg + 8;
                    self.write_inst(&format!("mov   w{reg}, w{tmp_reg}"));
                }

                // FIX: only for ARM
                self.write_inst(&format!("bl    _{fn_name}"));
            }
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! gen_level_asm {
    ($fn_name: ident, $t: ty, $next_fn: ident, $inst_fn: ident) => {
        fn $fn_name(&mut self, exp: $t) -> RustCcResult<()> {
            let (first_lower_level_exp, trailing_lower_level_exps) = exp.0;
            self.$next_fn(first_lower_level_exp)?;

            for (op, lower_level_exp) in trailing_lower_level_exps {
                self.push_stack();
                self.$next_fn(lower_level_exp)?;
                self.pop_stack_into_backup();
                self.$inst_fn(op.into());
            }

            Ok(())
        }
    };
}
