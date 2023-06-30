use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    fs::File,
};

use crate::{
    gen_level_asm,
    parsing::{ops::*, parser_types::*},
    utils::{
        error::{CodegenError, RustCcResult},
        scoped_map::{ScopedMap, VarLoc},
        types::{BasicType, IntegerType, ReturnType},
    },
};

use super::{
    codegen_enums::{Arch, Cond, Mnemonic},
    context::{Context, GlobalContext, Instruction},
    function_map::FunctionMap,
};

pub trait AsmGenerator {
    const PRIMARY_REGISTER: &'static str;
    const BACKUP_REGISTER: &'static str;
    const RETURN_REGISTER: &'static str;
    const DEFAULT_ARGS: &'static str;
    const UNARY_ARGS: &'static str;
    const INT_SIZE: usize;

    /* Trivial getters and setters that clients need to implement */
    fn get_fn_map(&self) -> &FunctionMap;
    fn get_arch(&self) -> Arch;
    fn global_context(&self) -> &GlobalContext;
    fn global_context_mut(&mut self) -> &mut GlobalContext;
    fn stack_ptr(&self) -> usize;
    fn increment_stack_ptr(&mut self);
    fn decrement_stack_ptr(&mut self);

    /* More assembly generation stuff that clients need to implement */

    /// Write instruction to store `PRIMARY_REGISTER` on the stack.
    fn save_to_stack(&mut self, stack_offset: usize);

    /// Write instruction to store `PRIMARY_REGISTER` on the stack.
    fn assign_to_global(&mut self, id: &str, offset: usize);

    /// Write instruction to define global variables
    fn define_global(&mut self, id: &str, val: &i64);

    /// Write instruction to declare uninitialized global variables
    fn declare_global(&mut self, id: &str);

    /// Loads the variable stored at `loc` to the specified register.
    fn load_var(&mut self, dst_reg: &str, loc: VarLoc);

    /// Compares `PRIMARY_REGISTER` with `BACKUP_REGISTER`. Stores result in `PRIMARY_REGISTER`
    fn compare_primary_with_backup(&mut self, cond: Cond);

    /// Write instructions to perform logical negation of the contents of `PRIMARY_REGISTER` and
    /// store the result in `PRIMARY_REGISTER`.
    fn logical_not(&mut self);

    /// Write instructions for branching on a specified condition. Requires a comparison to be made
    /// prior to calling.
    // TODO: once we target more asms this could be removed as long as the client specifies the
    // jump mnemonic. Not sure if any targets differ wildly w/r/t jumping
    // TODO: wonder if we could statically enforce comparisons coming before branch insts?
    fn write_branch_inst(&mut self, cond: Cond, lbl: usize);

    fn write_fn_call(&mut self, fn_name: &str, args: Vec<Level15Exp>) -> RustCcResult<()>;

    /// When entering a function, move the arguments from registers onto the stack
    fn move_args_into_curr_frame(&mut self, params: &[Param]) -> RustCcResult<()>;

    /// Compare `PRIMARY_REGISTER` with 0 and store result in `PRIMARY_REGISTER`.
    fn cmp_primary_with_zero(&mut self);

    /// Moves `val` into `PRIMARY_REGISTER`. Can use numeric literals or registers.
    fn mov_into_primary(&mut self, val: &str);

    /// Write instructions needed to perform modular division.
    fn gen_remainder_inst(&mut self);

    /// Write division instruction. Needed for x86
    fn gen_div_inst(&mut self) {
        self.write_mnemonic(Mnemonic::Divide);
    }

    /* Getters and setters that don't need to be implemented by structs */
    fn get_scoped_map(&self) -> &ScopedMap {
        &self.curr_ctx().scoped_map
    }
    fn get_scoped_map_mut(&mut self) -> &mut ScopedMap {
        &mut self.curr_ctx_mut().scoped_map
    }
    fn get_break_stack(&self) -> &Vec<usize> {
        &self.curr_ctx().break_stack
    }
    fn get_break_stack_mut(&mut self) -> &mut Vec<usize> {
        &mut self.curr_ctx_mut().break_stack
    }
    fn get_continue_stack(&self) -> &Vec<usize> {
        &self.curr_ctx().continue_stack
    }
    fn get_continue_stack_mut(&mut self) -> &mut Vec<usize> {
        &mut self.curr_ctx_mut().continue_stack
    }
    fn curr_ctx(&self) -> &Context {
        self.global_context().curr_ctx()
    }
    fn curr_ctx_mut(&mut self) -> &mut Context {
        self.global_context_mut().curr_ctx_mut()
    }

    /* Instruction utilities */
    fn write_to_buffer(&mut self, inst: Instruction) {
        self.curr_ctx_mut().insts.push(inst);
    }

    fn write_to_file(&mut self, asm_filename: &str) -> RustCcResult<()> {
        let mut asm_file = File::create(asm_filename)?;
        self.global_context_mut().write_to_file(&mut asm_file)
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
        // HACK: I dislike this immensely, need to come up with something better for x86 asm
        if self.get_arch() == Arch::x86 {
            self.write_inst("movl %edx, %eax");
        }
    }

    fn write_unary_inst(&mut self, mnemonic: Mnemonic) {
        self.write_inst(&format!(
            "{:5} {}",
            mnemonic.for_arch(self.get_arch()),
            Self::UNARY_ARGS
        ));
    }

    /* Provided stack ops */
    fn pop_stack_into_backup(&mut self) {
        self.load_var(Self::BACKUP_REGISTER, VarLoc::CurrFrame(self.stack_ptr()));
        self.decrement_stack_ptr();
    }

    fn push_stack(&mut self) {
        self.increment_stack_ptr();
        self.save_to_stack(self.stack_ptr());
        let sp = self.stack_ptr();
        let fc = self.curr_ctx_mut();
        fc.max_stack_offset = max(fc.max_stack_offset, sp);
    }

    /* Jmp label utilities */
    fn get_next_jmp_label(&mut self) -> usize {
        self.curr_ctx_mut().get_next_jmp_label()
    }

    fn write_jmp_label(&mut self, lbl: usize) {
        self.write_inst(&format!(".L{lbl}_{}:", self.curr_ctx().function_name));
    }

    fn gen_asm(&mut self, asm_filename: &str, prog: Program) -> RustCcResult<()> {
        let mut offset = Self::INT_SIZE;
        let mut defined_globals = HashMap::new();
        let mut declared_globals = HashSet::new();
        for top_level_item in &prog.0 {
            match top_level_item {
                TopLevelItem::Var(GlobalVar::Definition(id, val)) => {
                    let gc = self.global_context_mut();
                    gc.scoped_map.initialize_var(
                        id,
                        BasicType::Int(IntegerType::Int).into(),
                        ReturnType::NonVoid(BasicType::Int(IntegerType::Int).into()),
                        VarLoc::Global(id.to_owned(), offset),
                    )?;
                    offset += Self::INT_SIZE;
                    defined_globals.insert(id.to_owned(), val);
                    self.increment_stack_ptr();
                }
                TopLevelItem::Var(GlobalVar::Declaration(id)) => {
                    let gc = self.global_context_mut();
                    gc.scoped_map.declare_var(
                        id,
                        BasicType::Int(IntegerType::Int).into(),
                        VarLoc::Global(id.to_owned(), offset),
                    )?;
                    offset += Self::INT_SIZE;

                    declared_globals.insert(id.to_owned());

                    self.increment_stack_ptr();
                }
                TopLevelItem::Fun(function) => match function {
                    Function::Declaration(..) => {
                        /* We've already constructed the function map, so we don't need to worry
                         * about function declarations preceding the function's first use */
                    }
                    Function::Definition(_id, _ret, params, block_items) => {
                        /* Making a new function, so declare a new context */
                        self.global_context_mut().new_function_context(function);

                        /* And then enter a new scope */
                        self.get_scoped_map_mut().new_scope()?;

                        /* Move arguments from registers and the previous stack frame into the
                         * current stack frame */
                        self.move_args_into_curr_frame(params)?;

                        // PERF: don't clone this
                        self.gen_block_asm(block_items.to_owned())?;

                        let num_deallocated_vars = self.get_scoped_map_mut().exit_scope()?;
                        for _ in 0..num_deallocated_vars {
                            self.decrement_stack_ptr();
                        }
                    }
                },
            }
        }

        for (id, val) in &defined_globals {
            self.define_global(id, val);
        }

        for id in &declared_globals {
            if !defined_globals.contains_key(id) {
                self.declare_global(id);
            }
        }

        self.write_to_file(asm_filename)?;

        Ok(())
    }

    fn gen_var_decl_asm(&mut self, decl: Declaration) -> RustCcResult<()> {
        let (id, lh_type, exp_opt) = decl;
        let var_loc = self.stack_ptr();
        // TODO: this is incorrect. should be able to get the type out of declaration
        match exp_opt {
            Some(exp) => {
                let rh_type = exp.exp_type(self.get_fn_map(), self.get_scoped_map());
                let sm = self.get_scoped_map_mut();
                sm.initialize_var(
                    &id,
                    lh_type,
                    rh_type,
                    VarLoc::CurrFrame(var_loc + Self::INT_SIZE),
                )?;
                self.gen_l15_asm(exp)?;
                self.push_stack();
            }
            None => {
                let sm = self.get_scoped_map_mut();
                sm.declare_var(&id, lh_type, VarLoc::CurrFrame(var_loc + Self::INT_SIZE))?;
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
            let variables_to_deallocate = self.get_scoped_map_mut().exit_scope()?;

            for _ in 0..variables_to_deallocate {
                self.decrement_stack_ptr();
            }
        }

        Ok(())
    }

    fn gen_stmt_asm(&mut self, stmt: Statement) -> RustCcResult<()> {
        match stmt {
            Statement::Exp(exp_opt) => {
                if let Some(exp) = exp_opt {
                    self.gen_l15_asm(exp)?;
                }
            }
            Statement::Select(select_stmt) => self.gen_select_stmt_asm(select_stmt)?,
            Statement::Compound(block_items) => self.gen_block_asm(block_items)?,
            Statement::Iter(iter_stmt) => self.gen_iter_stmt_asm(iter_stmt)?,
            Statement::Jmp(jmp_stmt) => self.gen_jmp_stmt_asm(jmp_stmt)?,
        }

        Ok(())
    }

    fn gen_select_stmt_asm(&mut self, select_stmt: SelectionStatement) -> RustCcResult<()> {
        match select_stmt {
            SelectionStatement::If(exp, predicate, else_opt) => {
                let else_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();

                /* Evaluate exp and store in primary register */
                self.gen_l15_asm(exp)?;

                /* If the exp == 0, trigger the else case to minimize the jump
                 * instructions we need */
                self.cmp_primary_with_zero();

                self.write_branch_inst(Cond::Equals, else_label);

                /* Execute if the if-exp is nonzero */
                self.gen_stmt_asm(*predicate)?;
                self.write_branch_inst(Cond::Always, exit_label);

                /* If we don't have an else case, jumping to this label
                 * will just cause us to fall through to the exit label */
                self.write_jmp_label(else_label);

                if let Some(else_stmt) = else_opt {
                    self.gen_stmt_asm(*else_stmt)?;
                }
                self.write_jmp_label(exit_label);
            }
        }
        Ok(())
    }

    fn gen_iter_stmt_asm(&mut self, iter_stmt: IterationStatement) -> RustCcResult<()> {
        match iter_stmt {
            IterationStatement::While(exp, stmt) => {
                let continue_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();

                self.get_continue_stack_mut().push(continue_label);
                self.get_break_stack_mut().push(exit_label);

                self.write_jmp_label(continue_label);

                /* Evaluate exp and store in primary register */
                self.gen_l15_asm(exp)?;

                /* If the exp == 0, trigger the else case to minimize the jump
                 * instructions we need */
                self.cmp_primary_with_zero();

                self.write_branch_inst(Cond::Equals, exit_label);

                /* Execute if the if-exp is nonzero */
                self.gen_stmt_asm(*stmt)?;
                self.write_branch_inst(Cond::Always, continue_label);

                self.write_jmp_label(exit_label);
            }
            IterationStatement::DoWhile(stmt, exp) => {
                let continue_label = self.get_next_jmp_label();
                let exit_label = self.get_next_jmp_label();

                self.get_continue_stack_mut().push(continue_label);
                self.get_break_stack_mut().push(exit_label);

                /* DO stmt */
                self.write_jmp_label(continue_label);
                self.gen_stmt_asm(*stmt)?;

                /* Evaluate exp and store in primary register */
                self.gen_l15_asm(exp)?;

                self.cmp_primary_with_zero();
                self.write_branch_inst(Cond::NotEquals, continue_label);
                self.write_jmp_label(exit_label);
            }
            IterationStatement::For(initial_exp, controlling_exp, post_exp, body) => {
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

                /* Skip to end if primary reg == 0 */
                self.cmp_primary_with_zero();
                self.write_branch_inst(Cond::Equals, exit_label);

                /* Exec stmt */
                self.gen_stmt_asm(*body)?;

                /* Eval post exp */
                if let Some(exp) = post_exp {
                    self.gen_l15_asm(exp)?;
                }

                self.write_branch_inst(Cond::Always, continue_label);

                self.write_jmp_label(exit_label);
            }
            IterationStatement::ForDecl(decl, controlling_exp, post_exp, body) => {
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

                /* Empty controlling exps evaluate to true */
                match controlling_exp {
                    Some(exp) => self.gen_l15_asm(exp)?,
                    None => self.mov_into_primary("1"),
                };

                /* Skip to end if primary reg == 0 */
                self.cmp_primary_with_zero();
                self.write_branch_inst(Cond::Equals, exit_label);

                /* Exec stmt */
                self.gen_stmt_asm(*body)?;

                self.write_jmp_label(continue_label);

                /* Eval post exp */
                if let Some(exp) = post_exp {
                    self.gen_l15_asm(exp)?;
                }

                self.write_branch_inst(Cond::Always, start_label);

                self.write_jmp_label(exit_label);
                {
                    let variables_to_deallocate = self.get_scoped_map_mut().exit_scope()?;

                    for _ in 0..variables_to_deallocate {
                        self.decrement_stack_ptr();
                    }
                }
            }
        }

        Ok(())
    }

    fn gen_jmp_stmt_asm(&mut self, js: JumpStatement) -> RustCcResult<()> {
        match js {
            JumpStatement::Break => {
                if let Some(break_label) = self.get_break_stack().last() {
                    self.write_branch_inst(Cond::Always, *break_label);
                } else {
                    return Err(CodegenError::UnenclosedBreak.into());
                }
            }
            JumpStatement::Continue => {
                if let Some(continue_label) = self.get_continue_stack().last() {
                    self.write_branch_inst(Cond::Always, *continue_label);
                } else {
                    return Err(CodegenError::UnenclosedContinue.into());
                }
            }
            JumpStatement::Return(exp_opt) => {
                match exp_opt {
                    Some(exp) => {
                        let exp_ret_type = exp.exp_type(self.get_fn_map(), self.get_scoped_map());
                        let fun_ret_type =
                            self.get_fn_map().ret_type(&self.curr_ctx().function_name);
                        if exp_ret_type != fun_ret_type {
                            return Err(CodegenError::ReturningIncorrectType(
                                self.curr_ctx().function_name.to_owned(),
                                fun_ret_type,
                                exp_ret_type,
                            )
                            .into());
                        }

                        self.gen_l15_asm(exp)?;
                    }
                    /* C standard states that int fns without a return value return 0 by default */
                    None => {
                        self.mov_into_primary("0");
                    }
                }
                self.write_to_buffer(Instruction::Ret);
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
                let exp_type = l15_exp.exp_type(self.get_fn_map(), self.get_scoped_map());
                let sm = self.get_scoped_map_mut();
                let var_loc = sm.assign_var(&identifier, exp_type)?;
                self.gen_l15_asm(*l15_exp)?;
                match var_loc {
                    VarLoc::CurrFrame(offset) => self.save_to_stack(offset),
                    VarLoc::PrevFrame(_) => {
                        return Err(CodegenError::AssignedToVarInPrevFrame.into());
                    }
                    VarLoc::Register(_) => {
                        return Err(CodegenError::AssignedToVarInRegister.into());
                    }
                    VarLoc::Global(id, offset) => {
                        self.assign_to_global(&id, offset);
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

                /* If the exp == 0, trigger the else case
                 * to minimize the jump instructions we need */
                self.cmp_primary_with_zero();

                self.write_branch_inst(Cond::Equals, else_label);

                /* Execute if the if-exp is nonzero */
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
            /* Get the last term checked */
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
    gen_level_asm!(
        gen_l7_asm,
        Level7Exp,
        gen_l6_asm,
        compare_primary_with_backup
    );
    gen_level_asm!(
        gen_l6_asm,
        Level6Exp,
        gen_l5_asm,
        compare_primary_with_backup
    );
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
                Level3Op::Division => self.gen_div_inst(),
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
                self.gen_l2_asm(*factor.clone())?;
                match op {
                    Level2Op::UnaryMinus => self.write_unary_inst(Mnemonic::Neg),
                    Level2Op::LogicalNot => self.logical_not(),
                    Level2Op::BitwiseNot => self.write_unary_inst(Mnemonic::BitwiseNot),
                    Level2Op::SizeOf => {
                        let sm = self.get_scoped_map();
                        let fn_map = self.get_fn_map();
                        let factor_type = factor.exp_type(fn_map, sm);
                        let size = factor_type.sizeof();
                        self.mov_into_primary(&size.to_string());
                    }
                    _ => todo!("{op} is unimplemented for now"),
                }
            }
            Level2Exp::ParenExp(exp) => self.gen_l15_asm(*exp)?,
            Level2Exp::FunctionCall(fn_name, args) => {
                self.get_fn_map().validate_fn_call(&fn_name, args.len())?;
                self.write_fn_call(&fn_name, args)?;
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
