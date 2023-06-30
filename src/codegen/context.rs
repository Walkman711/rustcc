#![allow(non_camel_case_types)]

use enum_dispatch::enum_dispatch;

use crate::{
    codegen::codegen_enums::Arch,
    parsing::parser_types::{self, Function},
    utils::{
        error::RustCcResult,
        scoped_map::{ScopedMap, VarLoc},
    },
};
use std::{fs::File, io::Write};

#[derive(Clone, Debug)]
pub enum Instruction {
    NoOffset(String),
    Address(String, VarLoc),
    Ret,
}

#[enum_dispatch]
#[derive(Clone, Debug)]
pub enum ArchContext {
    Arm(ArmContext),
    x86(x86Context),
}

#[derive(Clone, Debug)]
// TODO: un pub these
pub struct Context {
    pub function_name: String,
    pub num_args: usize,
    pub max_stack_offset: usize,
    pub insts: Vec<Instruction>,
    pub prologue: Vec<String>,
    pub scoped_map: ScopedMap,
    pub break_stack: Vec<usize>,
    pub continue_stack: Vec<usize>,
    pub curr_jmp_label: usize,
    pub arch: Arch,
    arch_ctx: ArchContext,
}

const GLOBAL_EXPECT: &str = "Will always have a global context";

impl Context {
    fn new(function: &parser_types::Function, scoped_map: ScopedMap, arch: Arch) -> Self {
        let (function_name, num_args) = match function {
            Function::Definition(function_name, _ret, args, _) => {
                (function_name.to_owned(), args.len())
            }
            Function::Declaration(function_name, _ret, args) => {
                (function_name.to_owned(), args.len())
            }
        };

        Self {
            function_name,
            num_args,
            max_stack_offset: 0,
            insts: vec![],
            prologue: vec![],
            scoped_map,
            break_stack: vec![],
            continue_stack: vec![],
            curr_jmp_label: 0,
            arch,
            arch_ctx: ArchContext::Arm(ArmContext {}),
        }
    }
}

impl Context {
    pub fn fn_prologue(&mut self) {
        let stack_frame_size = self.get_stack_frame_size();
        self.arch_ctx
            .fn_prologue(&mut self.prologue, &self.function_name, stack_frame_size);
    }

    pub fn get_stack_frame_size(&self) -> usize {
        self.arch_ctx
            .calc_stack_frame_size(self.num_args, self.max_stack_offset)
    }

    pub fn get_next_jmp_label(&mut self) -> usize {
        self.curr_jmp_label += 1;
        self.curr_jmp_label
    }

    pub fn write_to_file(&mut self, f: &mut std::fs::File) -> RustCcResult<()> {
        self.fn_prologue();
        for line in &self.prologue {
            writeln!(f, "{line}")?;
        }

        // BORROW: only happens once, so not a major issue
        let instr_copy = self.insts.clone();

        for instr in &instr_copy {
            self.arch_ctx
                .write_inst_to_file(f, instr, self.get_stack_frame_size())?;
        }

        Ok(())
    }
}

#[enum_dispatch(ArchContext)]
pub trait Ctx {
    fn fn_prologue(
        &mut self,
        prologue_buf: &mut Vec<String>,
        fn_name: &str,
        stack_frame_size: usize,
    );
    fn write_inst_to_file(
        &mut self,
        f: &mut std::fs::File,
        instr: &Instruction,
        stack_frame_size: usize,
    ) -> RustCcResult<()>;
    fn calc_stack_frame_size(&self, num_args: usize, max_offset: usize) -> usize {
        let default_size = 16;
        let arg_size = 4 * num_args;
        let unaligned = default_size + arg_size + max_offset;
        let diff = 16 - (unaligned % 16);
        unaligned + diff
    }
}

#[derive(Clone, Debug)]
pub struct ArmContext {}

impl Ctx for ArmContext {
    fn fn_prologue(
        &mut self,
        prologue_buf: &mut Vec<String>,
        fn_name: &str,
        stack_frame_size: usize,
    ) {
        prologue_buf.push(format!("\t.global _{fn_name}"));
        prologue_buf.push("\t.align 2".to_string());
        prologue_buf.push(format!("_{fn_name}:"));
        prologue_buf.push(format!("\tsub   sp, sp, {stack_frame_size}"));
        prologue_buf.push("\tstp   x29, x30, [sp, -16]!".to_string());
        prologue_buf.push("\tmov   x29, sp".to_string());
    }

    fn write_inst_to_file(
        &mut self,
        f: &mut std::fs::File,
        instr: &Instruction,
        stack_frame_size: usize,
    ) -> RustCcResult<()> {
        match instr {
            Instruction::NoOffset(inst) => writeln!(f, "\t{inst}")?,
            Instruction::Address(inst, loc) => {
                if let VarLoc::Register(reg) = loc {
                    writeln!(f, "\t{inst}, w{reg}")?;
                } else {
                    let addend = match loc {
                        VarLoc::CurrFrame(offset) => stack_frame_size - offset,
                        VarLoc::PrevFrame(offset) => stack_frame_size + offset,
                        VarLoc::Register(_reg) => unreachable!("checked above"),
                        VarLoc::Global(_, offset) => stack_frame_size - offset,
                    };
                    writeln!(f, "\t{inst}, [sp, {addend}]")?
                }
            }
            Instruction::Ret => {
                writeln!(f, "\tldp   x29, x30, [sp], 16")?;
                writeln!(f, "\tadd   sp, sp, {stack_frame_size}")?;
                writeln!(f, "\tret")?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct x86Context {}

impl Ctx for x86Context {
    fn fn_prologue(
        &mut self,
        prologue_buf: &mut Vec<String>,
        fn_name: &str,
        stack_frame_size: usize,
    ) {
        prologue_buf.push(format!(".globl {fn_name}"));
        prologue_buf.push(format!(".type  {fn_name}, @function"));
        prologue_buf.push(format!("{fn_name}:"));
        prologue_buf.push("\tpushq %rbp".to_string());
        prologue_buf.push("\tmovq  %rsp, %rbp".to_string());
        prologue_buf.push(format!("\tsub   ${stack_frame_size}, %rsp"));
    }

    fn write_inst_to_file(
        &mut self,
        f: &mut std::fs::File,
        instr: &Instruction,
        _stack_frame_size: usize,
    ) -> RustCcResult<()> {
        match instr {
            Instruction::NoOffset(inst) => writeln!(f, "\t{inst}")?,
            Instruction::Address(inst, loc) => {
                if let VarLoc::Register(reg) = loc {
                    writeln!(f, "\t{inst}, w{reg}")?;
                } else {
                    todo!("don't think i have to use address insts for x86?")
                }
            }
            Instruction::Ret => {
                writeln!(f, "\tmovq  %rbp, %rsp")?;
                writeln!(f, "\tpopq   %rbp")?;
                writeln!(f, "\tret")?;
            }
        }
        Ok(())
    }
}

// TODO: add function map?
pub struct GlobalContext {
    pub scoped_map: ScopedMap,
    pub function_contexts: Vec<Context>,
    pub defined_global_buffer: Vec<String>,
    pub declared_global_buffer: Vec<String>,
    pub arch: Arch,
}

impl GlobalContext {
    pub fn new(arch: Arch) -> Self {
        Self {
            scoped_map: ScopedMap::default(),
            function_contexts: vec![],
            defined_global_buffer: vec![],
            declared_global_buffer: vec![],
            arch,
        }
    }
    pub fn new_function_context(&mut self, function: &parser_types::Function) {
        let ctx = Context::new(function, self.scoped_map.clone(), self.arch);
        self.function_contexts.push(ctx);
    }

    pub fn curr_ctx(&self) -> &Context {
        self.function_contexts.last().expect(GLOBAL_EXPECT)
    }

    pub fn curr_ctx_mut(&mut self) -> &mut Context {
        self.function_contexts.last_mut().expect(GLOBAL_EXPECT)
    }

    pub fn write_defined_global_inst(&mut self, inst: String) {
        self.defined_global_buffer.push(inst);
    }

    pub fn write_declared_global_inst(&mut self, inst: String) {
        self.declared_global_buffer.push(inst);
    }

    pub fn write_to_file(&mut self, f: &mut File) -> RustCcResult<()> {
        self.write_data_section(f)?;
        for line in &self.defined_global_buffer {
            writeln!(f, "{line}")?;
        }

        self.write_text_section(f)?;
        for ctx in &mut self.function_contexts {
            ctx.write_to_file(f)?;
            writeln!(f)?;
        }

        for line in &self.declared_global_buffer {
            writeln!(f, "{line}")?;
        }

        Ok(())
    }

    fn write_data_section(&mut self, f: &mut File) -> RustCcResult<()> {
        match self.arch {
            Arch::x86 => writeln!(f, ".data")?,
            Arch::ARM => writeln!(f, ".section __DATA,__data")?,
            Arch::RISCV => todo!(),
        }
        Ok(())
    }

    fn write_text_section(&mut self, f: &mut File) -> RustCcResult<()> {
        match self.arch {
            Arch::x86 => writeln!(f, ".text")?,
            Arch::ARM => writeln!(f, ".section    __TEXT,__text,regular,pure_instructions")?,
            Arch::RISCV => todo!(),
        }
        Ok(())
    }
}
