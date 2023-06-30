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
        }
    }
}

// pub trait Ctx {
//     fn fn_prologue(&mut self);
//     fn write_inst_to_file(
//         &mut self,
//         f: &mut std::fs::File,
//         instr: &Instruction,
//     ) -> RustCcResult<()>;
//     fn calc_stack_frame_size(&self, num_args: usize, max_offset: usize) -> usize {
//         let default_size = 16;
//         let arg_size = 4 * num_args;
//         let unaligned = default_size + arg_size + max_offset;
//         let diff = 16 - (unaligned % 16);
//         unaligned + diff
//     }
// }

impl Context {
    pub fn calc_stack_frame_size(&self, num_args: usize, max_offset: usize) -> usize {
        let default_size = 16;
        let arg_size = 4 * num_args;
        let unaligned = default_size + arg_size + max_offset;
        let diff = 16 - (unaligned % 16);
        unaligned + diff
    }

    pub fn fn_prologue(&mut self) {
        match self.arch {
            Arch::x86 => self.x86_fn_prologue(),
            Arch::ARM => self.arm_fn_prologue(),
            Arch::RISCV => self.riscv_fn_prologue(),
        }
    }

    fn x86_fn_prologue(&mut self) {
        self.prologue.push(format!(".globl {}", self.function_name));
        self.prologue
            .push(format!(".type  {}, @function", self.function_name));
        self.prologue.push(format!("{}:", self.function_name));
        self.prologue.push("\tpushq %rbp".to_string());
        self.prologue.push("\tmovq  %rsp, %rbp".to_string());
        let stack_offset = self.get_stack_frame_size();
        self.prologue.push(format!("\tsub   ${stack_offset}, %rsp"));
    }

    fn arm_fn_prologue(&mut self) {
        self.prologue
            .push(format!("\t.global _{}", self.function_name));
        self.prologue.push("\t.align 2".to_string());
        self.prologue.push(format!("_{}:", self.function_name));

        let stack_offset = self.get_stack_frame_size();
        self.prologue
            .push(format!("\tsub   sp, sp, {stack_offset}"));
        self.prologue
            .push("\tstp   x29, x30, [sp, -16]!".to_string());
        self.prologue.push("\tmov   x29, sp".to_string());
    }

    fn riscv_fn_prologue(&mut self) {
        self.prologue.push(format!("{}:", self.function_name));
        let stack_offset = self.get_stack_frame_size();
        self.prologue
            .push(format!("\taddi  sp, sp, -{stack_offset}"));
    }

    fn write_inst_to_file(
        &mut self,
        f: &mut std::fs::File,
        instr: &Instruction,
    ) -> RustCcResult<()> {
        match self.arch {
            Arch::x86 => self.write_x86_inst_to_file(f, instr),
            Arch::ARM => self.write_arm_inst_to_file(f, instr),
            Arch::RISCV => self.write_riscv_inst_to_file(f, instr),
        }
    }

    fn write_x86_inst_to_file(
        &mut self,
        f: &mut std::fs::File,
        instr: &Instruction,
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

    fn write_arm_inst_to_file(
        &mut self,
        f: &mut std::fs::File,
        instr: &Instruction,
    ) -> RustCcResult<()> {
        match instr {
            Instruction::NoOffset(inst) => writeln!(f, "\t{inst}")?,
            Instruction::Address(inst, loc) => {
                if let VarLoc::Register(reg) = loc {
                    writeln!(f, "\t{inst}, w{reg}")?;
                } else {
                    let addend = match loc {
                        VarLoc::CurrFrame(offset) => self.get_stack_frame_size() - offset,
                        VarLoc::PrevFrame(offset) => self.get_stack_frame_size() + offset,
                        VarLoc::Register(_reg) => unreachable!("checked above"),
                        VarLoc::Global(_, offset) => self.get_stack_frame_size() - offset,
                    };
                    writeln!(f, "\t{inst}, [sp, {addend}]")?
                }
            }
            Instruction::Ret => {
                writeln!(f, "\tldp   x29, x30, [sp], 16")?;
                writeln!(f, "\tadd   sp, sp, {}", self.get_stack_frame_size())?;
                writeln!(f, "\tret")?;
            }
        }
        Ok(())
    }

    fn write_riscv_inst_to_file(
        &mut self,
        f: &mut std::fs::File,
        instr: &Instruction,
    ) -> RustCcResult<()> {
        match instr {
            Instruction::NoOffset(inst) => writeln!(f, "\t{inst}")?,
            Instruction::Address(inst, loc) => {
                // FIX: VarLoc Register should be a string
                if let VarLoc::Register(reg) = loc {
                    writeln!(f, "\t{inst}, a{reg}")?;
                } else {
                    let addend = match loc {
                        VarLoc::CurrFrame(offset) => self.get_stack_frame_size() - offset,
                        VarLoc::PrevFrame(offset) => self.get_stack_frame_size() + offset,
                        VarLoc::Register(_reg) => unreachable!("checked above"),
                        VarLoc::Global(_, offset) => self.get_stack_frame_size() - offset,
                    };
                    writeln!(f, "\t{inst}, {addend}(sp)")?
                }
            }
            Instruction::Ret => {
                writeln!(f, "\tmv    a0,a5")?;
                writeln!(f, "\taddi  sp, sp, {}", self.get_stack_frame_size())?;
                writeln!(f, "\tjr    ra")?;
            }
        }
        Ok(())
    }
}

impl Context {
    pub fn get_stack_frame_size(&self) -> usize {
        self.calc_stack_frame_size(self.num_args, self.max_stack_offset)
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
            self.write_inst_to_file(f, instr)?;
        }

        Ok(())
    }

    pub fn get_page_access(&self) -> String {
        if self.function_name == "main" {
            "PAGE".to_string()
        } else {
            "GOTPAGE".to_string()
        }
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
