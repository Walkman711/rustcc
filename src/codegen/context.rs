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
}

const GLOBAL_EXPECT: &str = "Will always have a global context";

impl Context {
    fn new(function: &parser_types::Function, scoped_map: ScopedMap) -> Self {
        let (function_name, num_args) = match function {
            Function::Definition(function_name, args, _) => (function_name.to_owned(), args.len()),
            Function::Declaration(function_name, args) => (function_name.to_owned(), args.len()),
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
        }
    }
}

impl Context {
    pub fn get_stack_frame_size(&self) -> usize {
        let default_size = 16;
        let arg_size = 4 * self.num_args;
        let unaligned = default_size + arg_size + self.max_stack_offset;
        let diff = 16 - (unaligned % 16);
        unaligned + diff
    }

    pub fn get_next_jmp_label(&mut self) -> usize {
        self.curr_jmp_label += 1;
        self.curr_jmp_label
    }

    fn fn_prologue(&mut self, arch: Arch) {
        match arch {
            Arch::x86 => todo!(),
            Arch::ARM => {
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
        }
    }

    pub fn write_to_file(&mut self, f: &mut std::fs::File, arch: Arch) -> RustCcResult<()> {
        self.fn_prologue(arch);
        for line in &self.prologue {
            writeln!(f, "{line}")?;
        }

        for line in &self.insts {
            match line {
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
#[derive(Default)]
pub struct GlobalContext {
    pub scoped_map: ScopedMap,
    pub function_contexts: Vec<Context>,
    pub defined_global_buffer: Vec<String>,
    pub declared_global_buffer: Vec<String>,
}

impl GlobalContext {
    pub fn new_function_context(&mut self, function: &parser_types::Function) {
        let ctx = Context::new(function, self.scoped_map.clone());
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

    pub fn write_to_file(&mut self, f: &mut File, arch: Arch) -> RustCcResult<()> {
        writeln!(f, ".section __DATA,__data")?;
        for line in &self.defined_global_buffer {
            writeln!(f, "{line}")?;
        }

        writeln!(f, ".section    __TEXT,__text,regular,pure_instructions")?;
        for ctx in &mut self.function_contexts {
            ctx.write_to_file(f, arch)?;
            writeln!(f)?;
        }

        for line in &self.declared_global_buffer {
            writeln!(f, "{line}")?;
        }

        Ok(())
    }
}
