use crate::{
    codegen::codegen_enums::Arch,
    parsing::parser_types::{self, Function},
    utils::scoped_map::VarLoc,
};
use std::io::Write;

#[derive(Clone, Debug)]
pub enum Instruction {
    NoOffset(String),
    Address(String, VarLoc),
}

#[derive(Clone, Debug)]
pub struct Context {
    pub function_name: String,
    pub num_args: usize,
    pub max_stack_offset: usize,
    pub insts: Vec<Instruction>,
    pub prologue: Vec<String>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            function_name: "GLOBAL_CONTEXT".to_owned(),
            num_args: 0,
            max_stack_offset: 0,
            insts: vec![],
            prologue: vec![],
        }
    }
}

impl From<&parser_types::Function> for Context {
    fn from(function: &parser_types::Function) -> Self {
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

    fn fn_prologue(&mut self, arch: Arch) {
        match arch {
            Arch::x86 => todo!(),
            Arch::ARM => {
                self.prologue
                    .push(".section    __TEXT,__text,regular,pure_instructions".to_string());
                self.prologue
                    .push(format!(".global _{}", self.function_name));
                self.prologue.push(".align 2".to_string());
                self.prologue.push(format!("_{}:", self.function_name));

                let stack_offset = self.get_stack_frame_size();
                self.prologue
                    .push(format!("\tsub   sp, sp, {stack_offset}"));
                self.prologue.push(format!("\tstp   x29, x30, [sp, -16]!",));
                self.prologue.push("\tmov   x29, sp".to_string());
            }
        }
    }

    // TODO:
    pub fn write_to_file(&mut self, f: &mut std::fs::File, arch: Arch) {
        // // HACK: going to make a proper global context struct and program context (global + vec of
        // // function contexts) in stage 10
        if self.function_name == "GLOBAL_CONTEXT" {
            for line in &self.insts {
                if let Instruction::NoOffset(inst) = line {
                    writeln!(f, "{inst}").expect("writeln! failed to write fn header to file.");
                }
            }
            writeln!(f).unwrap();
            return;
        }

        self.fn_prologue(arch);
        for line in &self.prologue {
            writeln!(f, "{line}").expect("writeln! failed to write fn header to file.");
        }

        for line in &self.insts {
            match line {
                Instruction::NoOffset(inst) => {
                    writeln!(f, "\t{inst}").expect("writeln! failed to write inst to file")
                }
                Instruction::Address(inst, loc) => {
                    if let VarLoc::Register(reg) = loc {
                        // writeln!(f, "ldr??").unwrap();
                        writeln!(f, "\t{inst}, w{reg}",)
                            // writeln!(f, "\t{inst}, [sp, {}]", offset)
                            .expect("writeln! failed to write inst to file")
                    } else {
                        let addend = match loc {
                            VarLoc::CurrFrame(offset) => self.get_stack_frame_size() - offset,
                            VarLoc::PrevFrame(offset) => self.get_stack_frame_size() + offset,
                            VarLoc::Register(_reg) => unreachable!("checked above"),
                            VarLoc::Global(_) => 10000,
                        };
                        writeln!(f, "\t{inst}, [sp, {addend}]",)
                            // writeln!(f, "\t{inst}, [sp, {}]", offset)
                            .expect("writeln! failed to write inst to file")
                    }
                }
            }
        }
    }
}
