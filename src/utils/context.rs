use crate::{codegen::codegen_enums::Arch, parsing::parser_types};
use std::io::Write;

#[derive(Clone, Debug)]
pub enum Instruction {
    NoOffset(String),
    Address(String, usize),
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
    fn from(value: &parser_types::Function) -> Self {
        Self {
            function_name: value.0 .0.to_owned(),
            num_args: value.0 .1.len(),
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
        let diff = unaligned % 16;
        unaligned + diff
    }

    fn fn_prologue(&mut self, arch: Arch) {
        match arch {
            Arch::x86 => todo!(),
            Arch::ARM => {
                self.prologue
                    .push(format!(".global _{}", self.function_name));
                self.prologue.push(".align 2".to_string());
                self.prologue.push(format!("_{}:", self.function_name));

                // self.prologue.push(format!(
                //     "stp   x29, x30, [sp, -{}]!",
                //     self.get_stack_frame_size()
                // ));
                // self.prologue.push("mov   x29, sp".to_string());
            }
        }
    }

    // TODO:
    pub fn write_to_file(&mut self, f: &mut std::fs::File, arch: Arch) {
        self.fn_prologue(arch);
        for line in &self.prologue {
            writeln!(f, "{line}").expect("writeln! failed to write fn header to file.");
        }

        for line in &self.insts {
            match line {
                Instruction::NoOffset(inst) => {
                    writeln!(f, "{inst}").expect("writeln! failed to write inst to file")
                }
                Instruction::Address(inst, offset) => {
                    writeln!(f, "{inst}, [sp, {}]", self.max_stack_offset - offset)
                        .expect("writeln! failed to write inst to file")
                }
            }
        }
    }
}

// impl Context {
//     pub fn allocated_new_var(&mut self) {
//         self.num_locals += 1;
//         self.max_stack_offset = std::cmp::max(self.max_stack_offset, self.num_locals * 4);
//     }
//
//     pub fn deallocated_new_var(&mut self) {
//         self.num_locals -= 1;
//     }
// }
