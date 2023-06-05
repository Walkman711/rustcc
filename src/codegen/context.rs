use crate::{
    codegen::codegen_enums::Arch,
    parsing::parser_types::{self, Function},
    utils::scoped_map::{ScopedMap, VarLoc},
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

const WRITELN_EXPECT: &'static str = "Writeln! failed to write to file.";

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
                self.prologue.push(format!("\tstp   x29, x30, [sp, -16]!",));
                self.prologue.push("\tmov   x29, sp".to_string());

                // dbg!(self.scoped_map.get_globals().unwrap());
                // TODO: load globals in at prologue time?
                // NOTE: requires moving scope map in here
                // for (id, details) in self.scoped_map.get_globals().unwrap() {
                //     if let VarLoc::Global(_, offset) = details.loc {
                //         self.prologue.push(format!("\tadrp  x9, _{id}@PAGE"));
                //         self.prologue
                //             .push(format!("\tldr   w8, [x8, _{id}@PAGEOFF]"));
                //         self.prologue.push("\tmov   w0, w8".to_string());
                //         // FIX: sp in context??
                //         self.prologue.push(format!(
                //             "\tstr   w0, [sp, {}]",
                //             self.get_stack_frame_size() - offset
                //         ));
                //     }
                // }
            }
        }
    }

    // TODO:
    pub fn write_to_file(&mut self, f: &mut std::fs::File, arch: Arch) {
        self.fn_prologue(arch);
        for line in &self.prologue {
            writeln!(f, "{line}").expect(WRITELN_EXPECT);
        }

        for line in &self.insts {
            match line {
                Instruction::NoOffset(inst) => writeln!(f, "\t{inst}").expect(WRITELN_EXPECT),
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
                            VarLoc::Global(_, offset) => self.get_stack_frame_size() - offset,
                        };
                        writeln!(f, "\t{inst}, [sp, {addend}]",)
                            // writeln!(f, "\t{inst}, [sp, {}]", offset)
                            .expect(WRITELN_EXPECT)
                    }
                }
                Instruction::Ret => {
                    writeln!(f, "\tldp   x29, x30, [sp], 16").expect(WRITELN_EXPECT);
                    writeln!(f, "\tadd   sp, sp, {}", self.get_stack_frame_size())
                        .expect(WRITELN_EXPECT);
                    writeln!(f, "\tret").expect(WRITELN_EXPECT);
                }
            }
        }
    }
}

// TODO: add function map?
pub struct GlobalContext {
    pub scoped_map: ScopedMap,
    pub function_contexts: Vec<Context>,
    pub defined_global_buffer: Vec<String>,
    pub declared_global_buffer: Vec<String>,
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self {
            scoped_map: ScopedMap::default(),
            function_contexts: vec![],
            defined_global_buffer: vec![],
            declared_global_buffer: vec![],
        }
    }
}

impl GlobalContext {
    pub fn new_function_context(&mut self, function: &parser_types::Function) {
        let ctx = Context::new(function, self.scoped_map.clone());
        self.function_contexts.push(ctx);
    }

    pub fn curr_function_context(&self) -> &Context {
        self.function_contexts
            .last()
            .expect("Will always have a global context.")
    }

    pub fn curr_function_context_mut(&mut self) -> &mut Context {
        self.function_contexts
            .last_mut()
            .expect("Will always have a global context.")
    }

    pub fn write_defined_global_inst(&mut self, inst: String) {
        self.defined_global_buffer.push(inst);
    }

    pub fn write_declared_global_inst(&mut self, inst: String) {
        self.declared_global_buffer.push(inst.to_owned());
    }

    pub fn write_to_file(&mut self, f: &mut File, arch: Arch) {
        writeln!(f, ".section __DATA,__data").expect(WRITELN_EXPECT);
        for line in &self.defined_global_buffer {
            writeln!(f, "{line}").expect(WRITELN_EXPECT);
        }

        writeln!(f, ".section    __TEXT,__text,regular,pure_instructions").expect(WRITELN_EXPECT);
        for ctx in &mut self.function_contexts {
            ctx.write_to_file(f, arch);
            writeln!(f).expect(WRITELN_EXPECT);
        }

        writeln!(f).expect(WRITELN_EXPECT);
        for line in &self.declared_global_buffer {
            writeln!(f, "{line}").expect(WRITELN_EXPECT);
        }
    }
}
