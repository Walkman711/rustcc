use crate::parsing::parser_types;
use std::io::Write;

#[derive(Clone, Debug)]
pub struct Context {
    pub function_name: String,
    pub num_args: usize,
    pub max_stack_offset: usize,
    pub buffer: Vec<String>,
    pub prologue: Vec<String>,
    pub epilogue: Vec<String>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            function_name: "GLOBAL_CONTEXT".to_owned(),
            num_args: 0,
            max_stack_offset: 0,
            buffer: vec![],
            prologue: vec![],
            epilogue: vec![],
        }
    }
}

impl From<&parser_types::Function> for Context {
    fn from(value: &parser_types::Function) -> Self {
        Self {
            function_name: value.0 .0.to_owned(),
            num_args: value.0 .1.len(),
            max_stack_offset: 0,
            buffer: vec![],
            prologue: vec![],
            epilogue: vec![],
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

    // TODO:
    pub fn write_to_file(&mut self, f: &mut std::fs::File) {
        for line in &self.prologue {
            writeln!(f, "{line}").expect("writeln! failed to write instruction to file.");
        }

        for line in &self.buffer {
            writeln!(f, "{line}").expect("writeln! failed to write instruction to file.");
        }

        for line in &self.epilogue {
            writeln!(f, "{line}").expect("writeln! failed to write instruction to file.");
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
