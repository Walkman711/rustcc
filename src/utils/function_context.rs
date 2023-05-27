use crate::parsing::parser_types;

#[derive(Clone, Debug)]
pub struct FunctionContext {
    pub function_name: String,
    pub num_args: usize,
    pub max_stack_offset: usize,
}

impl From<&parser_types::Function> for FunctionContext {
    fn from(value: &parser_types::Function) -> Self {
        Self {
            function_name: value.0 .0.to_owned(),
            num_args: value.0 .1.len(),
            max_stack_offset: 0,
        }
    }
}

impl FunctionContext {
    pub fn get_stack_frame_size(&self) -> usize {
        let default_size = 16;
        let arg_size = 4 * self.num_args;
        let unaligned = default_size + arg_size + self.max_stack_offset;
        let diff = unaligned % 16;
        unaligned + diff
    }
}

// impl FunctionContext {
//     pub fn allocated_new_var(&mut self) {
//         self.num_locals += 1;
//         self.max_stack_offset = std::cmp::max(self.max_stack_offset, self.num_locals * 4);
//     }
//
//     pub fn deallocated_new_var(&mut self) {
//         self.num_locals -= 1;
//     }
// }
