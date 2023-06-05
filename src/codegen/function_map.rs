use std::collections::{HashMap, HashSet};

use crate::{
    parsing::parser_types::{Function, GlobalVar, Program, TopLevelItem},
    utils::error::{CodegenError, FunctionError, RustCcError, RustCcResult},
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FunctionType {
    Declaration,
    Definition,
}

pub struct FunctionMap {
    // Fn name -> (Decl/Def, #args)
    fn_info: HashMap<String, (FunctionType, usize)>,
}

impl TryFrom<&Program> for FunctionMap {
    type Error = RustCcError;

    fn try_from(prog: &Program) -> Result<Self, Self::Error> {
        let mut fn_info: HashMap<String, (FunctionType, usize)> = HashMap::new();
        let mut global_names = HashSet::new();
        for top_level_item in &prog.0 {
            match top_level_item {
                TopLevelItem::Var(v) => match v {
                    GlobalVar::Declaration(id) | GlobalVar::Definition(id, _) => {
                        global_names.insert(id);
                    }
                },
                TopLevelItem::Fun(function) => match function {
                    Function::Definition(name, args, _) => {
                        if let Some((prev_type, num_args)) =
                            fn_info.insert(name.to_owned(), (FunctionType::Definition, args.len()))
                        {
                            if prev_type == FunctionType::Definition {
                                Err(RustCcError::FunctionError(
                                    FunctionError::MultipleDefinitions(name.to_owned()),
                                ))?;
                            }

                            if num_args != args.len() {
                                Err(RustCcError::FunctionError(FunctionError::ArgumentMismatch(
                                    args.len(),
                                    num_args,
                                )))?;
                            }

                            if num_args > 8 {
                                todo!("Pass extra parameters on the stack.")
                            }
                        } else {
                            fn_info.insert(name.to_owned(), (FunctionType::Definition, args.len()));
                        }
                    }
                    Function::Declaration(name, args) => {
                        if let Some((_already_declared, num_args)) = fn_info.get(name) {
                            if *num_args != args.len() {
                                Err(RustCcError::FunctionError(FunctionError::ArgumentMismatch(
                                    args.len(),
                                    *num_args,
                                )))?;
                            }

                            if *num_args > 8 {
                                todo!("Pass extra parameters on the stack.")
                            }
                        } else {
                            fn_info
                                .insert(name.to_owned(), (FunctionType::Declaration, args.len()));
                        }
                    }
                },
            }
        }

        for id in global_names {
            if fn_info.contains_key(id) {
                return Err(RustCcError::CodegenError(
                    CodegenError::ReusedIdentifierForFunctionAndGlobal(id.to_owned()),
                ));
            }
        }

        Ok(Self { fn_info })
    }
}

impl FunctionMap {
    pub fn validate_fn_call(&self, fn_name: &str, num_args_in_call: usize) -> RustCcResult<()> {
        if let Some((_, num_args_in_definition)) = self.fn_info.get(fn_name) {
            if num_args_in_call == *num_args_in_definition {
                Ok(())
            } else {
                Err(RustCcError::FunctionError(FunctionError::ArgumentMismatch(
                    *num_args_in_definition,
                    num_args_in_call,
                )))
            }
        } else {
            Err(RustCcError::FunctionError(
                FunctionError::UndeclaredFunction(fn_name.to_owned()),
            ))
        }
    }
}
