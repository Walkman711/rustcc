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
    // Fn name -> (Decl/Def, arg names)
    // TODO: once we start having a notion of variable types, will also want to store that here as
    // well
    fn_info: HashMap<String, (FunctionType, Vec<String>)>,
}

impl TryFrom<&Program> for FunctionMap {
    type Error = RustCcError;

    fn try_from(prog: &Program) -> Result<Self, Self::Error> {
        let mut fn_info: HashMap<String, (FunctionType, Vec<String>)> = HashMap::new();
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
                        if let Some((prev_type, prev_args)) = fn_info
                            .insert(name.to_owned(), (FunctionType::Definition, args.to_owned()))
                        {
                            if prev_type == FunctionType::Definition {
                                return Err(
                                    FunctionError::MultipleDefinitions(name.to_owned()).into()
                                );
                            }

                            if prev_args.len() != args.len() {
                                return Err(FunctionError::ArgumentMismatch(
                                    args.len(),
                                    prev_args.len(),
                                )
                                .into());
                            }

                            // TODO: prev_args and args should have the same variable types and
                            // order. Check the standard if they need to have the same name. I
                            // don't believe so. This can't be done until we start to have types.

                            if args.len() > 8 {
                                todo!("Pass extra parameters on the stack.")
                            }
                        } else {
                            fn_info.insert(
                                name.to_owned(),
                                (FunctionType::Definition, args.to_owned()),
                            );
                        }
                    }
                    Function::Declaration(name, args) => {
                        if let Some((_already_declared, prev_args)) = fn_info.get(name) {
                            let num_prev_args = prev_args.len();
                            if num_prev_args != args.len() {
                                return Err(FunctionError::ArgumentMismatch(
                                    args.len(),
                                    num_prev_args,
                                )
                                .into());
                            }

                            if num_prev_args > 8 {
                                todo!("Pass extra parameters on the stack.")
                            }
                        } else {
                            fn_info.insert(
                                name.to_owned(),
                                (FunctionType::Declaration, args.to_owned()),
                            );
                        }
                    }
                },
            }
        }

        for id in global_names {
            if fn_info.contains_key(id) {
                return Err(
                    CodegenError::ReusedIdentifierForFunctionAndGlobal(id.to_owned()).into(),
                );
            }
        }

        let Some(main_fn) = fn_info.get("main") else {
            return Err(FunctionError::NoMain.into());
        };

        match main_fn.1.len() {
            0 => {}
            1 => {
                todo!("handle `(void)`-style main fns")
            }
            2 => {
                if !main_fn.1.contains(&"argv".to_string())
                    || !main_fn.1.contains(&"argc".to_string())
                {
                    return Err(FunctionError::BadArgumentsToMain(main_fn.1.to_owned()).into());
                }
            }
            _ => {
                return Err(FunctionError::BadArgumentsToMain(main_fn.1.to_owned()).into());
            }
        }

        Ok(Self { fn_info })
    }
}

impl FunctionMap {
    // TODO: update to check arg types once we handle new numeric types
    pub fn validate_fn_call(&self, fn_name: &str, num_args_in_call: usize) -> RustCcResult<()> {
        if let Some((_, args_in_definition)) = self.fn_info.get(fn_name) {
            if num_args_in_call == args_in_definition.len() {
                Ok(())
            } else {
                Err(
                    FunctionError::ArgumentMismatch(args_in_definition.len(), num_args_in_call)
                        .into(),
                )
            }
        } else {
            Err(FunctionError::UndeclaredFunction(fn_name.to_owned()).into())
        }
    }
}
