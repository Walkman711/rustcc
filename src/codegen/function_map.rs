use std::collections::{HashMap, HashSet};

use crate::{
    parsing::parser_types::{Function, GlobalVar, Param, Program, TopLevelItem},
    utils::{
        error::{CodegenError, FunctionError, RustCcError, RustCcResult},
        types::{BasicType, IntegerType, ReturnType},
    },
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
    // TODO: I don't love this - should store function, but that'll be a messier update
    fn_info: HashMap<String, (FunctionType, ReturnType, Vec<Param>)>,
}

impl TryFrom<&Program> for FunctionMap {
    type Error = RustCcError;

    fn try_from(prog: &Program) -> Result<Self, Self::Error> {
        let mut fn_info: HashMap<String, (FunctionType, ReturnType, Vec<Param>)> = HashMap::new();
        let mut global_names = HashSet::new();
        for top_level_item in &prog.0 {
            match top_level_item {
                TopLevelItem::Var(v) => match v {
                    GlobalVar::Declaration(id) | GlobalVar::Definition(id, _) => {
                        global_names.insert(id);
                    }
                },
                TopLevelItem::Fun(function) => match function {
                    Function::Definition(name, ret, args, _) => {
                        if let Some((prev_type, prev_ret, prev_args)) = fn_info.insert(
                            name.to_owned(),
                            (FunctionType::Definition, *ret, args.to_owned()),
                        ) {
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

                            if *ret != prev_ret {
                                todo!("Definition and declaration don't have same return type");
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
                                (FunctionType::Definition, *ret, args.to_owned()),
                            );
                        }
                    }
                    Function::Declaration(name, ret, args) => {
                        if let Some((_already_declared, prev_ret, prev_args)) = fn_info.get(name) {
                            let num_prev_args = prev_args.len();
                            if num_prev_args != args.len() {
                                return Err(FunctionError::ArgumentMismatch(
                                    args.len(),
                                    num_prev_args,
                                )
                                .into());
                            }

                            if ret != prev_ret {
                                todo!("Definition and declaration don't have same return type");
                            }

                            if num_prev_args > 8 {
                                todo!("Pass extra parameters on the stack.")
                            }
                        } else {
                            fn_info.insert(
                                name.to_owned(),
                                (FunctionType::Declaration, *ret, args.to_owned()),
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

        if main_fn.1 != ReturnType::NonVoid(BasicType::Int(IntegerType::Int).into()) {
            todo!("Error: Main has to return type `int`");
        }

        let main_args = &main_fn.2;

        match main_args.len() {
            0 => {}
            2 => {
                let argc = main_args.iter().any(|param| param.id == "argc");
                let argv = main_args.iter().any(|param| param.id == "argv");
                if !argc || !argv {
                    return Err(FunctionError::BadArgumentsToMain(main_args.to_owned()).into());
                }
            }
            _ => {
                return Err(FunctionError::BadArgumentsToMain(main_args.to_owned()).into());
            }
        }

        Ok(Self { fn_info })
    }
}

impl FunctionMap {
    // TODO: update to check arg types once we handle new numeric types. This seems like it'll be
    // difficult with expressions. Will start needing to reason about what the return type of an
    // expression is.
    // TODO: update to check that the return type of the function will work with the variable the
    // result is assigned to.
    pub fn validate_fn_call(&self, fn_name: &str, num_args_in_call: usize) -> RustCcResult<()> {
        if let Some((_, _ret, args_in_definition)) = self.fn_info.get(fn_name) {
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

    pub fn ret_type(&self, fn_name: &str) -> ReturnType {
        self.fn_info.get(fn_name).unwrap().1
    }
}
