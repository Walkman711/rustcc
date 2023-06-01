use std::collections::HashMap;

use super::error::{RustCcError, RustCcResult, ScopeError};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum VarState {
    Param,
    InitializedInThisScope,
    InitializedInOuterScope,
    DeclaredInThisScope,
    DeclaredInOuterScope,
}

#[derive(Clone, Debug)]
pub enum VarLoc {
    CurrFrame(usize),
    PrevFrame(usize),
    Register(usize),
    Global(String, Option<usize>),
}

#[derive(Clone, Debug)]
pub struct VarDetails {
    state: VarState,
    pub loc: VarLoc,
}

#[derive(Clone, Debug)]
pub struct ScopedMap {
    // var_name -> loc
    var_maps: Vec<HashMap<String, VarDetails>>,
}

impl Default for ScopedMap {
    fn default() -> Self {
        Self {
            var_maps: vec![HashMap::new()],
        }
    }
}

impl ScopedMap {
    pub fn new_param(&mut self, var: &str, loc: VarLoc) -> RustCcResult<()> {
        let Some(last) = self.var_maps.last_mut() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        let details = VarDetails {
            state: VarState::Param,
            loc,
        };

        if let Some(VarDetails {
            state: VarState::Param,
            ..
        }) = last.insert(var.to_owned(), details)
        {
            panic!("can't use same param name twice in a function");
        }

        Ok(())
    }

    pub fn initialize_var(&mut self, var: &str, loc: VarLoc) -> RustCcResult<()> {
        let Some(last) = self.var_maps.last_mut() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        let details = VarDetails {
            state: VarState::InitializedInThisScope,
            loc,
        };

        if let Some(VarDetails { state, .. }) = last.insert(var.to_owned(), details) {
            match state {
                VarState::Param => {
                    Err(RustCcError::ScopeError(ScopeError::ReusedParamName(
                        var.to_owned(),
                    )))?;
                }
                VarState::InitializedInThisScope => {
                    Err(RustCcError::ScopeError(
                        ScopeError::InitializedTwiceInSameScope(var.to_owned()),
                    ))?;
                }
                VarState::InitializedInOuterScope
                | VarState::DeclaredInThisScope
                | VarState::DeclaredInOuterScope => {}
            }
        }

        Ok(())
    }

    pub fn declare_var(&mut self, var: &str, loc: VarLoc) -> RustCcResult<()> {
        let Some(last) = self.var_maps.last_mut() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        let details = VarDetails {
            state: VarState::DeclaredInThisScope,
            loc,
        };

        if let Some(VarDetails {
            state: VarState::DeclaredInThisScope,
            ..
        }) = last.insert(var.to_owned(), details)
        {
            Err(RustCcError::ScopeError(
                ScopeError::DeclaredTwiceInSameScope(var.to_owned()),
            ))?;
        }

        Ok(())
    }

    pub fn assign_var(&mut self, var: &str, offset: usize) -> RustCcResult<VarLoc> {
        let Some(last) = self.var_maps.last_mut() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        if let Some(var_details) = last.get_mut(var) {
            match var_details.state {
                VarState::Param => {
                    panic!("Can't assign to a parameter. TODO: make an actual error");
                }
                VarState::DeclaredInThisScope => {
                    var_details.state = VarState::InitializedInThisScope
                }
                VarState::DeclaredInOuterScope => {
                    var_details.state = VarState::InitializedInOuterScope
                }
                VarState::InitializedInThisScope | VarState::InitializedInOuterScope => {}
            }

            if let VarLoc::Global(id, None) = &var_details.loc {
                var_details.loc = VarLoc::Global(id.to_owned(), Some(offset));
            }

            Ok(var_details.loc.clone())
        } else {
            println!("here");
            Err(RustCcError::ScopeError(ScopeError::Undeclared(
                var.to_owned(),
            )))
        }
    }

    pub fn get_var(&mut self, var: &str) -> RustCcResult<VarDetails> {
        let Some(last) = self.var_maps.last() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        if let Some(details) = last.get(var) {
            match details.state {
                VarState::Param
                | VarState::InitializedInThisScope
                | VarState::InitializedInOuterScope => Ok(details.to_owned()),
                VarState::DeclaredInThisScope | VarState::DeclaredInOuterScope => Err(
                    RustCcError::ScopeError(ScopeError::Uninitialized(var.to_owned())),
                ),
            }
        } else {
            Err(RustCcError::ScopeError(ScopeError::Undeclared(
                var.to_owned(),
            )))
        }
    }

    pub fn new_scope(&mut self) -> RustCcResult<()> {
        let Some(last) = self.var_maps.last() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        let mut new_map = last.clone();
        for details in new_map.values_mut() {
            match details.state {
                VarState::InitializedInThisScope => {
                    details.state = VarState::InitializedInOuterScope
                }
                VarState::DeclaredInThisScope => details.state = VarState::DeclaredInOuterScope,
                _ => {}
            }
        }
        self.var_maps.push(new_map);
        Ok(())
    }

    // fn store_global_var_locally(&mut self, identifier: &str, offset: usize) -> RustCcResult<()> {
    //     let Some(last) = self.var_maps.last_mut() else {
    //         return Err(RustCcError::ScopeError(ScopeError::NoScope));
    //     };

    //     if let Some(var_details) = last.get_mut(identifier) {
    //         let VarLoc::Global(..) = var_details.loc else {
    //             panic!("var loc isn't global");
    //         };
    //         var_details.loc = VarLoc::CurrFrame(offset);
    //         return Ok(());
    //     }

    //     panic!("add a better error here for if the global var doesn't exist")
    // }

    pub fn exit_scope(&mut self) -> RustCcResult<(usize, Vec<(String, usize)>)> {
        let mut num_initialized_in_scope = 0;
        match self.var_maps.pop() {
            Some(vm) => {
                let mut globals = vec![];
                for (var, details) in vm {
                    if details.state == VarState::InitializedInThisScope {
                        num_initialized_in_scope += 1;
                    }

                    if let VarLoc::Global(id, Some(offset)) = details.loc {
                        globals.push((id, offset));
                    }
                }
                Ok((num_initialized_in_scope, globals))
            }
            None => todo!("add an error for exiting scope when we've already exited all scopes"),
        }
    }

    // PERF: remove clones?
    pub fn get_globals(&self) -> RustCcResult<Vec<(String, VarDetails)>> {
        let Some(last) = self.var_maps.last() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        let mut globals = vec![];
        for (id, details) in last {
            if let VarLoc::Global(..) = &details.loc {
                globals.push((id.to_owned(), details.to_owned()));
            }
        }

        Ok(globals)
    }
}
