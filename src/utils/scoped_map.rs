use std::collections::HashMap;

use super::error::{RustCcError, RustCcResult, ScopeError};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum VarState {
    Param,
    InitializedInThisScope,
    InitializedInOuterScope,
    DeclaredInThisScope,
    DeclaredInOuterScope,
    GlobalDeclared,
    GlobalInitialized,
}

#[derive(Clone, Debug)]
pub enum VarLoc {
    CurrFrame(usize),
    PrevFrame(usize),
    Register(usize),
    Global(String, usize),
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

        let state = if let VarLoc::Global(..) = loc {
            VarState::GlobalInitialized
        } else {
            VarState::InitializedInThisScope
        };

        let details = VarDetails { state, loc };

        if let Some(VarDetails { state, .. }) = last.insert(var.to_owned(), details) {
            match state {
                VarState::Param => {
                    Err(RustCcError::ScopeError(ScopeError::ReusedParamName(
                        var.to_owned(),
                    )))?;
                }
                VarState::GlobalInitialized | VarState::InitializedInThisScope => {
                    Err(RustCcError::ScopeError(
                        ScopeError::InitializedTwiceInSameScope(var.to_owned()),
                    ))?;
                }
                VarState::InitializedInOuterScope
                | VarState::DeclaredInThisScope
                | VarState::DeclaredInOuterScope
                | VarState::GlobalDeclared => {}
            }
        }

        Ok(())
    }

    pub fn declare_var(&mut self, var: &str, loc: VarLoc) -> RustCcResult<()> {
        let Some(last) = self.var_maps.last_mut() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        let state = if let VarLoc::Global(..) = loc {
            VarState::GlobalDeclared
        } else {
            VarState::DeclaredInThisScope
        };

        let details = VarDetails { state, loc };

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
                // TODO: is this correct?
                VarState::GlobalInitialized | VarState::GlobalDeclared => {}
            }

            // if let VarLoc::Global(id, None) = &var_details.loc {
            //     var_details.loc = VarLoc::Global(id.to_owned(), Some(offset));
            // }

            Ok(var_details.loc.clone())
        } else {
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
                | VarState::InitializedInOuterScope
                | VarState::GlobalInitialized
                | VarState::GlobalDeclared => Ok(details.to_owned()),
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
            let new_state = match details.state {
                VarState::InitializedInThisScope
                | VarState::GlobalDeclared
                | VarState::GlobalInitialized => VarState::InitializedInOuterScope,
                VarState::DeclaredInThisScope => VarState::DeclaredInOuterScope,
                VarState::Param => VarState::Param,
                VarState::InitializedInOuterScope => VarState::InitializedInOuterScope,
                VarState::DeclaredInOuterScope => VarState::DeclaredInOuterScope,
            };
            details.state = new_state;
        }
        self.var_maps.push(new_map);
        Ok(())
    }

    pub fn exit_scope(&mut self) -> RustCcResult<(usize, Vec<VarLoc>)> {
        let mut num_initialized_in_scope = 0;
        match self.var_maps.pop() {
            Some(vm) => {
                let mut globals = vec![];
                for (var, details) in vm {
                    if details.state == VarState::InitializedInThisScope {
                        num_initialized_in_scope += 1;
                    }

                    if let VarLoc::Global(..) = details.loc {
                        globals.push(details.loc);
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
