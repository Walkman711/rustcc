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

impl VarState {
    pub fn new_scope_state(&self) -> Self {
        match self {
            VarState::Param => VarState::Param,
            VarState::InitializedInThisScope | VarState::InitializedInOuterScope => {
                VarState::InitializedInOuterScope
            }
            VarState::DeclaredInThisScope | VarState::DeclaredInOuterScope => {
                VarState::DeclaredInOuterScope
            }
            VarState::GlobalDeclared => VarState::GlobalDeclared,
            VarState::GlobalInitialized => VarState::GlobalInitialized,
        }
    }

    pub fn validate_initialization(&self, new_state: VarState, var: &str) -> RustCcResult<()> {
        match self {
            VarState::Param => Err(RustCcError::ScopeError(
                ScopeError::ReusedParamNameInFunction(var.to_owned()),
            )),
            VarState::GlobalInitialized => {
                if new_state == VarState::GlobalInitialized {
                    Err(RustCcError::ScopeError(
                        ScopeError::InitializedTwiceInSameScope(var.to_owned()),
                    ))
                } else {
                    Ok(())
                }
            }
            VarState::InitializedInThisScope => Err(RustCcError::ScopeError(
                ScopeError::InitializedTwiceInSameScope(var.to_owned()),
            )),
            VarState::InitializedInOuterScope
            | VarState::DeclaredInThisScope
            | VarState::DeclaredInOuterScope
            | VarState::GlobalDeclared => Ok(()),
        }
    }

    pub fn has_value(&self) -> bool {
        match self {
            VarState::Param
            | VarState::InitializedInThisScope
            | VarState::InitializedInOuterScope
            | VarState::GlobalInitialized
            | VarState::GlobalDeclared => true,
            VarState::DeclaredInThisScope | VarState::DeclaredInOuterScope => false,
        }
    }
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
            return Err(RustCcError::ScopeError(
                ScopeError::ReusedParamNameInFunctionPrototype(var.to_owned()),
            ));
        }

        Ok(())
    }

    pub fn initialize_var(&mut self, var: &str, loc: VarLoc) -> RustCcResult<()> {
        let Some(last) = self.var_maps.last_mut() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        let new_state = if let VarLoc::Global(..) = loc {
            VarState::GlobalInitialized
        } else {
            VarState::InitializedInThisScope
        };

        let details = VarDetails {
            state: new_state,
            loc,
        };

        if let Some(VarDetails { state, .. }) = last.insert(var.to_owned(), details) {
            state.validate_initialization(new_state, var)?;
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

    pub fn assign_var(&mut self, var: &str, _offset: usize) -> RustCcResult<VarLoc> {
        let Some(last) = self.var_maps.last_mut() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        if let Some(var_details) = last.get_mut(var) {
            match var_details.state {
                VarState::DeclaredInThisScope => {
                    var_details.state = VarState::InitializedInThisScope
                }
                VarState::DeclaredInOuterScope => {
                    var_details.state = VarState::InitializedInOuterScope
                }
                VarState::InitializedInThisScope
                | VarState::InitializedInOuterScope
                | VarState::GlobalInitialized
                | VarState::GlobalDeclared
                | VarState::Param => {}
            }

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
            if details.state.has_value() {
                Ok(details.to_owned())
            } else {
                Err(RustCcError::ScopeError(ScopeError::Uninitialized(
                    var.to_owned(),
                )))
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
            details.state = details.state.new_scope_state();
        }
        self.var_maps.push(new_map);
        Ok(())
    }

    pub fn exit_scope(&mut self) -> RustCcResult<(usize, Vec<VarLoc>)> {
        let mut num_initialized_in_scope = 0;
        match self.var_maps.pop() {
            Some(vm) => {
                let mut globals = vec![];
                for (_var, details) in vm {
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
