use std::collections::HashMap;

use super::error::{RustCcError, RustCcResult, ScopeError};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum VarState {
    InitializedInThisScope,
    InitializedInOuterScope,
    DeclaredInThisScope,
    DeclaredInOuterScope,
}

#[derive(Clone, Debug)]
pub struct VarDetails {
    state: VarState,
    pub stack_offset: usize,
}

#[derive(Clone, Debug)]
pub struct ScopedMap {
    // var_name -> stack_offset
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
    pub fn initialize_var(&mut self, var: &str, stack_offset: usize) -> RustCcResult<()> {
        let Some(last) = self.var_maps.last_mut() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        let details = VarDetails {
            state: VarState::InitializedInThisScope,
            stack_offset,
        };

        if let Some(VarDetails {
            state: VarState::InitializedInThisScope,
            ..
        }) = last.insert(var.to_owned(), details)
        {
            Err(RustCcError::ScopeError(
                ScopeError::InitializedTwiceInSameScope(var.to_owned()),
            ))?;
        }

        Ok(())
    }

    pub fn declare_var(&mut self, var: &str, stack_offset: usize) -> RustCcResult<()> {
        let Some(last) = self.var_maps.last_mut() else {
            return Err(RustCcError::ScopeError(ScopeError::NoScope));
        };

        let details = VarDetails {
            state: VarState::DeclaredInThisScope,
            stack_offset,
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

    pub fn assign_var(&mut self, var: &str) -> RustCcResult<usize> {
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
                _ => {}
            }
            Ok(var_details.stack_offset)
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
                VarState::InitializedInThisScope | VarState::InitializedInOuterScope => {
                    Ok(details.to_owned())
                }
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

    pub fn exit_scope(&mut self) -> RustCcResult<usize> {
        let mut num_initialized_in_scope = 0;
        match self.var_maps.pop() {
            Some(vm) => {
                for (_var, details) in vm {
                    if details.state == VarState::InitializedInThisScope {
                        num_initialized_in_scope += 1;
                    }
                }
                Ok(num_initialized_in_scope)
            }
            None => todo!("add an error for exiting scope when we've already exited all scopes"),
        }
    }
}
