#![allow(dead_code)]

use std::collections::HashMap;

use crate::lexer_enums::Token;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum RustCcError {
    #[error("Bad lex {0}")]
    LexError(String),
    #[error("Parse Error: {0}")]
    ParseError(#[from] ParseError),
    #[error("Scope Error: {0}")]
    ScopeError(#[from] ScopeError),
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Expected to parse {0:?}, but found {1:?}")]
    ExpectedToken(Token, Token),
    #[error("Ran out of tokens")]
    UnexpectedTokenEnd,
    #[error("Next token didn't parse into op")]
    PeekFailed,
}

#[derive(Debug, Error)]
pub enum ScopeError {
    #[error("Initialized `{0}` twice in the same scope")]
    InitializedTwiceInSameScope(String),
    #[error("Declared `{0}` twice in the same scope")]
    DeclaredTwiceInSameScope(String),
    #[error("`{0}` is not initialized in this scope")]
    Uninitialized(String),
    #[error("`{0}` is not declared in this scope")]
    Undeclared(String),
}

pub type RustCcResult<T> = Result<T, RustCcError>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VarState {
    InitializedInThisScope,
    InitializedInOuterScope,
    DeclaredInThisScope,
    DeclaredInOuterScope,
}

#[derive(Clone, Debug)]
pub struct VarDetails {
    pub state: VarState,
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
        if let Some(VarDetails {
            state: VarState::InitializedInThisScope,
            ..
        }) = self.var_maps.last_mut().unwrap().insert(
            var.to_owned(),
            VarDetails {
                state: VarState::InitializedInThisScope,
                stack_offset,
            },
        ) {
            Err(RustCcError::ScopeError(
                ScopeError::InitializedTwiceInSameScope(var.to_owned()),
            ))
        } else {
            Ok(())
        }
    }

    pub fn declare_var(&mut self, var: &str, stack_offset: usize) -> RustCcResult<()> {
        if let Some(VarDetails {
            state: VarState::DeclaredInThisScope,
            ..
        }) = self.var_maps.last_mut().unwrap().insert(
            var.to_owned(),
            VarDetails {
                state: VarState::DeclaredInThisScope,
                stack_offset,
            },
        ) {
            Err(RustCcError::ScopeError(
                ScopeError::DeclaredTwiceInSameScope(var.to_owned()),
            ))
        } else {
            Ok(())
        }
    }

    pub fn assign_var(&mut self, var: &str) -> RustCcResult<usize> {
        let var_details = self.var_maps.last_mut().unwrap().get_mut(var).unwrap();
        match var_details.state {
            VarState::DeclaredInThisScope => var_details.state = VarState::InitializedInThisScope,
            VarState::DeclaredInOuterScope => var_details.state = VarState::InitializedInOuterScope,
            _ => {}
        }
        Ok(var_details.stack_offset)
    }

    pub fn get_var(&mut self, var: &str) -> RustCcResult<VarDetails> {
        if let Some(details) = self.var_maps.last().unwrap().get(var) {
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
        let mut new_map = self.var_maps.last().unwrap().clone();
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
