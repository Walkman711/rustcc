use std::collections::HashMap;

use super::{
    error::{RustCcResult, ScopeError},
    types::{ReturnType, VariableType},
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VarState {
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
            VarState::Param => Err(ScopeError::ReusedParamNameInFunction(var.to_owned()).into()),
            VarState::GlobalInitialized => {
                if new_state == VarState::GlobalInitialized {
                    Err(ScopeError::InitializedTwiceInSameScope(var.to_owned()).into())
                } else {
                    Ok(())
                }
            }
            VarState::InitializedInThisScope => {
                Err(ScopeError::InitializedTwiceInSameScope(var.to_owned()).into())
            }
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
    pub var_type: VariableType,
    pub state: VarState,
    pub loc: VarLoc,
}

#[derive(Clone, Debug, Default)]
pub struct Scope {
    // var_name -> loc
    var_map: HashMap<String, VarDetails>,
    // Label in c -> our internal asm representation
    labels: HashMap<String, String>,
}

#[derive(Clone, Debug)]
pub struct ScopedMap {
    scopes: Vec<Scope>,
}

impl Default for ScopedMap {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }
}

impl ScopedMap {
    pub fn new_label(&mut self, c_label: &str, asm_repr: &str) -> RustCcResult<()> {
        let Some(last) = self.scopes.last_mut() else {
            return Err(ScopeError::NoScope.into());
        };

        if let Some(prev_asm_repr) = last.labels.insert(c_label.to_owned(), asm_repr.to_owned()) {
            todo!("make an error for labels that already existed in the scope. Can't define em twice! \n `{prev_asm_repr}`");
        }

        Ok(())
    }

    pub fn get_label(&mut self, c_label: &str) -> RustCcResult<String> {
        let Some(last) = self.scopes.last_mut() else {
            return Err(ScopeError::NoScope.into());
        };

        Ok(last.labels.get(c_label).unwrap().to_owned())
    }
}

impl ScopedMap {
    pub fn new_param(&mut self, id: &str, var_type: VariableType, loc: VarLoc) -> RustCcResult<()> {
        let Some(last) = self.scopes.last_mut() else {
            return Err(ScopeError::NoScope.into());
        };

        let details = VarDetails {
            var_type,
            state: VarState::Param,
            loc,
        };

        if let Some(VarDetails {
            state: VarState::Param,
            ..
        }) = last.var_map.insert(id.to_owned(), details)
        {
            return Err(ScopeError::ReusedParamNameInFunctionPrototype(id.to_owned()).into());
        }

        Ok(())
    }

    pub fn initialize_var(
        &mut self,
        var: &str,
        var_type: VariableType,
        rh_type: ReturnType,
        loc: VarLoc,
    ) -> RustCcResult<()> {
        let Some(last) = self.scopes.last_mut() else {
            return Err(ScopeError::NoScope.into());
        };

        let ReturnType::NonVoid(_rh) = rh_type else {
            panic!("tried to assign void to a variable: {rh_type:?}");
        };

        let new_state = if let VarLoc::Global(..) = loc {
            VarState::GlobalInitialized
        } else {
            VarState::InitializedInThisScope
        };

        let details = VarDetails {
            var_type,
            state: new_state,
            loc,
        };

        if let Some(VarDetails { state, .. }) = last.var_map.insert(var.to_owned(), details) {
            state.validate_initialization(new_state, var)?;
        }

        Ok(())
    }

    pub fn declare_var(
        &mut self,
        var: &str,
        var_type: VariableType,
        loc: VarLoc,
    ) -> RustCcResult<()> {
        let Some(last) = self.scopes.last_mut() else {
            return Err(ScopeError::NoScope.into());
        };

        let state = if let VarLoc::Global(..) = loc {
            VarState::GlobalDeclared
        } else {
            VarState::DeclaredInThisScope
        };

        let details = VarDetails {
            var_type,
            state,
            loc,
        };

        if let Some(VarDetails {
            state: VarState::DeclaredInThisScope,
            ..
        }) = last.var_map.insert(var.to_owned(), details)
        {
            return Err(ScopeError::DeclaredTwiceInSameScope(var.to_owned()).into());
        }

        Ok(())
    }

    // TODO: validate that the RH value == var_type of LH
    pub fn assign_var(&mut self, var: &str, rh_type: ReturnType) -> RustCcResult<VarLoc> {
        let Some(last) = self.scopes.last_mut() else {
            return Err(ScopeError::NoScope.into());
        };

        let ReturnType::NonVoid(vt) = rh_type else {
            panic!("tried to assign void to a variable");
        };

        if let Some(var_details) = last.var_map.get_mut(var) {
            assert_eq!(var_details.var_type, vt);
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
            Err(ScopeError::Undeclared(var.to_owned()).into())
        }
    }

    pub fn get_var(&self, var: &str) -> RustCcResult<VarDetails> {
        let Some(last) = self.scopes.last() else {
            return Err(ScopeError::NoScope.into());
        };

        if let Some(details) = last.var_map.get(var) {
            if details.state.has_value() {
                Ok(details.to_owned())
            } else {
                Err(ScopeError::Uninitialized(var.to_owned()).into())
            }
        } else {
            Err(ScopeError::Undeclared(var.to_owned()).into())
        }
    }

    pub fn new_scope(&mut self) -> RustCcResult<()> {
        let Some(last) = self.scopes.last() else {
            return Err(ScopeError::NoScope.into());
        };

        let mut new_scope = last.clone();
        for details in new_scope.var_map.values_mut() {
            details.state = details.state.new_scope_state();
        }
        self.scopes.push(new_scope);
        Ok(())
    }

    pub fn exit_scope(&mut self) -> RustCcResult<usize> {
        let mut num_initialized_in_scope = 0;
        match self.scopes.pop() {
            Some(scope) => {
                for (_var, details) in scope.var_map {
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
