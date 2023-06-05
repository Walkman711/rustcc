use crate::lexing::lexer_enums::Token;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum RustCcError {
    #[error("Bad lex {0}")]
    LexError(String),
    #[error("Parse Error: {0}")]
    ParseError(#[from] ParseError),
    #[error("Scope Error: {0}")]
    ScopeError(#[from] ScopeError),
    #[error("Function Error: {0}")]
    FunctionError(#[from] FunctionError),
    #[error("Codegen Error: {0}")]
    CodegenError(#[from] CodegenError),
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Expected to parse {0:?}, but found {1:?}")]
    ExpectedToken(Token, Token),
    #[error("Ran out of tokens")]
    UnexpectedTokenEnd,
    #[error("Next token didn't parse into op")]
    PeekFailed,
    #[error("Int keyword not followed by an identifier")]
    MalformedDeclaration,
    #[error("`{0:?}` is not a valid L2 token")]
    UnexpectedBottomLevelToken(Token),
}

#[derive(Debug, Error)]
pub enum ScopeError {
    #[error("Initialized `{0}` twice in the same scope")]
    InitializedTwiceInSameScope(String),
    #[error("Tried to use `{0}` as a variable name in a function, but it's already being used as a param name")]
    ReusedParamNameInFunction(String),
    #[error("Tried to use `{0}` twice as a parameter name")]
    ReusedParamNameInFunctionPrototype(String),
    #[error("Declared `{0}` twice in the same scope")]
    DeclaredTwiceInSameScope(String),
    #[error("`{0}` is not initialized in this scope")]
    Uninitialized(String),
    #[error("`{0}` is not declared in this scope")]
    Undeclared(String),
    #[error("No longer in any scope")]
    NoScope,
}

#[derive(Debug, Error)]
pub enum FunctionError {
    #[error("Expected {0} arguments for function call, but passed in {1}")]
    ArgumentMismatch(usize, usize),
    #[error("Defined function {0} multiple times in program")]
    MultipleDefinitions(String),
    #[error("There is no declaration or definition for {0}")]
    UndeclaredFunction(String),
}

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Break statement is not enclosed in an iterating statement")]
    UnenclosedBreak,
    #[error("Continue statement is not enclosed in an iterating statement")]
    UnenclosedContinue,
    #[error("Cannot assign to a variable in a previous stack frame")]
    AssignedToVarInPrevFrame,
    #[error("Cannot assign to a variable in register. Try storing to stack first.")]
    AssignedToVarInRegister,
    #[error("`{0}` used for both function and global variable name")]
    ReusedIdentifierForFunctionAndGlobal(String),
}

pub type RustCcResult<T> = Result<T, RustCcError>;
