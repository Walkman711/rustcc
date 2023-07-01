use crate::{lexing::lexer_enums::Token, parsing::parser_types::Param};

use thiserror::Error;

use super::types::ReturnType;

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
    #[error("IO Error: {0}")]
    IoError(#[from] std::io::Error),
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Expected to parse {0:?}, but found {1:?}")]
    ExpectedToken(Token, Token),
    #[error("Ran out of tokens")]
    UnexpectedTokenEnd,
    #[error("Next token didn't parse into op")]
    PeekFailed,
    #[error("Keyword not followed by an identifier")]
    MalformedDeclaration,
    #[error("`{0:?}` is not a valid L2 token")]
    UnexpectedBottomLevelToken(Token),
    #[error("Could not parse basic type")]
    CouldNotParseBasicType,
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
    #[error("There is no main function")]
    NoMain,
    #[error(
        "Bad arguments to main. Should either have no arguments or `int argc, char *argv[]`: {0:?}"
    )]
    BadArgumentsToMain(Vec<Param>),
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
    #[error("Function `{0}` is supposed to return `{1:?}` but it is returning `{2:?}")]
    ReturningIncorrectType(String, ReturnType, ReturnType),
}

pub type RustCcResult<T> = Result<T, RustCcError>;
