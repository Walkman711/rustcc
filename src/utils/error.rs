use crate::lexer::lexer_enums::Token;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum RustCcError {
    #[error("Bad lex {0}")]
    LexError(String),
    #[error("Parse Error: {0}")]
    ParseError(#[from] ParseError),
    #[error("Scope Error: {0}")]
    ScopeError(#[from] ScopeError),
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
    #[error("No longer in any scope")]
    NoScope,
}

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Break statement is not enclosed in an iterating statement")]
    UnenclosedBreak,
    #[error("Continue statement is not enclosed in an iterating statement")]
    UnenclosedContinue,
}

pub type RustCcResult<T> = Result<T, RustCcError>;
