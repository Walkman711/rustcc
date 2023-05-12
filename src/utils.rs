use crate::lexer::Token;

#[derive(Debug)]
pub enum RustCcError {
    LexError(String),
    ParseError(Token, Option<Token>),
}

pub type RustCcResult<T> = Result<T, RustCcError>;
