use crate::utils::RustCcError;

#[derive(Clone, Debug)]
pub enum Token {
    Keyword(Keywords),
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    Semicolon,
    Integer(u64),
    Negation,
    LogicalNegation,
    BitwiseComplement,
    Identifier(String),
}

#[derive(Clone, Copy, Debug)]
pub enum Keywords {
    Int,
    // TODO: get rid of this
    Main,
    Return,
}

impl TryFrom<&str> for Token {
    type Error = RustCcError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "{" => Ok(Token::OpenBracket),
            "}" => Ok(Token::CloseBracket),
            "(" => Ok(Token::OpenParen),
            ")" => Ok(Token::CloseParen),
            ";" => Ok(Token::Semicolon),
            "int" => Ok(Token::Keyword(Keywords::Int)),
            "return" => Ok(Token::Keyword(Keywords::Return)),
            "-" => Ok(Token::Negation),
            "!" => Ok(Token::LogicalNegation),
            "~" => Ok(Token::BitwiseComplement),
            _ => {
                if let Ok(u) = value.parse::<u64>() {
                    Ok(Token::Integer(u))
                } else {
                    Ok(Token::Identifier(value.to_string()))
                }
            }
        }
    }
}

pub fn lex(program: &str) -> Vec<Token> {
    let mut lexed_tokens = vec![];

    let chars_to_expand = vec![';', '-', '!', '~', '(', ')', '{', '}'];

    // TODO: make list of special chars
    let program_with_whitespace = program
        .replace(';', " ; ")
        .replace('-', " - ")
        .replace('!', " ! ")
        .replace('~', " ~ ")
        .replace('(', " ( ")
        .replace(')', " ) ")
        .replace('{', " { ")
        .replace('}', " } ");

    for token in program_with_whitespace.split_whitespace() {
        let lexed_token = Token::try_from(token).unwrap();
        lexed_tokens.push(lexed_token);
    }

    lexed_tokens
}
