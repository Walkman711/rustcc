use crate::utils::RustCcError;

#[derive(Clone, Debug)]
pub enum Token {
    Keyword(Keywords),
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,
    Integer(u64),
    Minus,
    ExclamationPoint,
    Tilde,
    Identifier(String),
}

#[derive(Clone, Copy, Debug)]
pub enum Keywords {
    Int,
    Return,
}

impl TryFrom<&str> for Token {
    type Error = RustCcError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "{" => Ok(Token::OpenBrace),
            "}" => Ok(Token::CloseBrace),
            "(" => Ok(Token::OpenParen),
            ")" => Ok(Token::CloseParen),
            ";" => Ok(Token::Semicolon),
            "int" => Ok(Token::Keyword(Keywords::Int)),
            "return" => Ok(Token::Keyword(Keywords::Return)),
            "-" => Ok(Token::Minus),
            "!" => Ok(Token::ExclamationPoint),
            "~" => Ok(Token::Tilde),
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

#[derive(Clone, Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
    curr_idx: usize,
}

impl TryFrom<&str> for Lexer {
    type Error = RustCcError;

    fn try_from(program: &str) -> Result<Self, Self::Error> {
        let mut tokens = vec![];

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
            tokens.push(lexed_token);
        }

        Ok(Self {
            tokens,
            curr_idx: 0,
        })
    }
}

impl Lexer {
    pub fn get_token(&mut self) -> Option<Token> {
        let tok = self.tokens.get(self.curr_idx);
        self.curr_idx += 1;
        tok.map(|t| t.to_owned())
    }
}
