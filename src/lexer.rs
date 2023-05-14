use crate::utils::{ParseError, RustCcError, RustCcResult};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    // Stage 1
    Keyword(Keywords),
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,
    Integer(u64),
    Identifier(String),
    // Stage 2 (Unary ops)
    Minus,
    ExclamationPoint,
    Tilde,
    // Stage 3 (Binary ops)
    Plus,
    Star,
    Slash,
    // Stage 4 (Logical binary ops)
    And,
    Or,
    Equals,
    NotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Keywords {
    Int,
    Return,
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

        let chars_to_expand = vec![
            ';', '-', '!', '~', '(', ')', '{', '}', '&', '|', '<', '=', '>',
        ];
        let mut expanded_program = program.to_string();
        for c in chars_to_expand {
            expanded_program = expanded_program.replace(c, &format!(" {c} "));
        }

        let mut token_iter = expanded_program.split_whitespace().peekable();
        while let Some(token) = token_iter.next() {
            let lexed_token = match token {
                "int" => Token::Keyword(Keywords::Int),
                "return" => Token::Keyword(Keywords::Return),
                "{" => Token::OpenBrace,
                "}" => Token::CloseBrace,
                "(" => Token::OpenParen,
                ")" => Token::CloseParen,
                ";" => Token::Semicolon,
                "-" => Token::Minus,
                "!" => {
                    if let Some(&"=") = token_iter.peek() {
                        let _discard_equals = token_iter.next();
                        Token::NotEquals
                    } else {
                        Token::ExclamationPoint
                    }
                }
                "~" => Token::Tilde,
                "+" => Token::Plus,
                "*" => Token::Star,
                "/" => Token::Slash,
                "&" => {
                    if let Some("&") = token_iter.next() {
                        Token::And
                    } else {
                        unimplemented!("Implement bitwise AND")
                    }
                }
                "|" => {
                    if let Some("|") = token_iter.next() {
                        Token::Or
                    } else {
                        unimplemented!("Implement bitwise OR")
                    }
                }
                "=" => {
                    if let Some(&"=") = token_iter.peek() {
                        let _discard_equals = token_iter.next();
                        Token::Equals
                    } else {
                        unimplemented!("Implement assignment")
                    }
                }
                "<" => {
                    if let Some(&"=") = token_iter.peek() {
                        let _discard_equals = token_iter.next();
                        Token::LessThanEquals
                    } else {
                        Token::LessThan
                    }
                }
                ">" => {
                    if let Some(&"=") = token_iter.peek() {
                        let _discard_equals = token_iter.next();
                        Token::GreaterThanEquals
                    } else {
                        Token::GreaterThan
                    }
                }
                s => {
                    if let Ok(u) = s.parse::<u64>() {
                        Token::Integer(u)
                    } else {
                        Token::Identifier(s.to_string())
                    }
                }
            };
            tokens.push(lexed_token);
        }

        Ok(Self {
            tokens,
            curr_idx: 0,
        })
    }
}

impl Lexer {
    pub fn next_token(&mut self) -> Option<Token> {
        let tok = self.tokens.get(self.curr_idx);
        self.curr_idx += 1;
        tok.map(|t| t.to_owned())
    }

    pub fn peek(&self) -> Option<Token> {
        let tok = self.tokens.get(self.curr_idx);
        tok.map(|t| t.to_owned())
    }

    pub fn back(&mut self) {
        self.curr_idx -= 1;
    }

    pub fn expect_next(&mut self, expected: &Token) -> RustCcResult<()> {
        match self.next_token() {
            Some(ref actual) => {
                if actual == expected {
                    Ok(())
                } else {
                    Err(RustCcError::ParseError(ParseError::ExpectedToken(
                        expected.to_owned(),
                        actual.to_owned(),
                    )))
                }
            }
            None => Err(RustCcError::ParseError(ParseError::UnexpectedTokenEnd)),
        }
    }
}
