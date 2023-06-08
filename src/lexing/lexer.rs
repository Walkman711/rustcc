use crate::utils::error::{ParseError, RustCcError, RustCcResult};

use super::lexer_enums::{Keywords, Token};

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
            ';', '-', '!', '~', '(', ')', '{', '}', '&', '|', '<', '=', '>', '?', ':', '%', '*',
            '+', ',',
        ];
        let mut expanded_program = program.to_string();
        for c in chars_to_expand {
            expanded_program = expanded_program.replace(c, &format!(" {c} "));
        }

        let mut token_iter = expanded_program.split_whitespace().peekable();
        while let Some(token) = token_iter.next() {
            let lexed_token = match token {
                "{" => Token::OpenBrace,
                "}" => Token::CloseBrace,
                "(" => Token::OpenParen,
                ")" => Token::CloseParen,
                ";" => Token::Semicolon,
                "-" => match token_iter.peek() {
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::MinusEquals
                    }
                    Some(&">") => {
                        let _ = token_iter.next();
                        Token::Arrow
                    }
                    _ => Token::Minus,
                },
                "!" => match token_iter.peek() {
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::NotEquals
                    }
                    _ => Token::ExclamationPoint,
                },
                "~" => Token::Tilde,
                "+" => match token_iter.peek() {
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::PlusEquals
                    }
                    Some(&"+") => {
                        let _ = token_iter.next();
                        Token::Increment
                    }
                    _ => Token::Plus,
                },
                "*" => match token_iter.peek() {
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::StarEquals
                    }
                    _ => Token::Star,
                },
                "/" => match token_iter.peek() {
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::SlashEquals
                    }
                    _ => Token::Slash,
                },
                "&" => match token_iter.peek() {
                    Some(&"&") => {
                        let _ = token_iter.next();
                        Token::LogicalAnd
                    }
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::BitwiseAndEquals
                    }
                    _ => Token::SingleAnd,
                },
                "|" => match token_iter.peek() {
                    Some(&"|") => {
                        let _ = token_iter.next();
                        Token::LogicalOr
                    }
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::BitwiseOrEquals
                    }
                    _ => Token::BitwiseOr,
                },
                "^" => match token_iter.peek() {
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::BitwiseXorEquals
                    }
                    _ => Token::BitwiseXor,
                },
                "=" => match token_iter.peek() {
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::DoubleEquals
                    }
                    _ => Token::SingleEquals,
                },
                "<" => match token_iter.peek() {
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::LessThanEquals
                    }
                    Some(&"<") => {
                        let _ = token_iter.next();
                        match token_iter.peek() {
                            Some(&"=") => {
                                let _ = token_iter.next();
                                Token::BitwiseLeftShiftEquals
                            }
                            _ => Token::BitwiseLeftShift,
                        }
                    }
                    _ => Token::LessThan,
                },
                ">" => match token_iter.peek() {
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::GreaterThanEquals
                    }
                    Some(&">") => {
                        let _ = token_iter.next();
                        match token_iter.peek() {
                            Some(&"=") => {
                                let _ = token_iter.next();
                                Token::BitwiseRightShiftEquals
                            }
                            _ => Token::BitwiseRightShift,
                        }
                    }
                    _ => Token::GreaterThan,
                },
                "%" => match token_iter.peek() {
                    Some(&"=") => {
                        let _ = token_iter.next();
                        Token::PercentEquals
                    }
                    _ => Token::PercentSign,
                },
                "?" => Token::QuestionMark,
                ":" => Token::Colon,
                "," => Token::Comma,
                "." => Token::Period,
                s => {
                    if let Ok(kw) = Keywords::try_from(s) {
                        Token::Keyword(kw)
                    } else if let Ok(u) = s.parse::<u64>() {
                        Token::Integer(u)
                    } else {
                        Self::valid_identifier(s)?
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
    fn valid_identifier(s: &str) -> RustCcResult<Token> {
        for (i, c) in s.chars().enumerate() {
            if i == 0 {
                if !c.is_alphabetic() && c != '_' {
                    return Err(RustCcError::LexError(format!("Malformed identifier: {s}. C identifiers can only contain a-z, A-Z, 0-9, and _.")));
                }
            } else {
                if !c.is_alphanumeric() && c != '_' {
                    return Err(RustCcError::LexError(format!("Malformed identifier: {s}. C identifiers can only contain a-z, A-Z, 0-9, and _.")));
                }
            }
        }

        Ok(Token::Identifier(s.to_owned()))
    }
}

impl Lexer {
    pub fn next_token(&mut self) -> Option<Token> {
        let tok = self.tokens.get(self.curr_idx);
        self.curr_idx += 1;
        tok.map(|t| t.to_owned())
    }

    pub fn next_token_fallible(&mut self) -> RustCcResult<Token> {
        self.next_token()
            .ok_or(ParseError::UnexpectedTokenEnd.into())
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
                    Err(ParseError::ExpectedToken(expected.to_owned(), actual.to_owned()).into())
                }
            }
            None => Err(ParseError::UnexpectedTokenEnd.into()),
        }
    }

    pub fn advance_if_match(&mut self, tok: &Token) -> bool {
        match self.peek() {
            Some(pk) => {
                if pk == *tok {
                    let _ = self.next_token();
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }
}
