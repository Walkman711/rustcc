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
    LogicalAnd,
    LogicalOr,
    DoubleEquals,
    NotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    // Stage 5 (Local variables)
    SingleEquals,
    // Tons of remaining ops
    Increment,
    Decrement,
    PercentSign,
    BitwiseLeftShift,
    BitwiseRightShift,
    SingleAnd,
    BitwiseXor,
    BitwiseOr,
    QuestionMark,
    Colon,
    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    PercentEquals,
    BitwiseLeftShiftEquals,
    BitwiseRightShiftEquals,
    BitwiseAndEquals,
    BitwiseXorEquals,
    BitwiseOrEquals,
    Comma,
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
            ';', '-', '!', '~', '(', ')', '{', '}', '&', '|', '<', '=', '>', '?', ':', '%',
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
