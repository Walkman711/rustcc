use crate::{
    lexer::Lexer,
    lexer_enums::{Keywords, Token},
    parser_precedence::*,
    utils::{ParseError, RustCcError, RustCcResult},
};

pub struct ParserII {
    lexer: Lexer,
}

#[derive(Clone, Debug)]
pub enum ProgramII {
    Func(FunctionII),
}

#[derive(Clone, Debug)]
pub enum FunctionII {
    Fun(String, StatementII),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum StatementII {
    Return(Level15Exp),
}

impl ParserII {
    pub fn new(program: &str) -> RustCcResult<Self> {
        let lexer = Lexer::try_from(program)?;
        Ok(Self { lexer })
    }
}

impl ParserII {
    pub fn parse(&mut self) -> RustCcResult<ProgramII> {
        let function = self.parse_fn()?;
        Ok(ProgramII::Func(function))
    }

    fn parse_fn(&mut self) -> RustCcResult<FunctionII> {
        self.lexer.expect_next(&Token::Keyword(Keywords::Int))?;

        let tok = self
            .lexer
            .next_token()
            .ok_or(RustCcError::ParseError(ParseError::UnexpectedTokenEnd))?;

        let Token::Identifier(identifier) = tok else {
            return Err(RustCcError::ParseError(
                ParseError::ExpectedToken(
                    Token::Identifier("any function name".to_string()), tok)));
        };

        self.lexer.expect_next(&Token::OpenParen)?;
        self.lexer.expect_next(&Token::CloseParen)?;
        self.lexer.expect_next(&Token::OpenBrace)?;

        let statement = self.parse_statement()?;

        Ok(FunctionII::Fun(identifier, statement))
    }

    fn parse_statement(&mut self) -> RustCcResult<StatementII> {
        self.lexer.expect_next(&Token::Keyword(Keywords::Return))?;

        let exp = self.parse_level_15_exp()?;

        self.lexer.expect_next(&Token::Semicolon)?;
        self.lexer.expect_next(&Token::CloseBrace)?;

        Ok(StatementII::Return(exp))
    }

    fn parse_level_15_exp(&mut self) -> RustCcResult<Level15Exp> {
        let first_level_14_exp = self.parse_level_14_exp()?;
        let mut trailing_level_14_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level15Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_14_exp = self.parse_level_14_exp()?;
            trailing_level_14_exps.push((op, level_14_exp));
        }
        Ok((first_level_14_exp, trailing_level_14_exps))
    }

    fn parse_level_14_exp(&mut self) -> RustCcResult<Level14Exp> {
        let first_level_13_exp = self.parse_level_13_exp()?;
        let mut trailing_level_13_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level14Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_13_exp = self.parse_level_13_exp()?;
            trailing_level_13_exps.push((op, level_13_exp));
        }
        Ok((first_level_13_exp, trailing_level_13_exps))
    }

    fn parse_level_13_exp(&mut self) -> RustCcResult<Level13Exp> {
        let first_level_12_exp = self.parse_level_12_exp()?;
        let mut trailing_level_12_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level13Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_12_exp = self.parse_level_12_exp()?;
            trailing_level_12_exps.push((op, level_12_exp));
        }
        Ok((first_level_12_exp, trailing_level_12_exps))
    }

    fn parse_level_12_exp(&mut self) -> RustCcResult<Level12Exp> {
        let first_level_11_exp = self.parse_level_11_exp()?;
        let mut trailing_level_11_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level12Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_11_exp = self.parse_level_11_exp()?;
            trailing_level_11_exps.push((op, level_11_exp));
        }
        Ok((first_level_11_exp, trailing_level_11_exps))
    }

    fn parse_level_11_exp(&mut self) -> RustCcResult<Level11Exp> {
        let first_level_10_exp = self.parse_level_10_exp()?;
        let mut trailing_level_10_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level11Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_10_exp = self.parse_level_10_exp()?;
            trailing_level_10_exps.push((op, level_10_exp));
        }
        Ok((first_level_10_exp, trailing_level_10_exps))
    }

    fn parse_level_10_exp(&mut self) -> RustCcResult<Level10Exp> {
        let first_level_9_exp = self.parse_level_9_exp()?;
        let mut trailing_level_9_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level10Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_9_exp = self.parse_level_9_exp()?;
            trailing_level_9_exps.push((op, level_9_exp));
        }
        Ok((first_level_9_exp, trailing_level_9_exps))
    }

    fn parse_level_9_exp(&mut self) -> RustCcResult<Level9Exp> {
        let first_level_8_exp = self.parse_level_8_exp()?;
        let mut trailing_level_8_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level9Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_8_exp = self.parse_level_8_exp()?;
            trailing_level_8_exps.push((op, level_8_exp));
        }
        Ok((first_level_8_exp, trailing_level_8_exps))
    }

    fn parse_level_8_exp(&mut self) -> RustCcResult<Level8Exp> {
        let first_level_7_exp = self.parse_level_7_exp()?;
        let mut trailing_level_7_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level8Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_7_exp = self.parse_level_7_exp()?;
            trailing_level_7_exps.push((op, level_7_exp));
        }
        Ok((first_level_7_exp, trailing_level_7_exps))
    }

    fn parse_level_7_exp(&mut self) -> RustCcResult<Level7Exp> {
        let first_level_6_exp = self.parse_level_6_exp()?;
        let mut trailing_level_6_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level7Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_6_exp = self.parse_level_6_exp()?;
            trailing_level_6_exps.push((op, level_6_exp));
        }
        Ok((first_level_6_exp, trailing_level_6_exps))
    }

    fn parse_level_6_exp(&mut self) -> RustCcResult<Level6Exp> {
        let first_level_5_exp = self.parse_level_5_exp()?;
        let mut trailing_level_5_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level6Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_5_exp = self.parse_level_5_exp()?;
            trailing_level_5_exps.push((op, level_5_exp));
        }
        Ok((first_level_5_exp, trailing_level_5_exps))
    }

    fn parse_level_5_exp(&mut self) -> RustCcResult<Level5Exp> {
        let first_level_4_exp = self.parse_level_4_exp()?;
        let mut trailing_level_4_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level5Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_4_exp = self.parse_level_4_exp()?;
            trailing_level_4_exps.push((op, level_4_exp));
        }
        Ok((first_level_4_exp, trailing_level_4_exps))
    }

    fn parse_level_4_exp(&mut self) -> RustCcResult<Level4Exp> {
        let first_level_3_exp = self.parse_level_3_exp()?;
        let mut trailing_level_3_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level4Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_3_exp = self.parse_level_3_exp()?;
            trailing_level_3_exps.push((op, level_3_exp));
        }
        Ok((first_level_3_exp, trailing_level_3_exps))
    }

    fn parse_level_3_exp(&mut self) -> RustCcResult<Level3Exp> {
        let first_level_2_exp = self.parse_level_2_exp()?;
        let mut trailing_level_2_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level3Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let level_2_exp = self.parse_level_2_exp()?;
            trailing_level_2_exps.push((op, level_2_exp));
        }
        Ok((first_level_2_exp, trailing_level_2_exps))
    }

    fn parse_level_2_exp(&mut self) -> RustCcResult<Level2Exp> {
        let tok = self
            .lexer
            .next_token()
            .ok_or(RustCcError::ParseError(ParseError::UnexpectedTokenEnd))?;

        let level_2_exp = match tok {
            Token::OpenParen => {
                let f = Level2Exp::ParenExp(Box::new(self.parse_level_15_exp()?));
                self.lexer.expect_next(&Token::CloseParen)?;
                f
            }
            Token::Integer(u) => Level2Exp::Const(u),
            Token::Plus => {
                Level2Exp::Unary(Level2Op::UnaryPlus, Box::new(self.parse_level_2_exp()?))
            }
            Token::Minus => {
                Level2Exp::Unary(Level2Op::UnaryMinus, Box::new(self.parse_level_2_exp()?))
            }
            Token::ExclamationPoint => {
                Level2Exp::Unary(Level2Op::LogicalNot, Box::new(self.parse_level_2_exp()?))
            }
            Token::Tilde => {
                Level2Exp::Unary(Level2Op::BitwiseNot, Box::new(self.parse_level_2_exp()?))
            }
            _ => panic!("bad factor"),
        };

        Ok(level_2_exp)
    }
}
