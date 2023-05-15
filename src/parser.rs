use std::collections::HashMap;

use crate::{
    lexer::Lexer,
    lexer_enums::{Keywords, Token},
    parser_precedence::*,
    utils::{ParseError, RustCcError, RustCcResult},
};

pub struct Parser {
    lexer: Lexer,
    var_map: HashMap<String, Option<u64>>,
}

#[derive(Clone, Debug)]
pub enum Program {
    Func(Function),
}

#[derive(Clone, Debug)]
pub enum Function {
    Fun(String, Vec<Statement>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Return(Level15Exp),
    Declare(String, Option<Level15Exp>),
    Exp(Level15Exp),
}

impl Parser {
    pub fn new(program: &str) -> RustCcResult<Self> {
        let lexer = Lexer::try_from(program)?;
        Ok(Self {
            lexer,
            var_map: HashMap::new(),
        })
    }
}

impl Parser {
    pub fn parse(&mut self) -> RustCcResult<Program> {
        let function = self.parse_fn()?;
        Ok(Program::Func(function))
    }

    fn parse_fn(&mut self) -> RustCcResult<Function> {
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

        // TODO: will I need to keep a weird stack of brackets once we start doing conditionals and
        // loops?
        let mut statements = vec![];
        while Some(Token::CloseBrace) != self.lexer.peek() {
            println!("XXXXXXXXX STATEMENT BEGIN XXXXXXXX");
            let statement = self.parse_statement()?;
            println!("XXXXXXXXX STATEMENT PARSED XXXXXXXX");
            dbg!(&statement);
            statements.push(statement);
        }

        self.lexer.expect_next(&Token::CloseBrace)?;
        Ok(Function::Fun(identifier, statements))
    }

    fn parse_statement(&mut self) -> RustCcResult<Statement> {
        let stmt = match self.lexer.peek() {
            Some(Token::Keyword(Keywords::Return)) => {
                let _ = self.lexer.next_token();
                let exp = self.parse_l15_exp()?;

                Ok(Statement::Return(exp))
            }
            Some(Token::Keyword(Keywords::Int)) => {
                let _ = self.lexer.next_token();
                let Some(Token::Identifier(id)) = self.lexer.next_token() else {
                    panic!("assignment statment needs an identifier")
                };

                // Declaration vs initialization
                let exp = if let Some(Token::Semicolon) = self.lexer.peek() {
                    // don't advance the lexer on a match so we can catch semicolons below
                    None
                } else {
                    // TODO: change for +=, -=, etc.
                    self.lexer.expect_next(&Token::SingleEquals)?;
                    let exp = self.parse_l15_exp()?;
                    Some(exp)
                };

                Ok(Statement::Declare(id, exp))
            }
            _ => Ok(Statement::Exp(self.parse_l15_exp()?)),
        };

        self.lexer.expect_next(&Token::Semicolon)?;

        stmt
    }

    fn parse_l15_exp(&mut self) -> RustCcResult<Level15Exp> {
        let first_l14_exp = self.parse_l14_exp()?;
        let mut trailing_l14_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level15Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l14_exp = self.parse_l14_exp()?;
            trailing_l14_exps.push((op, l14_exp));
        }
        Ok((first_l14_exp, trailing_l14_exps))
    }

    fn parse_l14_exp(&mut self) -> RustCcResult<Level14Exp> {
        // println!("l14");
        // dbg!(self.lexer.peek());
        // println!("peek");
        if let Some(Token::Identifier(var_name)) = self.lexer.peek() {
            let _ = self.lexer.next_token();
            if let Some(Token::SingleEquals) = self.lexer.peek() {
                let _ = self.lexer.next_token();
                println!("SIMPLE ASSIGNMENT WOO: {var_name}");
                let l13_exp = self.parse_l13_exp()?;
                Ok(Level14Exp::SimpleAssignment(var_name, l13_exp))
            } else {
                println!("VAR REF: {var_name}");
                Ok(Level14Exp::Var(var_name))
            }
        } else {
            let first_l13_exp = self.parse_l13_exp()?;
            // let mut trailing_l13_exps = vec![];
            // while let Some(tok) = self.lexer.next_token() {
            //     let Ok(op) = Level14Op::try_from(tok) else {
            //     self.lexer.back();
            //     break;
            // };

            //     let l13_exp = self.parse_l13_exp()?;
            //     trailing_l13_exps.push((op, l13_exp));
            // }

            // NOTE: really not sure about this one
            Ok(Level14Exp::NonAssignment(first_l13_exp))
        }
    }

    fn parse_l13_exp(&mut self) -> RustCcResult<Level13Exp> {
        let first_l12_exp = self.parse_l12_exp()?;
        let mut trailing_l12_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level13Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l12_exp = self.parse_l12_exp()?;
            trailing_l12_exps.push((op, l12_exp));
        }
        Ok((first_l12_exp, trailing_l12_exps))
    }

    fn parse_l12_exp(&mut self) -> RustCcResult<Level12Exp> {
        let first_l11_exp = self.parse_l11_exp()?;
        let mut trailing_l11_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level12Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l11_exp = self.parse_l11_exp()?;
            trailing_l11_exps.push((op, l11_exp));
        }
        Ok((first_l11_exp, trailing_l11_exps))
    }

    fn parse_l11_exp(&mut self) -> RustCcResult<Level11Exp> {
        let first_l10_exp = self.parse_l10_exp()?;
        let mut trailing_l10_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level11Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l10_exp = self.parse_l10_exp()?;
            trailing_l10_exps.push((op, l10_exp));
        }
        Ok((first_l10_exp, trailing_l10_exps))
    }

    fn parse_l10_exp(&mut self) -> RustCcResult<Level10Exp> {
        let first_l9_exp = self.parse_l9_exp()?;
        let mut trailing_l9_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level10Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l9_exp = self.parse_l9_exp()?;
            trailing_l9_exps.push((op, l9_exp));
        }
        Ok((first_l9_exp, trailing_l9_exps))
    }

    fn parse_l9_exp(&mut self) -> RustCcResult<Level9Exp> {
        let first_l8_exp = self.parse_l8_exp()?;
        let mut trailing_l8_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level9Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l8_exp = self.parse_l8_exp()?;
            trailing_l8_exps.push((op, l8_exp));
        }
        Ok((first_l8_exp, trailing_l8_exps))
    }

    fn parse_l8_exp(&mut self) -> RustCcResult<Level8Exp> {
        let first_l7_exp = self.parse_l7_exp()?;
        let mut trailing_l7_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level8Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l7_exp = self.parse_l7_exp()?;
            trailing_l7_exps.push((op, l7_exp));
        }
        Ok((first_l7_exp, trailing_l7_exps))
    }

    fn parse_l7_exp(&mut self) -> RustCcResult<Level7Exp> {
        let first_l6_exp = self.parse_l6_exp()?;
        let mut trailing_l6_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level7Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l6_exp = self.parse_l6_exp()?;
            trailing_l6_exps.push((op, l6_exp));
        }
        Ok((first_l6_exp, trailing_l6_exps))
    }

    fn parse_l6_exp(&mut self) -> RustCcResult<Level6Exp> {
        let first_l5_exp = self.parse_l5_exp()?;
        let mut trailing_l5_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level6Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l5_exp = self.parse_l5_exp()?;
            trailing_l5_exps.push((op, l5_exp));
        }
        Ok((first_l5_exp, trailing_l5_exps))
    }

    fn parse_l5_exp(&mut self) -> RustCcResult<Level5Exp> {
        let first_l4_exp = self.parse_l4_exp()?;
        let mut trailing_l4_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level5Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l4_exp = self.parse_l4_exp()?;
            trailing_l4_exps.push((op, l4_exp));
        }
        Ok((first_l4_exp, trailing_l4_exps))
    }

    fn parse_l4_exp(&mut self) -> RustCcResult<Level4Exp> {
        let first_l3_exp = self.parse_l3_exp()?;
        let mut trailing_l3_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level4Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l3_exp = self.parse_l3_exp()?;
            trailing_l3_exps.push((op, l3_exp));
        }
        Ok((first_l3_exp, trailing_l3_exps))
    }

    fn parse_l3_exp(&mut self) -> RustCcResult<Level3Exp> {
        let first_l2_exp = self.parse_l2_exp()?;
        let mut trailing_l2_exps = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let Ok(op) = Level3Op::try_from(tok) else {
                self.lexer.back();
                break;
            };

            let l2_exp = self.parse_l2_exp()?;
            trailing_l2_exps.push((op, l2_exp));
        }
        Ok((first_l2_exp, trailing_l2_exps))
    }

    fn parse_l2_exp(&mut self) -> RustCcResult<Level2Exp> {
        let tok = self
            .lexer
            .next_token()
            .ok_or(RustCcError::ParseError(ParseError::UnexpectedTokenEnd))?;

        let l2_exp = match tok {
            Token::OpenParen => {
                let f = Level2Exp::ParenExp(Box::new(self.parse_l15_exp()?));
                self.lexer.expect_next(&Token::CloseParen)?;
                f
            }
            Token::Integer(u) => Level2Exp::Const(u),
            Token::Plus => Level2Exp::Unary(Level2Op::UnaryPlus, Box::new(self.parse_l2_exp()?)),
            Token::Minus => Level2Exp::Unary(Level2Op::UnaryMinus, Box::new(self.parse_l2_exp()?)),
            Token::ExclamationPoint => {
                Level2Exp::Unary(Level2Op::LogicalNot, Box::new(self.parse_l2_exp()?))
            }
            Token::Tilde => Level2Exp::Unary(Level2Op::BitwiseNot, Box::new(self.parse_l2_exp()?)),
            t => panic!("bad l2 token: {t:?}"),
        };

        Ok(l2_exp)
    }
}
