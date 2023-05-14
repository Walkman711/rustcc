use crate::{
    lexer::{Keywords, Lexer, Token},
    utils::{ParseError, RustCcError, RustCcResult},
};

#[derive(Clone, Debug)]
pub enum Program {
    Func(Function),
}

#[derive(Clone, Debug)]
pub enum Function {
    Fun(String, Statement),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Return(Expression),
}

pub type Expression = (Term, Vec<(AddSubtract, Term)>);
pub type Term = (Factor, Vec<(MultiplyDivide, Factor)>);

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Negation,
    LogicalNegation,
    BitwiseComplement,
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Clone, Debug)]
pub enum Factor {
    Const(u64),
    Unary(UnaryOp, Box<Factor>),
    ParenExp(Box<Expression>),
}

// TODO: do something with bin op precedence
#[derive(Clone, Debug)]
pub enum AddSubtract {
    Addition,
    Subtraction,
}

#[derive(Clone, Debug)]
pub enum MultiplyDivide {
    Multiply,
    Divide,
}

/// <program> ::= <function>
/// <function> ::= "int" <id> "(" ")" "{" <statement> ""
/// <statement> ::= "return" <exp> ";"
/// <exp> ::= <term> { ("+" | "-") <term> }
/// <term> ::= <factor> { ("*" | "/") <factor> }
/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int_value>
/// <unop> ::= "!" | "~" | "-"

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(program: &str) -> RustCcResult<Self> {
        let lexer = Lexer::try_from(program)?;
        Ok(Self { lexer })
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

        let statement = self.parse_statement()?;

        Ok(Function::Fun(identifier, statement))
    }

    fn parse_statement(&mut self) -> RustCcResult<Statement> {
        self.lexer.expect_next(&Token::Keyword(Keywords::Return))?;

        let exp = self.parse_exp()?;

        self.lexer.expect_next(&Token::Semicolon)?;
        self.lexer.expect_next(&Token::CloseBrace)?;

        Ok(Statement::Return(exp))
    }

    fn parse_exp(&mut self) -> RustCcResult<Expression> {
        let first_term = self.parse_term()?;
        let mut trailing_terms = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let op = match tok {
                Token::Plus => AddSubtract::Addition,
                // FIX: no way this works
                Token::Minus => AddSubtract::Subtraction,
                _ => {
                    // go back by one if next token isn't a trailing term
                    self.lexer.back();
                    break;
                }
            };
            let term = self.parse_term()?;
            trailing_terms.push((op, term));
        }
        Ok((first_term, trailing_terms))
    }

    fn parse_term(&mut self) -> RustCcResult<Term> {
        let first_factor = self.parse_factor()?;
        let mut trailing_factors = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let op = match tok {
                Token::Star => MultiplyDivide::Multiply,
                Token::Slash => MultiplyDivide::Divide,
                _ => {
                    // go back by one if next token isn't a trailing factor
                    self.lexer.back();
                    break;
                }
            };
            let factor = self.parse_factor()?;
            trailing_factors.push((op, factor));
        }
        Ok((first_factor, trailing_factors))
    }

    fn parse_factor(&mut self) -> RustCcResult<Factor> {
        let tok = self
            .lexer
            .next_token()
            .ok_or(RustCcError::ParseError(ParseError::UnexpectedTokenEnd))?;

        let factor = match tok {
            Token::OpenParen => {
                let f = Factor::ParenExp(Box::new(self.parse_exp()?));
                self.lexer.expect_next(&Token::CloseParen)?;
                f
            }
            Token::Integer(u) => Factor::Const(u),
            Token::Minus => Factor::Unary(UnaryOp::Negation, Box::new(self.parse_factor()?)),
            Token::ExclamationPoint => {
                Factor::Unary(UnaryOp::LogicalNegation, Box::new(self.parse_factor()?))
            }
            Token::Tilde => {
                Factor::Unary(UnaryOp::BitwiseComplement, Box::new(self.parse_factor()?))
            }
            _ => panic!("bad factor"),
        };

        Ok(factor)
    }
}
