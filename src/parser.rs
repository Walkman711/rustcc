use crate::{
    lexer::{Keywords, Lexer, Token},
    utils::{RustCcError, RustCcResult},
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

#[derive(Clone, Debug)]
pub enum Expression {
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Fact(Factor),
}

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

/// <program> ::= <function>
/// <function> ::= "int" <id> "(" ")" "{" <statement> ""
/// <statement> ::= "return" <exp> ";"
/// <exp> ::= <exp> <binary_op> <exp> | <factor>
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
        let tok = self.lexer.get_token();
        let Some(Token::Keyword(Keywords::Int)) = tok else {
        return Err(RustCcError::ParseError(Token::Keyword(Keywords::Int), tok));
        };

        let tok = self.lexer.get_token();
        let Some(Token::Identifier(identifier)) = tok else {
            return Err(RustCcError::ParseError(Token::Identifier("any function name".to_string()), tok));
        };

        let tok = self.lexer.get_token();
        let Some(Token::OpenParen) = tok else {
            return Err(RustCcError::ParseError(Token::OpenParen, tok));
        };

        let tok = self.lexer.get_token();
        let Some(Token::CloseParen) = tok else {
            return Err(RustCcError::ParseError(Token::CloseParen, tok));
        };

        let tok = self.lexer.get_token();
        let Some(Token::OpenBrace) = tok else {
            return Err(RustCcError::ParseError(Token::OpenBrace, tok))
        };

        let statement = self.parse_statement()?;

        Ok(Function::Fun(identifier, statement))
    }

    fn parse_statement(&mut self) -> RustCcResult<Statement> {
        let tok = self.lexer.get_token();
        let Some(Token::Keyword(Keywords::Return)) = tok else {
            return Err(RustCcError::ParseError(Token::Keyword(Keywords::Return), tok));
        };

        let exp = self.parse_exp()?;

        let tok = self.lexer.get_token();
        let Some(Token::Semicolon) = tok else {
            return Err(RustCcError::ParseError(Token::Semicolon, tok));
        };

        // dbg!(tokens);
        let tok = self.lexer.get_token();
        let Some(Token::CloseBrace) = tok else {
            return Err(RustCcError::ParseError(Token::CloseBrace, tok));
        };

        Ok(Statement::Return(exp))
    }

    fn parse_exp(&mut self) -> RustCcResult<Expression> {
        // let tok = self.lexer.get_token().unwrap();
        // let exp = match tok {
        //     // Token::Integer(u) => Expression::Const(u),
        //     // Token::Minus => Expression::UnOp(UnaryOp::Negation, Box::new(self.parse_exp()?)),
        //     // Token::ExclamationPoint => {
        //     //     Expression::UnOp(UnaryOp::LogicalNegation, Box::new(self.parse_exp()?))
        //     // }
        //     // Token::Tilde => {
        //     //     Expression::UnOp(UnaryOp::BitwiseComplement, Box::new(self.parse_exp()?))
        //     // }
        //     _ => panic!("come up with better error type for when multiple tokens are valid"),
        // };
        let exp = Expression::Fact(self.parse_factor()?);

        Ok(exp)
    }

    fn parse_factor(&mut self) -> RustCcResult<Factor> {
        let tok = self.lexer.get_token().unwrap();
        let factor = match tok {
            Token::OpenParen => Factor::ParenExp(Box::new(self.parse_exp()?)),
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
