use crate::{
    lexer::{Keywords, Lexer, Token},
    utils::{RustCcError, RustCcResult},
};

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Negation,
    LogicalNegation,
    BitwiseComplement,
}

#[derive(Clone, Debug)]
pub enum Expression {
    UnOp(UnaryOp, Box<Expression>),
    Const(u64),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Clone, Debug)]
pub enum Function {
    Fun(String, Statement),
}

#[derive(Clone, Debug)]
pub enum Program {
    Func(Function),
}

/// <program> ::= <function>
/// <function> ::= "int" <id> "(" ")" "{" <statement> ""
/// <statement> ::= "return" <exp> ";"
/// <exp> ::= <int> | <unop> <exp>
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
        return Err(RustCcError::ParseError(Token::Keyword(Keywords::Int), tok.map(|t| t.to_owned())));
        };

        let tok = self.lexer.get_token();
        let Some(Token::Identifier(identifier)) = tok else {
            return Err(RustCcError::ParseError(Token::Identifier("any function name".to_string()), tok.map(|t| t.to_owned())));
        };

        let tok = self.lexer.get_token();
        let Some(Token::OpenParen) = tok else {
            return Err(RustCcError::ParseError(Token::OpenParen, tok.map(|t| t.to_owned())));
        };

        let tok = self.lexer.get_token();
        let Some(Token::CloseParen) = tok else {
            return Err(RustCcError::ParseError(Token::CloseParen, tok.map(|t| t.to_owned())));
        };

        let tok = self.lexer.get_token();
        let Some(Token::OpenBrace) = tok else {
            return Err(RustCcError::ParseError(Token::OpenBrace, tok.map(|t| t.to_owned())));
        };

        let statement = self.parse_statement()?;

        Ok(Function::Fun(identifier.to_owned(), statement))
    }

    fn parse_statement(&mut self) -> RustCcResult<Statement> {
        let tok = self.lexer.get_token();
        let Some(Token::Keyword(Keywords::Return)) = tok else {
            return Err(RustCcError::ParseError(Token::Keyword(Keywords::Return), tok.map(|t| t.to_owned())));
        };

        let exp = self.parse_exp()?;

        let tok = self.lexer.get_token();
        let Some(Token::Semicolon) = tok else {
            return Err(RustCcError::ParseError(Token::Semicolon, tok.map(|t| t.to_owned())));
        };

        // dbg!(tokens);
        let tok = self.lexer.get_token();
        let Some(Token::CloseBrace) = tok else {
            return Err(RustCcError::ParseError(Token::CloseBrace, tok.map(|t| t.to_owned())));
        };

        Ok(Statement::Return(exp))
    }

    fn parse_exp(&mut self) -> RustCcResult<Expression> {
        let tok = self.lexer.get_token().unwrap();
        let exp = match tok {
            Token::Integer(u) => Expression::Const(u),
            Token::Negation => Expression::UnOp(UnaryOp::Negation, Box::new(self.parse_exp()?)),
            Token::LogicalNegation => {
                Expression::UnOp(UnaryOp::LogicalNegation, Box::new(self.parse_exp()?))
            }
            Token::BitwiseComplement => {
                Expression::UnOp(UnaryOp::BitwiseComplement, Box::new(self.parse_exp()?))
            }
            _ => panic!("come up with better error type for when multiple tokens are valid"),
        };

        Ok(exp)
    }
}
