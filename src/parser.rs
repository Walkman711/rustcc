use crate::{
    lexer::{Keywords, Token},
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

pub fn parse(tokens: &[Token]) -> RustCcResult<Program> {
    let function = parse_fn(tokens)?;
    Ok(Program::Func(function))
}

fn parse_fn(tokens: &[Token]) -> RustCcResult<Function> {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next();
    let Some(Token::Keyword(Keywords::Int)) = tok else {
        return Err(RustCcError::ParseError(Token::Keyword(Keywords::Int), tok.map(|t| t.to_owned())));
    };

    let tok = tok_it.next();
    let Some(Token::Identifier(identifier)) = tok else {
        return Err(RustCcError::ParseError(Token::Identifier("any function name".to_string()), tok.map(|t| t.to_owned())));
    };

    let tok = tok_it.next();
    let Some(Token::OpenParen) = tok else {
        return Err(RustCcError::ParseError(Token::OpenParen, tok.map(|t| t.to_owned())));
    };

    let tok = tok_it.next();
    let Some(Token::CloseParen) = tok else {
        return Err(RustCcError::ParseError(Token::CloseParen, tok.map(|t| t.to_owned())));
    };

    let tok = tok_it.next();
    let Some(Token::OpenBrace) = tok else {
        return Err(RustCcError::ParseError(Token::OpenBrace, tok.map(|t| t.to_owned())));
    };

    let statement = parse_statement(&tokens[5..])?;

    Ok(Function::Fun(identifier.to_owned(), statement))
}

fn parse_statement(tokens: &[Token]) -> RustCcResult<Statement> {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next();
    let Some(Token::Keyword(Keywords::Return)) = tok else {
        return Err(RustCcError::ParseError(Token::Keyword(Keywords::Return), tok.map(|t| t.to_owned())));
    };

    let exp = parse_exp(&tokens[1..])?;

    dbg!(tokens);
    let Token::CloseBrace = tokens[tokens.len() - 1] else {
        return Err(RustCcError::ParseError(Token::CloseBrace, tok.map(|t| t.to_owned())));
    };

    Ok(Statement::Return(exp))
}

fn parse_exp(tokens: &[Token]) -> RustCcResult<Expression> {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next().unwrap();
    let exp = match tok {
        Token::Integer(u) => Expression::Const(*u),
        Token::Negation => Expression::UnOp(UnaryOp::Negation, Box::new(parse_exp(&tokens[1..])?)),
        Token::LogicalNegation => {
            Expression::UnOp(UnaryOp::LogicalNegation, Box::new(parse_exp(&tokens[1..])?))
        }
        Token::BitwiseComplement => Expression::UnOp(
            UnaryOp::BitwiseComplement,
            Box::new(parse_exp(&tokens[1..])?),
        ),
        _ => panic!("come up with better error type for when multiple tokens are valid"),
    };

    let tok = tok_it.next();
    let Some(Token::Semicolon) = tok else {
        return Err(RustCcError::ParseError(Token::Semicolon, tok.map(|t| t.to_owned())));
    };

    Ok(exp)
}
