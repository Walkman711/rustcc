use strum_macros::EnumString;

#[derive(Clone, Copy, Debug)]
pub enum Token {
    Keyword(Keywords),
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    Semicolon,
    Integer(i64),
}

#[derive(Debug)]
pub enum RustCcError {
    LexError(String),
    // Expected, actual
    ParseError(Token, Option<Token>),
}

pub type RustCcResult<T> = Result<T, RustCcError>;

impl TryFrom<&str> for Token {
    type Error = RustCcError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "{" => Ok(Token::OpenBracket),
            "}" => Ok(Token::CloseBracket),
            "(" => Ok(Token::OpenParen),
            ")" => Ok(Token::CloseParen),
            ";" => Ok(Token::Semicolon),
            "int" => Ok(Token::Keyword(Keywords::Int)),
            "main" => Ok(Token::Keyword(Keywords::Main)),
            "return" => Ok(Token::Keyword(Keywords::Return)),
            _ => {
                if let Ok(i) = value.parse::<i64>() {
                    Ok(Token::Integer(i))
                } else {
                    Err(RustCcError::LexError(value.to_string()))
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, EnumString)]
pub enum Keywords {
    Int,
    // TODO: get rid of this
    Main,
    Return,
}

#[derive(Clone, Copy, Debug)]
pub enum Expression {
    Const(i64),
}

#[derive(Clone, Copy, Debug)]
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

fn lex(program: &str) -> Vec<Token> {
    let mut lexed_tokens = vec![];

    let program_with_whitespace = program
        .replace(';', " ; ")
        .replace('(', " ( ")
        .replace(')', " ) ")
        .replace('{', " { ")
        .replace('}', " } ");

    for token in program_with_whitespace.split_whitespace() {
        let lexed_token = Token::try_from(token).unwrap();
        lexed_tokens.push(lexed_token);
    }

    lexed_tokens
}

/// <program> ::= <function>
/// <function> ::= "int" <id> "(" ")" "{" <statement> ""
/// <statement> ::= "return" <exp> ";"
/// <exp> ::= <int>

fn parse(lexed: &[Token]) -> RustCcResult<Program> {
    let function = parse_fn(&lexed)?;
    Ok(Program::Func(function))
}

fn parse_fn(tokens: &[Token]) -> RustCcResult<Function> {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next();
    let Some(Token::Keyword(Keywords::Int)) = tok else {
        return Err(RustCcError::ParseError(Token::Keyword(Keywords::Int), tok.map(|t| *t)));
    };

    let tok = tok_it.next();
    let Some(Token::Keyword(Keywords::Main)) = tok else {
        return Err(RustCcError::ParseError(Token::Keyword(Keywords::Main), tok.map(|t| *t)));
    };

    let tok = tok_it.next();
    let Some(Token::OpenParen) = tok else {
        return Err(RustCcError::ParseError(Token::OpenParen, tok.map(|t| *t)));
    };

    let tok = tok_it.next();
    let Some(Token::CloseParen) = tok else {
        return Err(RustCcError::ParseError(Token::CloseParen, tok.map(|t| *t)));
    };

    let tok = tok_it.next();
    let Some(Token::OpenBracket) = tok else {
        return Err(RustCcError::ParseError(Token::OpenBracket, tok.map(|t| *t)));
    };

    let statement = parse_statement(&tokens[5..])?;

    Ok(Function::Fun("main".to_string(), statement))
}

fn parse_statement(tokens: &[Token]) -> RustCcResult<Statement> {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next();
    let Some(Token::Keyword(Keywords::Return)) = tok else {
        return Err(RustCcError::ParseError(Token::Keyword(Keywords::Return), tok.map(|t| *t)));
    };

    let exp = parse_exp(&tokens[1..]);

    Ok(Statement::Return(exp))
}

fn parse_exp(tokens: &[Token]) -> Expression {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next();
    let Some(Token::Integer(i)) = tok else {
        panic!("");
    };

    Expression::Const(*i)
}

fn generate_asm(prog: Program) {
    match prog {
        Program::Func(func) => match func {
            Function::Fun(identifier, stmt) => {
                println!(".global _{identifier}");
                println!(".align 2");
                println!();
                println!("_{identifier}:");
                match stmt {
                    Statement::Return(exp) => {
                        match exp {
                            Expression::Const(i) => {
                                println!("  mov w0, {i} ");
                            }
                        };
                        println!("  ret")
                    }
                }
            }
        },
    }
}

fn main() -> RustCcResult<()> {
    let test_program = "int main() { return 2; }";
    let lexed_tokens = lex(test_program);
    // dbg!(&lexed_tokens);

    let parsed_program = parse(&lexed_tokens)?;
    // dbg!(&parsed_program);
    generate_asm(parsed_program);
    Ok(())
}
