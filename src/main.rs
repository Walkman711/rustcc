#[derive(Clone, Copy, Debug)]
pub enum Token {
    Keyword(Keywords),
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    Semicolon,
    Integer(u64),
    Negation,
    LogicalNegation,
    BitwiseComplement,
}

#[derive(Debug)]
pub enum RustCcError {
    LexError(String),
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
            "-" => Ok(Token::Negation),
            "!" => Ok(Token::LogicalNegation),
            "~" => Ok(Token::BitwiseComplement),
            _ => {
                if let Ok(u) = value.parse::<u64>() {
                    Ok(Token::Integer(u))
                } else {
                    Err(RustCcError::LexError(value.to_string()))
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Keywords {
    Int,
    // TODO: get rid of this
    Main,
    Return,
}

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

fn lex(program: &str) -> Vec<Token> {
    let mut lexed_tokens = vec![];

    let chars_to_expand = vec![';', '-', '!', '~', '(', ')', '{', '}'];

    // TODO: make list of special chars
    let program_with_whitespace = program
        .replace(';', " ; ")
        .replace('-', " - ")
        .replace('!', " ! ")
        .replace('~', " ~ ")
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
/// <exp> ::= <int> | <unop> <exp>
/// <unop> ::= "!" | "~" | "-"

fn parse(tokens: &[Token]) -> RustCcResult<Program> {
    let function = parse_fn(tokens)?;
    Ok(Program::Func(function))
}

fn parse_fn(tokens: &[Token]) -> RustCcResult<Function> {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next();
    let Some(Token::Keyword(Keywords::Int)) = tok else {
        return Err(RustCcError::ParseError(Token::Keyword(Keywords::Int), tok.copied()));
    };

    let tok = tok_it.next();
    let Some(Token::Keyword(Keywords::Main)) = tok else {
        return Err(RustCcError::ParseError(Token::Keyword(Keywords::Main), tok.copied()));
    };

    let tok = tok_it.next();
    let Some(Token::OpenParen) = tok else {
        return Err(RustCcError::ParseError(Token::OpenParen, tok.copied()));
    };

    let tok = tok_it.next();
    let Some(Token::CloseParen) = tok else {
        return Err(RustCcError::ParseError(Token::CloseParen, tok.copied()));
    };

    let tok = tok_it.next();
    let Some(Token::OpenBracket) = tok else {
        return Err(RustCcError::ParseError(Token::OpenBracket, tok.copied()));
    };

    let statement = parse_statement(&tokens[5..])?;

    Ok(Function::Fun("main".to_string(), statement))
}

fn parse_statement(tokens: &[Token]) -> RustCcResult<Statement> {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next();
    let Some(Token::Keyword(Keywords::Return)) = tok else {
        return Err(RustCcError::ParseError(Token::Keyword(Keywords::Return), tok.copied()));
    };

    let exp = parse_exp(&tokens[1..]);

    Ok(Statement::Return(exp))
}

fn parse_exp(tokens: &[Token]) -> Expression {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next().unwrap();
    match tok {
        Token::Integer(u) => Expression::Const(*u),
        Token::Negation => Expression::UnOp(UnaryOp::Negation, Box::new(parse_exp(&tokens[1..]))),
        Token::LogicalNegation => {
            Expression::UnOp(UnaryOp::LogicalNegation, Box::new(parse_exp(&tokens[1..])))
        }
        Token::BitwiseComplement => Expression::UnOp(
            UnaryOp::BitwiseComplement,
            Box::new(parse_exp(&tokens[1..])),
        ),
        _ => panic!("come up with better error type for when multiple tokens are valid"),
    }
}

fn generate_asm(prog: Program) {
    match prog {
        Program::Func(func) => match func {
            Function::Fun(identifier, stmt) => {
                println!(".global _{identifier}");
                println!(".align 2");
                println!();
                println!("_{identifier}:");
                generate_stmt_asm(stmt);
            }
        },
    }
}

fn generate_stmt_asm(stmt: Statement) {
    match stmt {
        Statement::Return(exp) => {
            generate_expression_asm(exp);
        }
    }
    println!("  ret");
}

fn generate_expression_asm(exp: Expression) {
    match exp {
        Expression::Const(u) => {
            println!("  mov w0, {u} ");
        }
        Expression::UnOp(op, nested_exp) => {
            generate_expression_asm(*nested_exp);
            match op {
                UnaryOp::Negation => println!(" neg w0, w0"),
                UnaryOp::LogicalNegation => {
                    println!("  cmp w0, wzr");
                    println!("  cset w0, eq");
                    println!("  uxtb w0, w0")
                }
                UnaryOp::BitwiseComplement => println!("  mvn w0, w0"),
            }
        }
    };
}

fn main() -> RustCcResult<()> {
    let test_program = "int main() { return !2; }";
    // let args: Vec<String> = std::env::args().collect();
    // let test_program = std::fs::read_to_string(&args[1]).unwrap();
    let lexed_tokens = lex(&test_program);
    // dbg!(&lexed_tokens);

    let parsed_program = parse(&lexed_tokens)?;
    // dbg!(&parsed_program);
    generate_asm(parsed_program);
    Ok(())
}
