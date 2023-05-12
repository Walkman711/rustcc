use strum_macros::EnumString;

#[derive(Clone, Copy, Debug)]
pub enum Tokens {
    Keyword(Keywords),
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    Semicolon,
    Integer(i64),
}

impl TryFrom<&str> for Tokens {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "{" => Ok(Tokens::OpenBracket),
            "}" => Ok(Tokens::CloseBracket),
            "(" => Ok(Tokens::OpenParen),
            ")" => Ok(Tokens::CloseParen),
            ";" => Ok(Tokens::Semicolon),
            "int" => Ok(Tokens::Keyword(Keywords::Int)),
            "main" => Ok(Tokens::Keyword(Keywords::Main)),
            "return" => Ok(Tokens::Keyword(Keywords::Return)),
            _ => {
                if let Ok(i) = value.parse::<i64>() {
                    Ok(Tokens::Integer(i))
                } else {
                    Err(value.to_string())
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

fn lex(program: &str) -> Vec<Tokens> {
    let mut lexed_tokens = vec![];

    let program_with_whitespace = program
        .replace(';', " ; ")
        .replace('(', " ( ")
        .replace(')', " ) ")
        .replace('{', " { ")
        .replace('}', " } ");

    for token in program_with_whitespace.split_whitespace() {
        let lexed_token = Tokens::try_from(token).unwrap();
        lexed_tokens.push(lexed_token);
    }

    lexed_tokens
}

/// <program> ::= <function>
/// <function> ::= "int" <id> "(" ")" "{" <statement> ""
/// <statement> ::= "return" <exp> ";"
/// <exp> ::= <int>

fn parse(lexed: &[Tokens]) -> Program {
    let function = parse_fn(&lexed);
    Program::Func(function)
}

fn parse_fn(tokens: &[Tokens]) -> Function {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next();
    let Some(Tokens::Keyword(Keywords::Int)) = tok else {
        panic!("only support functions starting with int");
    };

    let tok = tok_it.next();
    let Some(Tokens::Keyword(Keywords::Main)) = tok else {
        panic!("");
    };

    let tok = tok_it.next();
    let Some(Tokens::OpenParen) = tok else {
        panic!("");
    };

    let tok = tok_it.next();
    let Some(Tokens::CloseParen) = tok else {
        panic!("");
    };

    let tok = tok_it.next();
    let Some(Tokens::OpenBracket) = tok else {
        panic!("");
    };

    let statement = parse_statement(&tokens[5..]);

    Function::Fun("main".to_string(), statement)
}

fn parse_statement(tokens: &[Tokens]) -> Statement {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next();
    let Some(Tokens::Keyword(Keywords::Return)) = tok else {
        panic!("");
    };

    let exp = parse_exp(&tokens[1..]);

    Statement::Return(exp)
}

fn parse_exp(tokens: &[Tokens]) -> Expression {
    let mut tok_it = tokens.iter();
    let tok = tok_it.next();
    let Some(Tokens::Integer(i)) = tok else {
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

fn main() {
    let test_program = "int main() { return 2; }";
    let lexed_tokens = lex(test_program);
    // dbg!(&lexed_tokens);
    let parsed_program = parse(&lexed_tokens);
    // dbg!(&parsed_program);
    generate_asm(parsed_program);
}
