use rustcc::{
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
    let lexed_tokens = rustcc::lexer::lex(&test_program);
    // dbg!(&lexed_tokens);

    let parsed_program = parse(&lexed_tokens)?;
    // dbg!(&parsed_program);
    generate_asm(parsed_program);
    Ok(())
}
