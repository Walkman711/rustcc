use rustcc::{
    parser::{Expression, Function, Program, Statement, UnaryOp},
    utils::RustCcResult,
};

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

    let parsed_program = rustcc::parser::parse(&lexed_tokens)?;
    // dbg!(&parsed_program);
    generate_asm(parsed_program);
    Ok(())
}
