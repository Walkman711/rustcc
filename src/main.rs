use rustcc::{codegen::generate_asm, utils::RustCcResult};

fn main() -> RustCcResult<()> {
    // let test_program = "int main() { return ~2; }";
    let args: Vec<String> = std::env::args().collect();
    let test_program = std::fs::read_to_string(&args[1]).unwrap();
    let lexed_tokens = rustcc::lexer::lex(&test_program);
    let parsed_program = rustcc::parser::parse(&lexed_tokens)?;
    generate_asm(parsed_program);

    Ok(())
}
