use std::process::Command;

use rustcc::{codegen::generate_asm, utils::RustCcResult};

fn main() -> RustCcResult<()> {
    // let test_program = "int main() { return ~2; }";
    let args: Vec<String> = std::env::args().collect();
    let c_filename = &args[1];
    let asm_filename = c_filename.replace(".c", ".s");
    let out_filename = c_filename.replace(".c", "");

    let test_program = std::fs::read_to_string(c_filename).unwrap();
    let lexed_tokens = rustcc::lexer::lex(&test_program);
    let parsed_program = rustcc::parser::parse(&lexed_tokens)?;
    generate_asm(parsed_program, &asm_filename);
    Command::new("gcc")
        .arg("-o")
        .arg(out_filename)
        .arg(asm_filename)
        .output()
        .expect("failed to compile");

    Ok(())
}
