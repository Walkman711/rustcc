use std::process::Command;

use rustcc::{codegen::AsmGenerator, parser::Parser};

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let c_filename = &args[1];
    let asm_filename = c_filename.replace(".c", ".s");
    let out_filename = c_filename.replace(".c", "");

    let test_program = std::fs::read_to_string(c_filename)?;

    // Lex & parse program
    let mut parser = Parser::new(&test_program)?;
    let parsed_program = parser.parse()?;

    dbg!(&parsed_program);

    // Translate to assembly
    let mut asm_generator = AsmGenerator::new(&asm_filename);
    asm_generator.generate_asm(parsed_program);

    // Build
    Command::new("gcc")
        .arg("-o")
        .arg(out_filename)
        .arg(asm_filename)
        .output()
        .unwrap();

    Ok(())
}
