use std::process::Command;

use rustcc::{
    codegen::{
        arm_generator::ArmGenerator, asm_generator::AsmGenerator, risc_v_generator::RiscVGenerator,
    },
    parsing::parser::Parser,
};

#[allow(unused_imports)]
use rustcc::parsing::parser_types::PrettyPrinter;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let c_filename = &args[1];
    let asm_filename = c_filename.replace(".c", ".s");
    let out_filename = c_filename.replace(".c", "");

    let test_program = std::fs::read_to_string(c_filename)?;
    // println!("\n{test_program}");

    // Lex & parse program
    let mut parser = Parser::new(&test_program)?;
    let parsed_program = parser.parse()?;

    // dbg!(&parsed_program);
    // println!("{parsed_program}");
    // parsed_program.pretty_print(0);

    // Translate to assembly
    // let mut arm_generator = ArmGenerator::try_from(&parsed_program)?;
    // arm_generator.gen_asm(&asm_filename, parsed_program)?;

    let mut risc_v_generator = RiscVGenerator::try_from(&parsed_program)?;
    risc_v_generator.gen_asm(&asm_filename, parsed_program)?;

    // Build
    Command::new("gcc")
        .arg("-o")
        .arg(out_filename)
        .arg(asm_filename)
        .output()?;

    Ok(())
}
