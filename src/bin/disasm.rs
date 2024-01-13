use anyhow::{Context, Result};

use mos6502::{
    ast::Node,
    disassembler::{disassemble_code, listing},
};

#[tracing::instrument]
fn main() -> Result<()> {
    let _trace_guard = mos6502::instrumentation::trace();

    let input = std::env::args()
        .nth(1)
        .with_context(|| "No input file specified")?;
    let bytes = std::fs::read(input).with_context(|| "Unable to read file")?;
    let ast = disassemble_code(&bytes)
        .into_iter()
        .map(Node::Instruction)
        .collect();
    println!("{}", listing::generate(0x8000, ast));

    Ok(())
}
