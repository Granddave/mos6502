use anyhow::{Context, Result};

const PROGRAM_START: u16 = 0x8000;

#[tracing::instrument]
fn main() -> Result<()> {
    let _trace_guard = mos6502::instrumentation::trace();

    let input_source = if let Some(filename) = std::env::args().nth(1) {
        std::fs::read_to_string(filename).unwrap()
    } else {
        return Err(anyhow::anyhow!("No file specified. Allowed filetype: .asm"));
    };

    let bytes = mos6502::assembler::compile_code(&input_source, PROGRAM_START)
        .with_context(|| "Compilation failed")?;

    let output_filename = "a.bin";
    std::fs::write(output_filename, bytes).with_context(|| "Unable to write file")?;
    eprintln!("Wrote {}", output_filename);

    Ok(())
}
