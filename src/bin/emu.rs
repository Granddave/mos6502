use anyhow::{Context, Result};

use tracing_chrome::ChromeLayerBuilder;
use tracing_subscriber::prelude::*;

use mos6502::{
    assembler::compile_code,
    emulator::{self, tui},
};

// TODO: Either pass this as an argument, or check 0xfffc...?
const PROGRAM_START: u16 = 0x8000;

fn get_program_bytes() -> anyhow::Result<Vec<u8>> {
    if let Some(filename) = std::env::args().nth(1) {
        if filename.ends_with(".bin") {
            // Read binary file
            std::fs::read(filename).with_context(|| "Unable to read file")
        } else if filename.ends_with(".asm") {
            // Read assembly file
            let input_source = std::fs::read_to_string(filename).unwrap();
            compile_code(&input_source, PROGRAM_START).with_context(|| "Compilation failed")
        } else {
            return Err(anyhow::anyhow!("Unknown file type. Allowed: .bin, .asm"));
        }
    } else {
        Err(anyhow::anyhow!("No file specified. Allowed: .bin, .asm"))
    }
}

fn main() -> Result<()> {
    let (chrome_layer, _guard) = ChromeLayerBuilder::new().build();
    tracing_subscriber::registry().with(chrome_layer).init();

    let program_bytes = get_program_bytes()?;

    if std::env::args().any(|arg| arg == "--tui") {
        tui::exec(&program_bytes, PROGRAM_START)?;
    } else {
        emulator::run(&program_bytes, PROGRAM_START)?;
    };

    Ok(())
}
