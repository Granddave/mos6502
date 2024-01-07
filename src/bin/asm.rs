use anyhow::{Context, Result};
use tracing_chrome::ChromeLayerBuilder;
use tracing_subscriber::prelude::*;

fn demo() -> &'static str {
    "
  define zero $00
  LDX #zero
  LDY #$00
firstloop:
  TXA
  STA $0200,Y
  PHA
  INX
  INY
  CPY #$10
  BNE firstloop ;loop until Y is $10
secondloop:
  PLA
  STA $0200,Y
  INY
  CPY #$20      ;loop until Y is $20
  BNE secondloop
"
}

const PROGRAM_START: u16 = 0x8000;

#[tracing::instrument]
fn main() -> Result<()> {
    let (chrome_layer, _guard) = ChromeLayerBuilder::new().build();
    tracing_subscriber::registry().with(chrome_layer).init();

    let input_source = if let Some(filename) = std::env::args().nth(1) {
        std::fs::read_to_string(filename).unwrap()
    } else {
        demo().to_string()
    };

    let bytes = mos6502::assembler::compile_code(&input_source, PROGRAM_START)
        .with_context(|| "Compilation failed")?;

    let output_filename = "a.bin";
    std::fs::write(output_filename, bytes).with_context(|| "Unable to write file")?;
    eprintln!("Wrote {}", output_filename);

    Ok(())
}
