use anyhow::{Context, Result};
use tracing_chrome::ChromeLayerBuilder;
use tracing_subscriber::prelude::*;

use mos6502::{
    assembler::compile_code,
    emulator::{
        cpu::{self, Cpu},
        memory::{Bus, Memory},
    },
};

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

fn main() -> Result<()> {
    let (chrome_layer, _guard) = ChromeLayerBuilder::new().build();
    tracing_subscriber::registry().with(chrome_layer).init();

    let input_source = if let Some(filename) = std::env::args().nth(1) {
        std::fs::read_to_string(filename).unwrap()
    } else {
        demo().to_string()
    };

    let bytes = compile_code(&input_source).with_context(|| "Compilation failed")?;

    const PROGRAM_START: u16 = 0x0800;
    let mut memory = Memory::new();
    memory.write_word(cpu::RESET_VECTOR, PROGRAM_START);
    memory.load(PROGRAM_START, &bytes);

    let mut cpu = Cpu::new();
    cpu.reset();
    cpu.run(&mut memory, 1000);

    Ok(())
}
