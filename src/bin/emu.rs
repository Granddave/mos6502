use anyhow::{Context, Result};
use tracing_chrome::ChromeLayerBuilder;
use tracing_subscriber::prelude::*;

use mos6502::{
    assembler::compile_code,
    emulator::{
        cpu::{self, Cpu, RunOption},
        memory::{Bus, Memory},
        tui::Tui,
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

const PROGRAM_START: u16 = 0x8000;

enum RunMode {
    Headless,
    Tui,
}

fn main() -> Result<()> {
    let (chrome_layer, _guard) = ChromeLayerBuilder::new().build();
    tracing_subscriber::registry().with(chrome_layer).init();

    let bytes = if let Some(filename) = std::env::args().nth(1) {
        if filename.ends_with(".bin") {
            // Read binary file
            std::fs::read(filename).with_context(|| "Unable to read file")?
        } else if filename.ends_with(".asm") {
            // Read assembly file
            let input_source = std::fs::read_to_string(filename).unwrap();
            compile_code(&input_source, PROGRAM_START).with_context(|| "Compilation failed")?
        } else {
            return Err(anyhow::anyhow!("Unknown file type. Allowed: .bin, .asm"));
        }
    } else {
        compile_code(demo(), PROGRAM_START).with_context(|| "Compilation failed")?
    };

    let run_mode = if std::env::args().any(|arg| arg == "--tui") {
        RunMode::Tui
    } else {
        RunMode::Headless
    };

    let mut memory = Memory::new();
    memory.write_word(cpu::RESET_VECTOR, PROGRAM_START);
    memory.load(PROGRAM_START, &bytes);

    let mut cpu = Cpu::new();
    cpu.reset();
    match run_mode {
        RunMode::Headless => cpu.run(&mut memory, RunOption::StopOnBreakInstruction),
        RunMode::Tui => {
            let tui = &mut Tui::new(&mut cpu, &mut memory);
            tui.exec();
        }
    }

    Ok(())
}
