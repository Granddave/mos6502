use mos6502::{assembler::compiler::Compiler, hexdump};

fn main() {
    let input = "
  LDX #$00
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
";

    let mut compiler = Compiler::new(0x0600);
    let bytes = compiler.compile(input);

    hexdump::hexdump(&bytes, 7, 16);
    std::fs::write("a.bin", &bytes).unwrap();
}
