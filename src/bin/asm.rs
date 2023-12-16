fn default() -> &'static str {
    "
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
"
}

fn main() {
    let input = if let Some(filename) = std::env::args().nth(1) {
        std::fs::read_to_string(filename).unwrap()
    } else {
        default().to_string()
    };

    let bytes = mos6502::assembler::compile_code(&input);

    println!("{}", mos6502::hexdump::hexdump(&bytes, 7, 16));
    std::fs::write("a.bin", &bytes).unwrap();
}
