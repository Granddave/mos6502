use mos6502::assembler::compiler::Compiler;

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
  BNE secondloop";

    // let mut lexer = Lexer::new(input);
    // let mut parser = Parser::new(&mut lexer);
    // let ast = parser.parse_program();
    // for instruction in ast {
    //     println!("{}", instruction);
    // }
    let mut compiler = Compiler::new();
    let bytes = compiler.compile(input);
    eprintln!("{:?}", bytes);
}
