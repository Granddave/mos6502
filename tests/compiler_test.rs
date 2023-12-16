use mos6502::assembler::compile_code;

#[test]
fn test_basic() {
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
    let bytes = compile_code(input);
    let expected = [
        /* LDX */ 0xa2, 0x00, /* LDA */ 0xa0, 0x00, /* TXA */ 0x8a,
        /* STA */ 0x99, 0x00, 0x02, /* PHA */ 0x48, /* INX */ 0xe8,
        /* INY */ 0xc8, /* CPY */ 0xc0, 0x10, /* BNE */ 0xd0, 0xf5,
        /* PLA */ 0x68, /* STA */ 0x99, 0x00, 0x02, /* INY */ 0xc8,
        /* CPY */ 0xc0, 0x20, /* BNE */ 0xd0, 0xf7,
    ];
    assert_eq!(bytes, expected);
}
