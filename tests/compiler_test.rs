use mos6502::assembler::assemble_code;

use pretty_assertions::assert_eq;

#[test]
fn test_basic() {
    let input = "
  define zero 0
  define hex_10 16
  define hex_20 $20

.org $8000

  LDX #zero
  LDY #zero
firstloop:
  TXA
  STA $0200,Y
  PHA
  INX
  INY
  CPY #hex_10
  BNE firstloop ;loop until Y is $10
secondloop:
  PLA
  STA $0200,Y
  INY
  CPY #hex_20   ;loop until Y is $20
  BNE secondloop
";
    let bytes = assemble_code(input, 0x0000).unwrap_or_else(|e| panic!("{}", e));
    let mut expected: Vec<u8> = Vec::new();
    expected.extend(vec![0; 0x8000 - expected.len()]);
    expected.extend([
        /* LDX */ 0xa2, 0x00, /* LDA */ 0xa0, 0x00, /* TXA */ 0x8a,
        /* STA */ 0x99, 0x00, 0x02, /* PHA */ 0x48, /* INX */ 0xe8,
        /* INY */ 0xc8, /* CPY */ 0xc0, 0x10, /* BNE */ 0xd0, 0xf5,
        /* PLA */ 0x68, /* STA */ 0x99, 0x00, 0x02, /* INY */ 0xc8,
        /* CPY */ 0xc0, 0x20, /* BNE */ 0xd0, 0xf7,
    ]);
    assert_eq!(bytes, expected);
}
