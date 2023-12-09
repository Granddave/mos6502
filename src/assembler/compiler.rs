use core::panic;

use crate::assembler::ast::{
    instruction::OPCODE_MAPPING, ASTAddressingMode, ASTInstruction, ASTMnemonic,
};

use self::symbol_resolver::SymbolTable;
use super::{
    ast::{ASTNode, ASTOperand},
    lexer::Lexer,
    parser::Parser,
};

mod symbol_resolver;

pub struct Compiler {
    symbol_table: SymbolTable,
    current_address: u16,
    // TODO: Add program_offset
}

impl<'a> Compiler {
    pub fn new() -> Compiler {
        Compiler {
            symbol_table: SymbolTable::new(),
            current_address: 0,
        }
    }

    fn compile_node(&mut self, node: &ASTNode, bytes: &mut Vec<u8>) {
        match node {
            ASTNode::Instruction(ins) => {
                if let ASTOperand::Label(label) = &ins.operand {
                    let symbol = self
                        .symbol_table
                        .symbols
                        .iter()
                        .find(|symbol| symbol.name == *label)
                        .expect("Label not found");
                    let opcode = match ins.ins.mnemonic {
                        ASTMnemonic::JMP => OPCODE_MAPPING
                            .find_opcode(ASTInstruction {
                                mnemonic: ASTMnemonic::JMP,
                                addr_mode: ASTAddressingMode::Absolute,
                            })
                            .expect("Invalid instruction"),
                        ASTMnemonic::BNE
                        | ASTMnemonic::BEQ
                        | ASTMnemonic::BPL
                        | ASTMnemonic::BMI
                        | ASTMnemonic::BCC
                        | ASTMnemonic::BCS
                        | ASTMnemonic::BVC
                        | ASTMnemonic::BVS => OPCODE_MAPPING
                            .find_opcode(ASTInstruction {
                                mnemonic: ins.ins.mnemonic,
                                addr_mode: ASTAddressingMode::Relative,
                            })
                            .expect("Invalid instruction"),
                        _ => panic!("Unmatched instruction using label as operand: {:?}", ins),
                    };

                    bytes.push(opcode);

                    // TODO: Differentiate between relative and absolute addresses
                    //       Now relative addresses are used as absolute addresses which is wrong

                    // TODO: Get program offset from settings
                    let program_offset = 0x0000;
                    let offset = symbol.byte_offset as u16 + program_offset;
                    bytes.extend(offset.to_le_bytes());
                } else {
                    bytes.extend(ins.bytes());
                    self.current_address += bytes.len() as u16;
                }
            }
            ASTNode::Label(_) => (), // TODO: Document why we don't need to do anything here
        }
    }

    pub fn compile(&mut self, source: &'a str) -> Vec<u8> {
        let mut lexer = Lexer::new(source);
        let mut parser = Parser::new(&mut lexer);
        let ast = parser.parse_program();
        symbol_resolver::resolve_label(&ast, &mut self.symbol_table);

        let mut bytes = Vec::new();
        for node in ast {
            self.compile_node(&node, &mut bytes);
        }

        bytes
    }
}

#[cfg(test)]
mod tests {
    use super::Compiler;

    // Test that the compiler can compile single instructions
    #[test]
    fn test_compile_nodes() {
        let tests = vec![
            ("LDA #$01", vec![0xA9, 0x01]),
            ("LDA $0200", vec![0xAD, 0x00, 0x02]),
            ("LDA $0200,X", vec![0xBD, 0x00, 0x02]),
            // TODO: Test more instructions
        ];

        for (source, expected) in tests {
            let mut compiler = Compiler::new();
            let bytes = compiler.compile(source);
            assert_eq!(bytes, expected);
        }
    }

    #[test]
    fn test_compile_program() {
        let tests = vec![
            ("LDA #$01\nLDA $0200", vec![0xA9, 0x01, 0xAD, 0x00, 0x02]),
            (
                "LDA #$01\nLDA $0200\nLDA $0200,X",
                vec![0xA9, 0x01, 0xAD, 0x00, 0x02, 0xBD, 0x00, 0x02],
            ),
            (
                "LDA #$03
  JMP there
  BRK
  BRK
  BRK
there:
  STA $0200",
                vec![
                    0xA9, 0x03, 0x4C, 0x07, 0x00, 0x00, 0x00, 0x00, 0x8D, 0x00, 0x02,
                ], // No program offset included in test
            ),
            (
                "  LDX #$08
decrement:
  DEX
  STX $0200
  CPX #$03
  BNE decrement
  STX $0201
  BRK",
                vec![
                    0xA2, 0x08, 0xCA, 0x8E, 0x00, 0x02, 0xE0, 0x03, /*BNE*/ 0xD0, 0xF8, 0x8E,
                    0x01, 0x02, 0x00,
                ],
            ),
        ];

        for (source, expected) in tests {
            let mut compiler = Compiler::new();
            let bytes = compiler.compile(source);
            assert_eq!(bytes, expected);
        }
    }
}
