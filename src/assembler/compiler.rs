use self::symbol_resolver::{SymbolTable, SymbolType};
use super::{
    ast::{ASTAddressingMode, ASTNode, ASTOperand},
    lexer::Lexer,
    parser::Parser,
};

mod symbol_resolver;

pub struct Compiler {
    symbol_table: SymbolTable,
    current_address: u16,
    // TODO: Add program_offset, typically 0x0600
}

impl<'a> Compiler {
    pub fn new() -> Compiler {
        Compiler {
            symbol_table: SymbolTable::new(),
            current_address: 0,
        }
    }

    /// Resolve labels to absolute and relative addresses. This is done by looking up the label in
    /// the symbol table and replacing the label with the address of the label.
    fn resolve_labels_to_addr(&mut self, ast: &mut Vec<ASTNode>) {
        let mut current_addr = 0;
        for node in ast {
            match node {
                ASTNode::Instruction(ins) => {
                    // The current address is pointing to the address of the next instruction.
                    // The relative offset is calculated from the address of the following
                    // instruction due to the fact that the CPU has already incremented the
                    // program counter past the current instruction.
                    current_addr += ins.size();
                    if let ASTOperand::Label(label) = &ins.operand {
                        let symbol = self
                            .symbol_table
                            .symbols
                            .iter()
                            .find(|symbol| symbol.name == *label)
                            .expect("Label not found");
                        let SymbolType::Label(absolute_offset_in_program) = symbol.symbol;
                        match ins.ins.addr_mode {
                            ASTAddressingMode::Absolute => {
                                ins.operand =
                                    ASTOperand::Absolute(absolute_offset_in_program as u16);
                            }
                            ASTAddressingMode::Relative => {
                                let offset_addr = (absolute_offset_in_program as u16)
                                    .wrapping_sub(current_addr as u16)
                                    as i8;
                                ins.operand = ASTOperand::Relative(offset_addr);
                            }
                            _ => panic!("Invalid addressing mode for label"),
                        }
                    }
                }
                ASTNode::Label(_) => (),
            }
        }
    }

    fn compile_node(&mut self, node: &ASTNode, bytes: &mut Vec<u8>) {
        match node {
            ASTNode::Instruction(ins) => {
                bytes.extend(ins.bytes());
                self.current_address += ins.size() as u16;
            }
            // We don't need to generate any bytes for labels. They are just used for symbol
            // resolution.
            ASTNode::Label(_) => (),
        }
    }

    pub fn compile(&mut self, source: &'a str) -> Vec<u8> {
        let mut lexer = Lexer::new(source);
        let mut parser = Parser::new(&mut lexer);
        let mut ast = parser.parse_program();

        symbol_resolver::resolve_labels(&ast, &mut self.symbol_table);

        self.resolve_labels_to_addr(&mut ast);

        let mut bytes = Vec::new();
        ast.iter()
            .for_each(|node| self.compile_node(&node, &mut bytes));

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
                "  LDX #$08
decrement:
  DEX
  STX $0200
  CPX #$03
  BNE decrement
  STX $0201
  BRK",
                vec![
                    /* LDX */ 0xA2, 0x08, /* DEX */ 0xCA, /* STX */ 0x8E, 0x00,
                    0x02, /* CPX */ 0xE0, 0x03, /* BNE */ 0xD0, 0xF8,
                    /* STX */ 0x8E, 0x01, 0x02, /* BRK */ 0x00,
                ],
            ),
        ];

        for (source, expected) in tests {
            let mut compiler = Compiler::new();
            let bytes = compiler.compile(source);
            assert_eq!(bytes, expected);
        }
    }

    #[test]
    fn test_relative_branch() {
        // Some tests for relative branch instructions that tests that the compiler can
        // correctly calculate the relative offset. Both forward and backward branches are
        // tested.
        let tests = vec![
            (
                "  LDX #$08
loop:
  LDA #$01
  STA $0200
  BNE loop",
                vec![
                    /* LDX */ 0xA2, 0x08, /* LDA */ 0xA9, 0x01, /* STA */ 0x8D,
                    0x00, 0x02, /* BNE */ 0xD0, 0xF9,
                ],
            ),
            (
                "  LDA #$03
  JMP there
  BRK
  BRK
  BRK
there:
  STA $0200",
                vec![
                    /*LDA*/ 0xA9, 0x03, /*JMP*/ 0x4C, 0x08, 0x00, /*BRKs*/ 0x00,
                    0x00, 0x00, /*STA*/ 0x8D, 0x00, 0x02,
                ],
            ),
            (
                "  LDX #$08
loop:
  LDA #$01
  JMP end
  STA $0200
  BNE loop
end:
  BRK",
                vec![
                    /* LDX */ 0xA2, 0x08, /* LDA */ 0xA9, 0x01, /* JMP */ 0x4C,
                    0x0C, 0x00, /* STA */ 0x8D, 0x00, 0x02, /* BNE */ 0xD0, 0xF6,
                    /* BRK */ 0x00,
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
