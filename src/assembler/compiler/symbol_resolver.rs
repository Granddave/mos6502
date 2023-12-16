use crate::ast::{ASTConstantValue, ASTNode, ASTOperand, AST};

#[derive(Debug, PartialEq)]
pub enum SymbolType {
    /// Label with an offset into the program
    Label(usize),
    ConstantByte(u8),
    ConstantWord(u16),
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    /// Name of the symbol
    pub name: String,
    /// Symbol type with data
    pub symbol: SymbolType,
}

/// The symbol table is used to resolve labels and constants.
#[derive(Debug)]
pub struct SymbolTable {
    pub symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            symbols: Vec::new(),
        }
    }

    pub fn find_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.iter().find(|symbol| symbol.name == name)
    }
}

pub fn resolve_labels(ast: &AST, symbol_table: &mut SymbolTable) {
    let mut current_addr = 0;

    ast.iter().for_each(|node| match node {
        ASTNode::Instruction(ins_node) => {
            current_addr += ins_node.size();
        }
        ASTNode::Label(label) => symbol_table.symbols.push(Symbol {
            name: label.clone(),
            symbol: SymbolType::Label(current_addr),
        }),
        _ => (),
    })
}

pub fn resolve_constants(ast: &AST, symbol_table: &mut SymbolTable) {
    ast.iter().for_each(|node| {
        if let ASTNode::Constant(constant) = node {
            symbol_table.symbols.push(Symbol {
                name: constant.identifier.clone(),
                symbol: match constant.value {
                    ASTConstantValue::Byte(byte) => SymbolType::ConstantByte(byte),
                    ASTConstantValue::Word(word) => SymbolType::ConstantWord(word),
                },
            });
        }
    })
}

pub fn verify_symbols(ast: &AST, symbol_table: &mut SymbolTable) {
    // Verify that no symbols are defined multiple times
    symbol_table.symbols.iter().for_each(|symbol| {
        let mut count = 0;
        symbol_table.symbols.iter().for_each(|s| {
            if s.name == symbol.name {
                count += 1;
            }
            if count > 1 {
                panic!("Symbol defined multiple times: '{}'", symbol.name);
            }
        });
    });

    // Verify that no instruction uses a label or constant as operand that has not been resolved
    ast.iter().for_each(|node| {
        if let ASTNode::Instruction(ins_node) = node {
            match &ins_node.operand {
                ASTOperand::Label(label_str) => {
                    symbol_table
                        .find_symbol(label_str)
                        .expect("Label not found");
                }
                ASTOperand::Constant(constant_str) => {
                    symbol_table
                        .find_symbol(constant_str)
                        .expect("Constant not found");
                }
                _ => (),
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ASTAddressingMode, ASTConstantNode, ASTMnemonic, ASTNode, ASTOperand, AST};

    fn example_ast() -> AST {
        vec![
            ASTNode::Constant(ASTConstantNode::new_byte("zero".to_string(), 0x00)),
            ASTNode::Constant(ASTConstantNode::new_word("addr".to_string(), 0x1234)),
            ASTNode::new_instruction(
                ASTMnemonic::LDX,
                ASTAddressingMode::Immediate,
                ASTOperand::Constant("zero".to_string()),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::LDY,
                ASTAddressingMode::Immediate,
                ASTOperand::Immediate(0x00),
            ),
            ASTNode::Label("firstloop".to_string()),
            ASTNode::new_instruction(
                ASTMnemonic::TXA,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            ),
            ASTNode::new_instruction(
                ASTMnemonic::STA,
                ASTAddressingMode::Absolute,
                ASTOperand::Constant("addr".to_string()),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::PHA,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            ),
            ASTNode::new_instruction(
                ASTMnemonic::INX,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            ),
            ASTNode::new_instruction(
                ASTMnemonic::INY,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            ),
            ASTNode::new_instruction(
                ASTMnemonic::CPY,
                ASTAddressingMode::Immediate,
                ASTOperand::Immediate(0x10),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::BNE,
                ASTAddressingMode::Relative,
                ASTOperand::Label("firstloop".to_string()),
            ),
            ASTNode::Label("secondloop".to_string()),
            ASTNode::new_instruction(
                ASTMnemonic::PLA,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            ),
            ASTNode::new_instruction(
                ASTMnemonic::STA,
                ASTAddressingMode::AbsoluteY,
                ASTOperand::Absolute(0x0200),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::INY,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            ),
            ASTNode::new_instruction(
                ASTMnemonic::CPY,
                ASTAddressingMode::Immediate,
                ASTOperand::Immediate(0x20),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::BNE,
                ASTAddressingMode::Relative,
                ASTOperand::Label("secondloop".to_string()),
            ),
        ]
    }

    // ** Happy path cases **
    #[test]
    fn test_resolve_symbols() {
        let mut symbol_table = SymbolTable::new();
        let ast = example_ast();

        resolve_labels(&ast, &mut symbol_table);
        resolve_constants(&ast, &mut symbol_table);
        verify_symbols(&ast, &mut symbol_table);
        assert_eq!(symbol_table.symbols.len(), 4);
        assert_eq!(symbol_table.symbols[0].name, "firstloop");
        assert_eq!(symbol_table.symbols[0].symbol, SymbolType::Label(0x04));
        assert_eq!(symbol_table.symbols[1].name, "secondloop");
        assert_eq!(symbol_table.symbols[1].symbol, SymbolType::Label(0x0f));
        assert_eq!(symbol_table.symbols[2].name, "zero");
        assert_eq!(
            symbol_table.symbols[2].symbol,
            SymbolType::ConstantByte(0x00)
        );
        assert_eq!(symbol_table.symbols[3].name, "addr");
        assert_eq!(
            symbol_table.symbols[3].symbol,
            SymbolType::ConstantWord(0x1234)
        );
    }

    // ** Error cases **
    // Undefined symbols
    #[test]
    #[should_panic]
    fn test_undefined_label() {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![ASTNode::new_instruction(
            ASTMnemonic::BNE,
            ASTAddressingMode::Relative,
            ASTOperand::Label("undefined".to_string()),
        )];

        resolve_labels(&ast, &mut symbol_table);
        verify_symbols(&ast, &mut symbol_table);
    }

    #[test]
    #[should_panic]
    fn test_undefined_constant() {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![ASTNode::new_instruction(
            ASTMnemonic::LDX,
            ASTAddressingMode::Immediate,
            ASTOperand::Constant("zero".to_string()),
        )];

        resolve_constants(&ast, &mut symbol_table);
        verify_symbols(&ast, &mut symbol_table);
    }

    // Double definitions
    #[test]
    #[should_panic]
    fn test_double_label_definition() {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![
            ASTNode::Label("label".to_string()),
            ASTNode::Label("label".to_string()),
        ];
        resolve_labels(&ast, &mut symbol_table);
        verify_symbols(&ast, &mut symbol_table);
    }

    #[test]
    #[should_panic]
    fn test_double_constant_definition() {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![
            ASTNode::Constant(ASTConstantNode::new_byte("my_byte".to_string(), 0x12)),
            ASTNode::Constant(ASTConstantNode::new_byte("my_byte".to_string(), 0x12)),
        ];
        resolve_constants(&ast, &mut symbol_table);
        verify_symbols(&ast, &mut symbol_table);
    }
}
