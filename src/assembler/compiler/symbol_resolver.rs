use crate::ast::{ASTNode, ASTOperand};

#[derive(Debug, PartialEq)]
pub enum SymbolType {
    /// Label with an offset into the program
    Label(usize),
    // TODO: Constant,
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    /// Name of the symbol
    pub name: String,
    /// Symbol type with data
    pub symbol: SymbolType,
}

/// Symbol table for labels.
///
/// The symbol table is a list of symbols.
/// Each symbol has a name and an address.
///
/// The symbol table is used to resolve labels.
/// The assembler first reads the source code and creates a symbol table.
/// Then it reads the source code again and generates the machine code.
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
}

pub fn resolve_labels(nodes: &Vec<ASTNode>, symbol_table: &mut SymbolTable) {
    let mut current_addr = 0;

    // Find label definitions and populate symbol table
    for node in nodes {
        match node {
            ASTNode::Instruction(ins_node) => {
                current_addr += ins_node.size();
            }
            ASTNode::Label(label) => symbol_table.symbols.push(Symbol {
                name: label.clone(),
                symbol: SymbolType::Label(current_addr),
            }),
        }
    }

    // Verify that no label is defined multiple times
    symbol_table.symbols.iter().for_each(|symbol| {
        let mut count = 0;
        symbol_table.symbols.iter().for_each(|s| {
            if s.name == symbol.name {
                count += 1;
            }
            if count > 1 {
                panic!("Label defined multiple times: '{}'", symbol.name);
            }
        });
    });

    // Verify that no instruction uses a label as operand that has not been resolved
    for node in nodes {
        match node {
            ASTNode::Instruction(ins_node) => match &ins_node.operand {
                ASTOperand::Label(label_str) => {
                    symbol_table
                        .symbols
                        .iter()
                        .find(|symbol| symbol.name == *label_str)
                        .expect("Label not found");
                }
                _ => (),
            },
            _ => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{ASTAddressingMode, ASTMnemonic, ASTNode, ASTOperand};

    fn example_ast() -> Vec<ASTNode> {
        vec![
            ASTNode::new_instruction(
                ASTMnemonic::LDX,
                ASTAddressingMode::Immediate,
                ASTOperand::Immediate(0x00),
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
                ASTAddressingMode::AbsoluteY,
                ASTOperand::Absolute(0x0200),
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

    use super::*;
    #[test]
    fn test_resolve_offsets() {
        let mut symbol_table = SymbolTable::new();
        let nodes = example_ast();

        resolve_labels(&nodes, &mut symbol_table);
        assert_eq!(symbol_table.symbols.len(), 2);
        assert_eq!(symbol_table.symbols[0].name, "firstloop");
        assert_eq!(symbol_table.symbols[0].symbol, SymbolType::Label(0x04));
        assert_eq!(symbol_table.symbols[1].name, "secondloop");
        assert_eq!(symbol_table.symbols[1].symbol, SymbolType::Label(0x0f));
    }

    #[test]
    #[should_panic]
    fn test_undefined_label() {
        let mut symbol_table = SymbolTable::new();
        let nodes = vec![ASTNode::new_instruction(
            ASTMnemonic::BNE,
            ASTAddressingMode::Relative,
            ASTOperand::Label("undefined".to_string()),
        )];

        resolve_labels(&nodes, &mut symbol_table);
    }

    #[test]
    #[should_panic]
    fn test_double_label_declaration() {
        let mut symbol_table = SymbolTable::new();
        let nodes = vec![
            ASTNode::Label("label".to_string()),
            ASTNode::Label("label".to_string()),
        ];
        resolve_labels(&nodes, &mut symbol_table);
        assert_eq!(symbol_table.symbols.len(), 1);
    }
}
