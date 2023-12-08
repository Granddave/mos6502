use crate::assembler::ast::ASTNode;

#[derive(Debug)]
pub struct Symbol {
    /// Name of the symbol.
    pub name: String,
    /// Byte offset from the start of the program.
    pub byte_offset: usize,
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

pub fn resolve_label(nodes: &Vec<ASTNode>, symbol_table: &mut SymbolTable) {
    let mut byte_offset = 0;
    for node in nodes {
        match node {
            ASTNode::Instruction(ins_node) => {
                byte_offset += ins_node.size();
            }
            ASTNode::Label(label) => symbol_table.symbols.push(Symbol {
                name: label.clone(),
                byte_offset,
            }),
        }
    }

    // TODO: Handle label in operand. Make sure that the label is resolved.
    //       We don't want to have any unresolved labels at this point.
}

#[cfg(test)]
mod tests {
    use crate::assembler::ast::{ASTAddressingMode, ASTMnemonic, ASTNode, ASTOperand};

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

        resolve_label(&nodes, &mut symbol_table);
        assert_eq!(symbol_table.symbols.len(), 2);
        assert_eq!(symbol_table.symbols[0].name, "firstloop");
        assert_eq!(symbol_table.symbols[0].byte_offset, 0x0004);
        assert_eq!(symbol_table.symbols[1].name, "secondloop");
        assert_eq!(symbol_table.symbols[1].byte_offset, 0x000F);
    }
}
