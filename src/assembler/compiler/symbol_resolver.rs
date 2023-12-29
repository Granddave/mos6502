use crate::ast::{ASTConstantValue, ASTNode, ASTOperand, AST};

use std::fmt;

use super::CompilerError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolType {
    /// Label with an offset into the program
    Label(usize),
    ConstantByte(u8),
    ConstantWord(u16),
}

impl fmt::Display for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolType::Label(offset) => write!(f, "Label({:#x})", offset),
            SymbolType::ConstantByte(byte) => write!(f, "ConstantByte({:#x})", byte),
            SymbolType::ConstantWord(word) => write!(f, "ConstantWord({:#x})", word),
        }
    }
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
    #[tracing::instrument]
    pub fn new() -> SymbolTable {
        SymbolTable {
            symbols: Vec::new(),
        }
    }

    #[tracing::instrument]
    pub fn find_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.iter().find(|symbol| symbol.name == name)
    }
}

/// Resolve labels in the AST to the symbol table.
#[tracing::instrument]
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

/// Resolve constants in the AST to the symbol table.
#[tracing::instrument]
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

/// Verify that
///   - no symbol is defined multiple times
///   - all symbols used in the AST are defined
#[tracing::instrument]
pub fn verify_symbols(ast: &AST, symbol_table: &mut SymbolTable) -> Result<(), CompilerError> {
    // Verify that no symbol is defined multiple times
    for outer in &symbol_table.symbols {
        let mut count = 0;
        for inner in &symbol_table.symbols {
            if inner.name == outer.name {
                count += 1;
            }
            if count > 1 {
                return Err(CompilerError::SymbolAlreadyDefined(inner.name.clone()));
            }
        }
    }

    // Verify that all symbols used in the AST are defined
    for node in ast {
        if let ASTNode::Instruction(ins_node) = node {
            match &ins_node.operand {
                ASTOperand::Label(label_str) => match symbol_table.find_symbol(label_str) {
                    Some(_) => (),
                    None => return Err(CompilerError::UndefinedSymbol(label_str.clone())),
                },
                ASTOperand::Constant(constant_str) => {
                    match symbol_table.find_symbol(constant_str) {
                        Some(_) => (),
                        None => return Err(CompilerError::UndefinedSymbol(constant_str.clone())),
                    }
                }
                _ => (),
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ASTAddressingMode, ASTConstantNode, ASTMnemonic, ASTNode, ASTOperand, AST};

    use pretty_assertions::assert_eq;

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
        let res = verify_symbols(&ast, &mut symbol_table);
        assert!(res.is_ok());
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
    fn test_undefined_label() {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![ASTNode::new_instruction(
            ASTMnemonic::BNE,
            ASTAddressingMode::Relative,
            ASTOperand::Label("undefined".to_string()),
        )];

        resolve_labels(&ast, &mut symbol_table);
        let res = verify_symbols(&ast, &mut symbol_table);
        assert_eq!(
            res,
            Err(CompilerError::UndefinedSymbol("undefined".to_string()))
        );
    }

    #[test]
    fn test_undefined_constant() {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![ASTNode::new_instruction(
            ASTMnemonic::LDX,
            ASTAddressingMode::Immediate,
            ASTOperand::Constant("zero".to_string()),
        )];

        resolve_constants(&ast, &mut symbol_table);
        let res = verify_symbols(&ast, &mut symbol_table);
        assert_eq!(res, Err(CompilerError::UndefinedSymbol("zero".to_string())));
    }

    // Double definitions
    #[test]
    fn test_double_label_definition() {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![
            ASTNode::Label("label".to_string()),
            ASTNode::Label("label".to_string()),
        ];

        resolve_labels(&ast, &mut symbol_table);
        let res = verify_symbols(&ast, &mut symbol_table);
        assert_eq!(
            res,
            Err(CompilerError::SymbolAlreadyDefined("label".to_string()))
        );
    }

    #[test]
    fn test_double_constant_definition() {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![
            ASTNode::Constant(ASTConstantNode::new_byte("my_byte".to_string(), 0x12)),
            ASTNode::Constant(ASTConstantNode::new_byte("my_byte".to_string(), 0x12)),
        ];

        resolve_constants(&ast, &mut symbol_table);
        let res = verify_symbols(&ast, &mut symbol_table);
        assert_eq!(
            res,
            Err(CompilerError::SymbolAlreadyDefined("my_byte".to_string()))
        );
    }
}
