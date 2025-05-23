use super::{Symbol, SymbolError, SymbolTable, SymbolType};
use crate::ast::{ConstantValue, Directive, Node, Operand, AST};

/// Find and add labels in the AST to the symbol table.
#[tracing::instrument]
pub fn index_labels(ast: &AST, symbol_table: &mut SymbolTable) -> Result<(), SymbolError> {
    let mut current_addr = 0;

    for node in ast.iter() {
        match node {
            Node::Instruction(ins) => {
                current_addr += ins.size();
            }
            Node::Label(label) => symbol_table.new_symbol(Symbol {
                name: label.clone(),
                symbol: SymbolType::Label(current_addr),
            })?,
            Node::Directive(directive) => match directive {
                Directive::Origin(org_addr) => current_addr = *org_addr as usize,
                Directive::Byte(_) => current_addr += 1,
                Directive::Word(_) => current_addr += 2,
            },
            _ => (),
        }
    }

    Ok(())
}

/// Find and add constants in the AST to the symbol table.
#[tracing::instrument]
pub fn index_constants(ast: &AST, symbol_table: &mut SymbolTable) -> Result<(), SymbolError> {
    for node in ast.iter() {
        if let Node::Constant(constant) = node {
            symbol_table.new_symbol(Symbol {
                name: constant.identifier.clone(),
                symbol: match constant.value {
                    ConstantValue::Byte(byte) => SymbolType::ConstantByte(byte),
                    ConstantValue::Word(word) => SymbolType::ConstantWord(word),
                },
            })?;
        }
    }

    Ok(())
}

/// Verify that all symbols used in the AST are defined
#[tracing::instrument]
pub fn verify_symbols(ast: &AST, symbol_table: &SymbolTable) -> Result<(), SymbolError> {
    for node in ast {
        if let Node::Instruction(ins) = node {
            match &ins.operand {
                Operand::Label(label_str) => {
                    symbol_table
                        .find_symbol(label_str)
                        .ok_or_else(|| SymbolError::UndefinedSymbol(label_str.clone()))?;
                }
                Operand::Constant(constant_str) => {
                    symbol_table
                        .find_symbol(constant_str)
                        .ok_or_else(|| SymbolError::UndefinedSymbol(constant_str.clone()))?;
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
    use crate::ast::{AddressingMode, Constant, Mnemonic, Node, Operand, AST};

    use pretty_assertions::assert_eq;

    fn example_ast() -> AST {
        vec![
            Node::Directive(Directive::Origin(0x8000)),
            Node::Directive(Directive::Byte(0x12)),
            Node::Directive(Directive::Word(0x3456)),
            Node::Constant(Constant::new_byte("zero".to_string(), 0x00)),
            Node::Constant(Constant::new_word("addr".to_string(), 0x1234)),
            Node::new_instruction(
                Mnemonic::LDX,
                AddressingMode::Immediate,
                Operand::Constant("zero".to_string()),
            ),
            Node::new_instruction(
                Mnemonic::LDY,
                AddressingMode::Immediate,
                Operand::Immediate(0x00),
            ),
            Node::Label("firstloop".to_string()),
            Node::new_instruction(Mnemonic::TXA, AddressingMode::Implied, Operand::Implied),
            Node::new_instruction(
                Mnemonic::STA,
                AddressingMode::Absolute,
                Operand::Constant("addr".to_string()),
            ),
            Node::new_instruction(Mnemonic::PHA, AddressingMode::Implied, Operand::Implied),
            Node::new_instruction(Mnemonic::INX, AddressingMode::Implied, Operand::Implied),
            Node::new_instruction(Mnemonic::INY, AddressingMode::Implied, Operand::Implied),
            Node::new_instruction(
                Mnemonic::CPY,
                AddressingMode::Immediate,
                Operand::Immediate(0x10),
            ),
            Node::new_instruction(
                Mnemonic::BNE,
                AddressingMode::Relative,
                Operand::Label("firstloop".to_string()),
            ),
            Node::Label("secondloop".to_string()),
            Node::new_instruction(Mnemonic::PLA, AddressingMode::Implied, Operand::Implied),
            Node::new_instruction(
                Mnemonic::STA,
                AddressingMode::AbsoluteY,
                Operand::Absolute(0x0200),
            ),
            Node::new_instruction(Mnemonic::INY, AddressingMode::Implied, Operand::Implied),
            Node::new_instruction(
                Mnemonic::CPY,
                AddressingMode::Immediate,
                Operand::Immediate(0x20),
            ),
            Node::new_instruction(
                Mnemonic::BNE,
                AddressingMode::Relative,
                Operand::Label("secondloop".to_string()),
            ),
        ]
    }

    // ** Happy path cases **
    #[test]
    fn test_resolve_symbols() -> Result<(), SymbolError> {
        let mut symbol_table = SymbolTable::new();
        let ast = example_ast();

        index_labels(&ast, &mut symbol_table)?;
        index_constants(&ast, &mut symbol_table)?;
        let res = verify_symbols(&ast, &symbol_table);
        assert!(res.is_ok());
        assert_eq!(symbol_table.symbols.len(), 4);
        assert_eq!(symbol_table.symbols[0].name, "firstloop");
        assert_eq!(
            symbol_table.symbols[0].symbol,
            SymbolType::Label(0x8000 + 0x07)
        );
        assert_eq!(symbol_table.symbols[1].name, "secondloop");
        assert_eq!(
            symbol_table.symbols[1].symbol,
            SymbolType::Label(0x8000 + 0x12)
        );
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

        Ok(())
    }

    // ** Error cases **
    // Undefined symbols
    #[test]
    fn test_undefined_label() -> Result<(), SymbolError> {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![Node::new_instruction(
            Mnemonic::BNE,
            AddressingMode::Relative,
            Operand::Label("undefined".to_string()),
        )];

        index_labels(&ast, &mut symbol_table)?;
        let res = verify_symbols(&ast, &symbol_table);
        assert_eq!(
            res,
            Err(SymbolError::UndefinedSymbol("undefined".to_string()))
        );
        Ok(())
    }

    #[test]
    fn test_undefined_constant() -> Result<(), SymbolError> {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![Node::new_instruction(
            Mnemonic::LDX,
            AddressingMode::Immediate,
            Operand::Constant("zero".to_string()),
        )];

        index_constants(&ast, &mut symbol_table)?;
        let res = verify_symbols(&ast, &symbol_table);
        assert_eq!(res, Err(SymbolError::UndefinedSymbol("zero".to_string())));

        Ok(())
    }

    // Double definitions
    #[test]
    fn test_double_label_definition() -> Result<(), SymbolError> {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![
            Node::Label("label".to_string()),
            Node::Label("some_other".to_string()),
            Node::Label("label".to_string()),
        ];

        assert_eq!(
            index_labels(&ast, &mut symbol_table),
            Err(SymbolError::SymbolAlreadyDefined("label".to_string()))
        );
        Ok(())
    }

    #[test]
    fn test_double_constant_definition() -> Result<(), SymbolError> {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![
            Node::Constant(Constant::new_byte("my_byte".to_string(), 0x12)),
            Node::Constant(Constant::new_byte("my_word".to_string(), 0x12)),
            Node::Constant(Constant::new_byte("my_byte".to_string(), 0x34)),
        ];

        assert_eq!(
            index_constants(&ast, &mut symbol_table),
            Err(SymbolError::SymbolAlreadyDefined("my_byte".to_string()))
        );

        Ok(())
    }
}
