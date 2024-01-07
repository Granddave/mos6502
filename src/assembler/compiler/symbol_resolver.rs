use crate::ast::{ConstantValue, Directive, Node, Operand, AST};

use std::fmt;

use super::CompilerError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolType {
    /// Label with an absolute offset into the program
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

    #[tracing::instrument]
    pub fn new_symbol(&mut self, symbol: Symbol) -> Result<(), CompilerError> {
        if self.find_symbol(&symbol.name).is_some() {
            return Err(CompilerError::SymbolAlreadyDefined(symbol.name));
        }
        self.symbols.push(symbol);
        Ok(())
    }
}

/// Resolve labels in the AST to the symbol table.
#[tracing::instrument]
pub fn resolve_labels(ast: &AST, symbol_table: &mut SymbolTable) -> Result<(), CompilerError> {
    let mut current_addr = 0;

    for node in ast.iter() {
        match node {
            Node::Instruction(ins) => {
                current_addr += ins.size();
            }
            Node::Label(label) => symbol_table.new_symbol(Symbol {
                // TODO: Refactor out to helper function that also checks if label is already defined
                name: label.clone(),
                symbol: SymbolType::Label(current_addr),
            })?,
            Node::Directive(directive) => match directive {
                Directive::Origin(org_addr) => current_addr = *org_addr as usize,
            },
            _ => (),
        }
    }

    Ok(())
}

/// Resolve constants in the AST to the symbol table.
#[tracing::instrument]
pub fn resolve_constants(ast: &AST, symbol_table: &mut SymbolTable) -> Result<(), CompilerError> {
    for node in ast.iter() {
        if let Node::Constant(constant) = node {
            symbol_table.new_symbol(Symbol {
                // TODO: Refactor out to helper function that also checks if constant is already defined
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

#[tracing::instrument]
pub fn verify_symbols(ast: &AST, symbol_table: &mut SymbolTable) -> Result<(), CompilerError> {
    // Verify that all symbols used in the AST are defined
    for node in ast {
        if let Node::Instruction(ins) = node {
            match &ins.operand {
                Operand::Label(label_str) => match symbol_table.find_symbol(label_str) {
                    Some(_) => (),
                    None => return Err(CompilerError::UndefinedSymbol(label_str.clone())),
                },
                Operand::Constant(constant_str) => match symbol_table.find_symbol(constant_str) {
                    Some(_) => (),
                    None => return Err(CompilerError::UndefinedSymbol(constant_str.clone())),
                },
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
    fn test_resolve_symbols() -> Result<(), CompilerError> {
        let mut symbol_table = SymbolTable::new();
        let ast = example_ast();

        resolve_labels(&ast, &mut symbol_table)?;
        resolve_constants(&ast, &mut symbol_table)?;
        let res = verify_symbols(&ast, &mut symbol_table);
        assert!(res.is_ok());
        assert_eq!(symbol_table.symbols.len(), 4);
        assert_eq!(symbol_table.symbols[0].name, "firstloop");
        assert_eq!(
            symbol_table.symbols[0].symbol,
            SymbolType::Label(0x8000 + 0x04)
        );
        assert_eq!(symbol_table.symbols[1].name, "secondloop");
        assert_eq!(
            symbol_table.symbols[1].symbol,
            SymbolType::Label(0x8000 + 0x0f)
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
    fn test_undefined_label() -> Result<(), CompilerError> {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![Node::new_instruction(
            Mnemonic::BNE,
            AddressingMode::Relative,
            Operand::Label("undefined".to_string()),
        )];

        resolve_labels(&ast, &mut symbol_table)?;
        let res = verify_symbols(&ast, &mut symbol_table);
        assert_eq!(
            res,
            Err(CompilerError::UndefinedSymbol("undefined".to_string()))
        );
        Ok(())
    }

    #[test]
    fn test_undefined_constant() -> Result<(), CompilerError> {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![Node::new_instruction(
            Mnemonic::LDX,
            AddressingMode::Immediate,
            Operand::Constant("zero".to_string()),
        )];

        resolve_constants(&ast, &mut symbol_table)?;
        let res = verify_symbols(&ast, &mut symbol_table);
        assert_eq!(res, Err(CompilerError::UndefinedSymbol("zero".to_string())));

        Ok(())
    }

    // Double definitions
    #[test]
    fn test_double_label_definition() -> Result<(), CompilerError> {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![
            Node::Label("label".to_string()),
            Node::Label("some_other".to_string()),
            Node::Label("label".to_string()),
        ];

        assert_eq!(
            resolve_labels(&ast, &mut symbol_table),
            Err(CompilerError::SymbolAlreadyDefined("label".to_string()))
        );
        Ok(())
    }

    #[test]
    fn test_double_constant_definition() -> Result<(), CompilerError> {
        let mut symbol_table = SymbolTable::new();
        let ast = vec![
            Node::Constant(Constant::new_byte("my_byte".to_string(), 0x12)),
            Node::Constant(Constant::new_byte("my_word".to_string(), 0x12)),
            Node::Constant(Constant::new_byte("my_byte".to_string(), 0x34)),
        ];

        assert_eq!(
            resolve_constants(&ast, &mut symbol_table),
            Err(CompilerError::SymbolAlreadyDefined("my_byte".to_string()))
        );

        Ok(())
    }
}
