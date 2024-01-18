use std::fmt;

use thiserror::Error;

use self::{
    indexing::{index_constants, index_labels, verify_symbols},
    resolve::{resolve_constants_to_values, resolve_labels_to_addr},
};
use crate::ast::{Instruction, AST};

/// Find all symbols in the AST and add them to the symbol table.
mod indexing;

/// Resolving of symbols to values and addresses.
mod resolve;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum SymbolError {
    #[error("Symbol not found: {0}")]
    SymbolNotFound(String),
    #[error("Symbol already defined: {0}")]
    SymbolAlreadyDefined(String),
    #[error("Symbol not defined: {0}")]
    UndefinedSymbol(String),
    #[error("Invalid addressing mode: {0}")]
    InvalidAddressingMode(Instruction),
    #[error("Invalid symbol type for constant operand: {0}")]
    InvalidSymbolTypeForConstantOperand(Instruction),
}

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
    pub fn new_symbol(&mut self, symbol: Symbol) -> Result<(), SymbolError> {
        if self.find_symbol(&symbol.name).is_some() {
            return Err(SymbolError::SymbolAlreadyDefined(symbol.name));
        }
        self.symbols.push(symbol);
        Ok(())
    }
}

#[tracing::instrument]
pub fn resolve_symbols(ast: &mut AST) -> Result<(), SymbolError> {
    let mut symbol_table = SymbolTable::new();

    // We need to resolve constants before the label are resolved.
    // This is due to the fact constants alter memory offsets which labels are dependent on.
    index_constants(ast, &mut symbol_table)?;
    resolve_constants_to_values(ast, &mut symbol_table)?;

    index_labels(ast, &mut symbol_table)?;
    resolve_labels_to_addr(ast, &mut symbol_table)?;

    verify_symbols(ast, &symbol_table)?;

    Ok(())
}
