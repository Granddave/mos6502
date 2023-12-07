use self::symbol_resolver::SymbolTable;
use super::parser::Parser;

mod symbol_resolver;

struct Compiler<'a> {
    parser: Parser<'a>,
    /// Symbol table for labels.
    symbol_table: SymbolTable,
    current_address: u16,
}
