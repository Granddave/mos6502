use crate::assembler::ast::ASTNode;

pub struct Symbol {
    name: String,
    byte_offset: usize,
}

/// Symbol table for labels.
/// The symbol table is a list of symbols.
/// Each symbol has a name and an address.
///
/// The symbol table is used to resolve labels.
/// The assembler first reads the source code and creates a symbol table.
/// Then it reads the source code again and generates the machine code.
pub struct SymbolTable {
    pub symbols: Vec<Symbol>,
}

pub fn resolve(nodes: &Vec<ASTNode>, symbol_table: &mut SymbolTable) {
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
}
