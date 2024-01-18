use crate::ast::{AddressingMode, Instruction, Operand, AST};

use super::{SymbolError, SymbolTable, SymbolType};

fn resolve_label_to_addr(
    ins: &mut Instruction,
    symbol_table: &mut SymbolTable,
    current_addr: usize,
) -> Result<(), SymbolError> {
    if let Operand::Label(label_operand) = &ins.operand {
        let label_symbol = match symbol_table.find_symbol(label_operand) {
            Some(symbol) => symbol,
            None => return Err(SymbolError::SymbolNotFound(label_operand.clone())),
        };

        if let SymbolType::Label(absolute_offset_in_program) = label_symbol.symbol {
            match ins.addr_mode {
                AddressingMode::Absolute => {
                    ins.operand = Operand::Absolute(absolute_offset_in_program as u16);
                }
                AddressingMode::Relative => {
                    let offset_addr =
                        (absolute_offset_in_program as u16).wrapping_sub(current_addr as u16) as i8;
                    ins.operand = Operand::Relative(offset_addr);
                }
                _ => {
                    return Err(SymbolError::InvalidAddressingMode(ins.clone()));
                }
            }
        }
    }

    Ok(())
}

/// Resolve labels to absolute and relative addresses. This is done by looking up the label in
/// the symbol table and replacing the label with the address of the label.
#[tracing::instrument]
pub fn resolve_labels_to_addr(
    ast: &mut AST,
    symbol_table: &mut SymbolTable,
) -> Result<(), SymbolError> {
    let mut current_addr = 0;

    for ins in ast.iter_mut().filter_map(|node| node.get_instruction()) {
        // The current address is pointing to the address of the next instruction.
        // The relative offset is calculated from the address of the following
        // instruction due to the fact that the CPU has already incremented the
        // program counter past the current instruction.
        //
        // TODO: Handle .org directives
        current_addr += ins.size();
        resolve_label_to_addr(ins, symbol_table, current_addr)?;
    }

    Ok(())
}

#[tracing::instrument]
pub fn resolve_constants_to_values(
    ast: &mut AST,
    symbol_table: &mut SymbolTable,
) -> Result<(), SymbolError> {
    for ins in ast.iter_mut().filter_map(|node| node.get_instruction()) {
        if let Operand::Constant(constant) = &ins.operand {
            let symbol = match symbol_table.find_symbol(constant) {
                Some(symbol) => symbol,
                None => {
                    return Err(SymbolError::SymbolNotFound(constant.clone()));
                }
            };

            match symbol.symbol {
                SymbolType::ConstantByte(byte) => match ins.addr_mode {
                    AddressingMode::Immediate => {
                        ins.operand = Operand::Immediate(byte);
                    }
                    AddressingMode::ZeroPageX
                    | AddressingMode::ZeroPageY
                    | AddressingMode::IndirectIndexedX
                    | AddressingMode::IndirectIndexedY => {
                        ins.operand = Operand::ZeroPage(byte);
                    }
                    AddressingMode::Constant => {
                        // Special case for the zeropage addressing mode since we at the
                        // parsing stage don't know if the operand is a byte or word.
                        ins.operand = Operand::ZeroPage(byte);
                        ins.addr_mode = AddressingMode::ZeroPage;
                    }
                    _ => {
                        return Err(SymbolError::InvalidAddressingMode(ins.clone()));
                    }
                },
                SymbolType::ConstantWord(word) => match ins.addr_mode {
                    AddressingMode::Constant => {
                        // Special case for the absolute addressing mode since we at the
                        // parsing stage don't know if the operand is a byte or word.
                        ins.operand = Operand::Absolute(word);
                        ins.addr_mode = AddressingMode::Absolute;
                    }
                    _ => {
                        return Err(SymbolError::InvalidAddressingMode(ins.clone()));
                    }
                },
                _ => {
                    return Err(SymbolError::InvalidSymbolTypeForConstantOperand(
                        ins.clone(),
                    ))
                }
            }
        }
    }

    Ok(())
}
