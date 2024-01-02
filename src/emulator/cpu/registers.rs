use super::STACK_POINTER_START;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Status {
    /// (C) Carry flag, set if the last operation caused an overflow from bit 7 or an
    /// underflow from bit 0.
    pub carry: bool,
    /// (Z) Zero flag, set if the result of the last operation was zero.
    pub zero: bool,
    /// (I) Interrupt disable flag, set if the CPU is not to respond to maskable
    /// interrupts (IRQ).
    pub interrupt_disable: bool,
    /// (D) Decimal mode flag, set if the CPU is in decimal mode.
    pub decimal: bool,
    /// (B) Break command flag, set if a software interrupt (BRK) instruction has
    /// been executed.
    pub break_command: bool,
    /// (V) Overflow flag, set if the last operation caused an overflow from bit 6 or an
    /// underflow from bit 1.
    pub overflow: bool,
    /// (S) Negative/Sign flag, set if the result of the last operation had bit 7 set.
    pub negative: bool,
}

impl From<Status> for u8 {
    fn from(status: Status) -> Self {
        let mut status_byte = 0;
        if status.carry {
            status_byte |= 0b0000_0001;
        }
        if status.zero {
            status_byte |= 0b0000_0010;
        }
        if status.interrupt_disable {
            status_byte |= 0b0000_0100;
        }
        if status.decimal {
            status_byte |= 0b0000_1000;
        }
        if status.break_command {
            status_byte |= 0b0001_0000;
        }
        if status.overflow {
            status_byte |= 0b0100_0000;
        }
        if status.negative {
            status_byte |= 0b1000_0000;
        }
        status_byte
    }
}

impl From<u8> for Status {
    fn from(status_byte: u8) -> Self {
        Self {
            carry: status_byte & 0b0000_0001 != 0,
            zero: status_byte & 0b0000_0010 != 0,
            interrupt_disable: status_byte & 0b0000_0100 != 0,
            decimal: status_byte & 0b0000_1000 != 0,
            break_command: status_byte & 0b0001_0000 != 0,
            overflow: status_byte & 0b0100_0000 != 0,
            negative: status_byte & 0b1000_0000 != 0,
        }
    }
}

#[derive(Debug)]
pub enum Register {
    A,
    X,
    Y,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Registers {
    // CPU registers
    /// Accumulator
    pub a: u8,
    /// X register
    pub x: u8,
    /// Y register
    pub y: u8,
    /// Program counter
    pub pc: u16,
    /// Stack pointer
    pub sp: u8,
    /// Status register
    pub status: Status,
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: STACK_POINTER_START,
            status: Status::default(),
        }
    }
}
