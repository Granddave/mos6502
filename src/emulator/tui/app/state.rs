#[derive(Debug, Default, Clone)]
pub struct StateValue<T> {
    value: T,
    did_change: bool,
}

impl<T: PartialEq> StateValue<T> {
    pub fn set(&mut self, value: T) {
        if self.value == value {
            return;
        }
        self.value = value;
        self.did_change = true;
    }

    pub fn get(&self) -> T
    where
        T: Copy,
    {
        self.value
    }

    pub fn has_changed(&self) -> bool {
        self.did_change
    }

    pub fn invalidate(&mut self) {
        self.did_change = false;
    }
}

#[derive(Debug, Default, Clone)]
pub struct EmulationState {
    pub a: StateValue<u8>,
    pub x: StateValue<u8>,
    pub y: StateValue<u8>,
    pub pc: StateValue<u16>,
    pub sp: StateValue<u8>,

    // Status register
    pub carry: StateValue<bool>,
    pub zero: StateValue<bool>,
    pub interrupt_disable: StateValue<bool>,
    pub decimal: StateValue<bool>,
    pub break_command: StateValue<bool>,
    pub overflow: StateValue<bool>,
    pub negative: StateValue<bool>,
}

impl EmulationState {
    pub fn invalidate(&mut self) {
        self.a.invalidate();
        self.x.invalidate();
        self.y.invalidate();
        self.pc.invalidate();
        self.sp.invalidate();

        self.carry.invalidate();
        self.zero.invalidate();
        self.interrupt_disable.invalidate();
        self.decimal.invalidate();
        self.break_command.invalidate();
        self.overflow.invalidate();
        self.negative.invalidate();
    }
}
