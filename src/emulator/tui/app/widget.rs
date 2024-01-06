#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub enum AppWidget {
    Registers,
    Stack,
    #[default]
    Disassembly,
    Memory,
}

impl AppWidget {
    pub fn next(&self) -> AppWidget {
        match self {
            AppWidget::Registers => AppWidget::Stack,
            AppWidget::Stack => AppWidget::Disassembly,
            AppWidget::Disassembly => AppWidget::Memory,
            AppWidget::Memory => AppWidget::Memory, // Don't wrap
        }
    }

    pub fn prev(&self) -> AppWidget {
        match self {
            AppWidget::Registers => AppWidget::Registers, // Don't wrap
            AppWidget::Stack => AppWidget::Registers,
            AppWidget::Disassembly => AppWidget::Stack,
            AppWidget::Memory => AppWidget::Disassembly,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_widget_cycle() {
        let mut w = AppWidget::default();
        assert_eq!(w, AppWidget::Disassembly);
        w = w.next();
        assert_eq!(w, AppWidget::Memory);
        w = w.next();
        assert_eq!(w, AppWidget::Memory);
        w = w.prev();
        assert_eq!(w, AppWidget::Disassembly);
        w = w.prev();
        assert_eq!(w, AppWidget::Stack);
        w = w.prev();
        assert_eq!(w, AppWidget::Registers);
        w = w.prev();
        assert_eq!(w, AppWidget::Registers);
        w = w.next();
        assert_eq!(w, AppWidget::Stack);
    }
}
