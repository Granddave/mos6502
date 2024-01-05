use ratatui::{prelude::*, widgets::*};

use crate::{
    emulator::cpu::STACK_BASE,
    hexdump::{hexdump_1, hexdump_2},
};

use super::app::{App, StateValue};

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

fn is_selected_style(selected: AppWidget, current_widget: AppWidget) -> Style {
    if selected == current_widget {
        Style::default().fg(Color::Yellow)
    } else {
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::DIM)
    }
}

fn style_state_text<T: PartialEq>(state: &StateValue<T>) -> Style {
    if state.has_changed() {
        Style::default().light_yellow().bold()
    } else {
        Style::default()
    }
}

fn render_registers_widget(app: &mut App, frame: &mut Frame, layout: Rect) {
    let state = app.state();

    let text: Vec<Line<'_>> = vec![
        vec![
            Span::raw("A:  0x"),
            Span::styled(format!("{:02x}", state.a.get()), style_state_text(&state.a)),
        ]
        .into(),
        vec![
            Span::raw("X:  0x"),
            Span::styled(format!("{:02x}", state.x.get()), style_state_text(&state.x)),
        ]
        .into(),
        vec![
            Span::raw("Y:  0x"),
            Span::styled(format!("{:02x}", state.y.get()), style_state_text(&state.y)),
        ]
        .into(),
        vec![
            Span::raw("PC: 0x"),
            Span::styled(
                format!("{:04x}", state.pc.get()),
                style_state_text(&state.pc),
            ),
        ]
        .into(),
        vec![
            Span::raw("SP: 0x"),
            Span::styled(
                format!("{:02x}", state.sp.get()),
                style_state_text(&state.sp),
            ),
        ]
        .into(),
        vec![
            Span::raw("S:  0b"),
            Span::styled(
                format!("{:1b}", state.negative.get() as u8),
                style_state_text(&state.negative),
            ),
            Span::styled(
                format!("{:1b}", state.overflow.get() as u8),
                style_state_text(&state.overflow),
            ),
            Span::raw("0"),
            Span::styled(
                format!("{:1b}", state.break_command.get() as u8),
                style_state_text(&state.break_command),
            ),
            Span::styled(
                format!("{:1b}", state.decimal.get() as u8),
                style_state_text(&state.decimal),
            ),
            Span::styled(
                format!("{:1b}", state.interrupt_disable.get() as u8),
                style_state_text(&state.interrupt_disable),
            ),
            Span::styled(
                format!("{:1b}", state.zero.get() as u8),
                style_state_text(&state.zero),
            ),
            Span::styled(
                format!("{:1b}", state.carry.get() as u8),
                style_state_text(&state.carry),
            ),
        ]
        .into(),
        Line::styled("      NV-BDIZC", Style::default().dim()),
    ];

    frame.render_widget(
        Paragraph::new(text)
            .block(
                Block::default()
                    .title("Registers")
                    .title_alignment(Alignment::Center)
                    .borders(Borders::ALL)
                    .border_type(BorderType::Rounded),
            )
            .style(is_selected_style(app.selected_widget, AppWidget::Registers))
            .alignment(Alignment::Left),
        layout,
    );
}

fn render_stack_widget(app: &mut App, frame: &mut Frame, layout: Rect) {
    let stack_slice = app.stack_memory();
    let mut lines: Vec<Line<'_>> = vec![];

    lines.push(Line::raw("Addr   Value"));
    for (ix, val) in stack_slice.iter().rev().enumerate() {
        lines.push(Line::raw(format!(
            "0x{:04x} 0x{:02x}",
            STACK_BASE as usize - ix,
            val
        )))
    }

    frame.render_widget(
        Paragraph::new(lines).block(
            Block::default()
                .title("Stack")
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded)
                .style(is_selected_style(app.selected_widget, AppWidget::Stack)),
        ),
        layout,
    );
}

fn render_disassembly_widget(app: &mut App, frame: &mut Frame, layout: Rect) {
    let pc = app.state().pc.get();
    let lines: Vec<Line> = app
        .disassembled_program
        .iter()
        .map(|(memory_addr, line)| {
            if *memory_addr == pc as usize {
                Line::styled(
                    format!("{:028} <", line),
                    Style::default().light_yellow().bold(),
                )
            } else {
                Line::raw(line)
            }
        })
        .collect();

    app.disassembly_frame_height = layout.height as usize - 2; // subtract border size
    let scroll_pos = app.disassembly_widget_scroll;
    let num_lines = lines.len();

    // Render text
    frame.render_widget(
        Paragraph::new(lines).scroll((scroll_pos as u16, 0)).block(
            Block::default()
                .title("Disassembly")
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded)
                .style(is_selected_style(
                    app.selected_widget,
                    AppWidget::Disassembly,
                )),
        ),
        layout,
    );

    // Render scrollbar
    let mut scrollbar_state = ScrollbarState::new(num_lines).position(scroll_pos);
    frame.render_stateful_widget(
        Scrollbar::default().orientation(ScrollbarOrientation::VerticalRight),
        layout.inner(&Margin::new(1, 0)),
        &mut scrollbar_state,
    );
}

fn render_memory_widget(app: &mut App, frame: &mut Frame, layout: Rect) {
    let page = app.memory_page_to_display as u16;
    let page_slice = app.memory_slice(page * 0x0100, (page + 1) * 0x0100);
    let mut lines: Vec<Line<'_>> = vec![];

    // let hexdump = hexdump_1(memory, 4, 16);
    // for line in hexdump.lines() {
    //     lines.push(Line::raw(line));
    // }

    let mut curr_line: Vec<Span> = vec![];

    let addr_width = 4;
    let stride = 16;
    let mut line = 0;
    let mut ix = 0;
    for bytes in page_slice.chunks(2) {
        if ix % stride == 0 {
            if ix != 0 {
                lines.push(curr_line.clone().into());
                curr_line.clear();
            }
            curr_line.push(Span::raw(format!(
                "0x{:0width$X}",
                page + stride * line + 0x0100,
                width = addr_width
            )));
            line += 1;
        }
        if ix % 2 == 0 {
            curr_line.push(Span::raw(" "));
        }

        curr_line.push(Span::raw(format!(
            "{:04x}",
            u16::from_le_bytes([bytes[0], if bytes.len() == 1 { 0 } else { bytes[1] }])
        )));
        ix += 2;
    }

    // Render text
    frame.render_widget(
        Paragraph::new(lines).block(
            Block::default()
                .title("Memory")
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded)
                .style(is_selected_style(app.selected_widget, AppWidget::Memory)),
        ),
        layout,
    );
}

fn render_top_bar(_app: &mut App, frame: &mut Frame, layout: Rect) {
    frame.render_widget(
        Paragraph::new(vec!["MOS 6502 Emulator".into()])
            .style(Style::default().fg(Color::Yellow).bold())
            .alignment(Alignment::Center),
        layout,
    );
}

fn render_bottom_bar(_app: &mut App, frame: &mut Frame, layout: Rect) {
    frame.render_widget(
        Paragraph::new(vec![
            "Press `Esc`, `Ctrl-C` or `q` to stop running.".into(),
            "Press `s` to step and `c` to run continuously until BRK instruction".into(),
            "Press `r` to reset the CPU and memory".into(),
        ])
        .style(Style::default().fg(Color::Yellow).dim())
        .alignment(Alignment::Left),
        layout,
    );
}

pub fn render(app: &mut App, frame: &mut Frame) {
    let main_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Max(1), // Top bar
            Constraint::Min(1), // App layout
            Constraint::Max(3), // Bottom bar
        ])
        .split(frame.size());

    // App layout
    let app_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Max(20),        // Registers
            Constraint::Max(20),        // Stack
            Constraint::Max(40),        // Disassembled code
            Constraint::Percentage(20), // Memory viewer
        ])
        .split(main_layout[1]);

    render_top_bar(app, frame, main_layout[0]);
    render_registers_widget(app, frame, app_layout[0]);
    render_stack_widget(app, frame, app_layout[1]);
    render_disassembly_widget(app, frame, app_layout[2]);
    render_memory_widget(app, frame, app_layout[3]);
    render_bottom_bar(app, frame, main_layout[2])
}
