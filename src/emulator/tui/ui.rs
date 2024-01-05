use ratatui::{prelude::*, widgets::*};

use crate::emulator::cpu::STACK_PAGE;

use super::app::{App, StateValue};

fn style_state_text<T: std::cmp::PartialEq>(state: &StateValue<T>) -> Style {
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
            .style(Style::default().fg(Color::Yellow))
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
            STACK_PAGE as usize + ix,
            val
        )))
    }

    frame.render_widget(
        Paragraph::new(lines).block(
            Block::default()
                .title("Stack")
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded)
                .style(Style::default().fg(Color::Yellow)),
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
                .style(Style::default().fg(Color::Yellow)),
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
            Constraint::Max(20), // Registers
            Constraint::Max(20), // Stack
            Constraint::Max(40), // Disassembled code
        ])
        .split(main_layout[1]);

    render_top_bar(app, frame, main_layout[0]);
    render_registers_widget(app, frame, app_layout[0]);
    render_stack_widget(app, frame, app_layout[1]);
    render_disassembly_widget(app, frame, app_layout[2]);
    render_bottom_bar(app, frame, main_layout[2])
}
