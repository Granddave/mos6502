use ratatui::{prelude::*, widgets::*};
// use ratatui::{
//     prelude::{Alignment, Frame},
//     style::{Color, Style},
//     text::Span,
//     widgets::{Block, BorderType, Borders, Paragraph},
// };

use super::app::{App, StateValue};

fn style_state_text<T: std::cmp::PartialEq>(state: &StateValue<T>) -> Style {
    if state.has_changed() {
        Style::default().light_yellow().bold()
    } else {
        Style::default()
    }
}

fn registers(app: &mut App) -> Vec<Line> {
    let state = app.state();

    let text: Vec<Line<'_>> = vec![
        vec![
            Span::raw("A:  "),
            Span::styled(
                format!("0x{:02x}", state.a.get()),
                style_state_text(&state.a),
            ),
        ]
        .into(),
        vec![
            Span::raw("X:  "),
            Span::styled(
                format!("0x{:02x}", state.x.get()),
                style_state_text(&state.x),
            ),
        ]
        .into(),
        vec![
            Span::raw("Y:  "),
            Span::styled(
                format!("0x{:02x}", state.y.get()),
                style_state_text(&state.y),
            ),
        ]
        .into(),
        vec![
            Span::raw("PC: "),
            Span::styled(
                format!("0x{:04x}", state.pc.get()),
                style_state_text(&state.pc),
            ),
        ]
        .into(),
        vec![
            Span::raw("SP: "),
            Span::styled(
                format!("0x{:02x}", state.sp.get()),
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
        Line::styled("      NV-BDIZC", Style::default().italic()),
    ];

    text
}

pub fn render(app: &mut App, frame: &mut Frame) {
    let main_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Max(1), Constraint::Min(1), Constraint::Max(3)])
        .split(frame.size());

    // Main title
    frame.render_widget(
        Paragraph::new("6502 Emulator")
            .style(Style::default().fg(Color::Yellow).italic().bold())
            .alignment(Alignment::Center),
        main_layout[0],
    );

    // App layout
    const REGISTER_LAYOUT_W: u16 = 20;
    let app_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Max(REGISTER_LAYOUT_W),
            Constraint::Min(REGISTER_LAYOUT_W),
        ])
        .split(main_layout[1]);

    // Registers widget
    frame.render_widget(
        Paragraph::new(registers(app))
            .block(
                Block::default()
                    .title("Registers")
                    .title_alignment(Alignment::Center)
                    .borders(Borders::ALL)
                    .border_type(BorderType::Rounded),
            )
            .style(Style::default().fg(Color::Yellow))
            .alignment(Alignment::Left),
        app_layout[0],
    );

    // Bottom help text
    frame.render_widget(
        Paragraph::new(vec![
            "Press `Esc`, `Ctrl-C` or `q` to stop running.".into(),
            "Press `s` to step and `c` to run continuously until BRK instruction".into(),
            "Press `r` to reset the CPU and memory".into(),
        ])
        .style(Style::default().fg(Color::Yellow).dim())
        .alignment(Alignment::Left),
        main_layout[2],
    );

    // output.push_str("------------");
    // output.push_str("Stack:");
    // self.memory.dump(
    //     STACK_BASE + self.cpu.stack_pointer() as u16,
    //     STACK_BASE + STACK_POINTER_START as u16,
    // );
}
