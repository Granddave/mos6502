use ratatui::{prelude::*, widgets::*};

use crate::{
    disassembler::listing,
    emulator::cpu::{STACK_BASE, STACK_PAGE},
};

use super::app::{App, StateValue};

fn style_state_text<T: std::cmp::PartialEq>(state: &StateValue<T>) -> Style {
    if state.has_changed() {
        Style::default().light_yellow().bold()
    } else {
        Style::default()
    }
}

fn registers(app: &mut App) -> Paragraph {
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

    Paragraph::new(text)
        .block(
            Block::default()
                .title("Registers")
                .title_alignment(Alignment::Center)
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded),
        )
        .style(Style::default().fg(Color::Yellow))
        .alignment(Alignment::Left)
}

fn stack_view(app: &mut App) -> Paragraph {
    let sp_addr = STACK_PAGE + app.state().sp.get() as u16;
    let stack_slice = app.memory_slice(sp_addr, STACK_BASE);
    let mut lines: Vec<Line<'_>> = vec![];

    lines.push(Line::raw("Addr   Value"));
    for (ix, val) in stack_slice.iter().rev().enumerate() {
        lines.push(Line::raw(format!(
            "0x{:04x} 0x{:02x}",
            STACK_PAGE as usize + ix,
            val
        )))
    }

    Paragraph::new(lines).block(
        Block::default()
            .title("Stack")
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .style(Style::default().fg(Color::Yellow)),
    )
}

fn disassembly(app: &mut App) -> Paragraph {
    let mut lines: Vec<Line<'_>> = vec![];

    lines.push(Line::raw("  Addr  Hexdump   Instruction"));

    // TODO: Center around program counter.
    //       Would be possible to disassemble the memory instead of the loaded program
    let pc = app.state().pc.get() as usize;
    let start_addr = app.program_start as usize;
    for (offset, node) in app.disassembled_program.iter() {
        let memory_addr = start_addr + offset;

        let line = listing::generate_line(memory_addr, node);
        if memory_addr == pc {
            lines.push(Line::styled(line, Style::default().light_yellow().bold()));
        } else {
            lines.push(Line::raw(line));
        }
    }

    Paragraph::new(lines)
        .scroll((app.disassembly_scroll, 0))
        .block(
            Block::default()
                .title("Disassembly")
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded)
                .style(Style::default().fg(Color::Yellow)),
        )
}

fn top_bar() -> Paragraph<'static> {
    Paragraph::new(vec!["MOS 6502 Emulator".into()])
        .style(Style::default().fg(Color::Yellow).bold())
        .alignment(Alignment::Center)
}

fn bottom_bar() -> Paragraph<'static> {
    Paragraph::new(vec![
        "Press `Esc`, `Ctrl-C` or `q` to stop running.".into(),
        "Press `s` to step and `c` to run continuously until BRK instruction".into(),
        "Press `r` to reset the CPU and memory".into(),
    ])
    .style(Style::default().fg(Color::Yellow).dim())
    .alignment(Alignment::Left)
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
    const REGISTER_LAYOUT_W: u16 = 20;
    let app_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Max(REGISTER_LAYOUT_W), // Registers
            Constraint::Percentage(20),         // Stack
            Constraint::Percentage(50),         // Disassembled code
        ])
        .split(main_layout[1]);

    frame.render_widget(top_bar(), main_layout[0]);
    frame.render_widget(registers(app), app_layout[0]);
    frame.render_widget(stack_view(app), app_layout[1]);
    frame.render_widget(disassembly(app), app_layout[2]);
    frame.render_widget(bottom_bar(), main_layout[2]);
}
