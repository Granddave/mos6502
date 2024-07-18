use ratatui::{prelude::*, widgets::*};

use crate::emulator::cpu::STACK_BASE;

use super::app::{widget::AppWidget, App, RunMode};

fn is_selected_style(selected: AppWidget, current_widget: AppWidget) -> Style {
    if selected == current_widget {
        Style::default().fg(Color::Yellow)
    } else {
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::DIM)
    }
}

fn style_active_text(condition: bool) -> Style {
    if condition {
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
            Span::styled(
                format!("{:02x}", state.a.get()),
                style_active_text(state.a.has_changed()),
            ),
        ]
        .into(),
        vec![
            Span::raw("X:  0x"),
            Span::styled(
                format!("{:02x}", state.x.get()),
                style_active_text(state.x.has_changed()),
            ),
        ]
        .into(),
        vec![
            Span::raw("Y:  0x"),
            Span::styled(
                format!("{:02x}", state.y.get()),
                style_active_text(state.y.has_changed()),
            ),
        ]
        .into(),
        vec![
            Span::raw("PC: 0x"),
            Span::styled(
                format!("{:04x}", state.pc.get()),
                style_active_text(state.pc.has_changed()),
            ),
        ]
        .into(),
        vec![
            Span::raw("SP: 0x"),
            Span::styled(
                format!("{:02x}", state.sp.get()),
                style_active_text(state.sp.has_changed()),
            ),
        ]
        .into(),
        vec![
            Span::raw("S:  0b"),
            Span::styled(
                format!("{:1b}", state.negative.get() as u8),
                style_active_text(state.negative.has_changed()),
            ),
            Span::styled(
                format!("{:1b}", state.overflow.get() as u8),
                style_active_text(state.overflow.has_changed()),
            ),
            Span::raw("0"),
            Span::styled(
                format!("{:1b}", state.break_command.get() as u8),
                style_active_text(state.break_command.has_changed()),
            ),
            Span::styled(
                format!("{:1b}", state.decimal.get() as u8),
                style_active_text(state.decimal.has_changed()),
            ),
            Span::styled(
                format!("{:1b}", state.interrupt_disable.get() as u8),
                style_active_text(state.interrupt_disable.has_changed()),
            ),
            Span::styled(
                format!("{:1b}", state.zero.get() as u8),
                style_active_text(state.zero.has_changed()),
            ),
            Span::styled(
                format!("{:1b}", state.carry.get() as u8),
                style_active_text(state.carry.has_changed()),
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
        layout.inner(Margin::new(1, 0)),
        &mut scrollbar_state,
    );
}

fn render_memory_widget(app: &mut App, frame: &mut Frame, layout: Rect) {
    let page = app.memory_page_to_display;
    let page_slice = app.memory_slice(page * 0x0100, (page + 1) * 0x0100);
    let mut lines: Vec<Line<'_>> = vec![];

    // TODO: Move this to a helper function in hexdump module
    let addr_width = 4;
    let stride = 16;
    let mut ix = 0;
    for (stride_ix, stride_slice) in page_slice.chunks(stride).enumerate() {
        let mut curr_line: Vec<Span> = vec![];
        curr_line.push(Span::raw(format!(
            "0x{:0width$x}",
            (page * 0x0100) + stride * stride_ix,
            width = addr_width
        )));
        for bytes in stride_slice.chunks(2) {
            if ix % 2 == 0 {
                curr_line.push(Span::raw(" "));
            }
            let word = u16::from_le_bytes([bytes[0], if bytes.len() == 1 { 0 } else { bytes[1] }]);
            curr_line.push(Span::styled(
                format!("{:04x}", word),
                style_active_text(word != 0),
            ));
            ix += 2;
        }
        lines.push(curr_line.clone().into());
    }

    let page_title = match page {
        0x00 => "Zero Page",
        0x01 => "Stack",
        0x02..=0x7f => "RAM",
        _ => "ROM",
    };

    // Render text
    frame.render_widget(
        Paragraph::new(lines).block(
            Block::default()
                .title(format!("Memory ({})", page_title))
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

fn render_help_text_widget(_app: &mut App, frame: &mut Frame, layout: Rect) {
    frame.render_widget(
        Paragraph::new(vec![
            "Press `Esc`, `Ctrl-C` or `q` to stop running.".into(),
            "Press `s` to step and `c` to run continuously until BRK instruction".into(),
            "Press `r` to reset the CPU and memory".into(),
            // TODO: Add more instructions
        ])
        .style(Style::default().fg(Color::Yellow).dim())
        .alignment(Alignment::Left),
        layout,
    );
}

fn render_simulation_state_widget(app: &mut App, frame: &mut Frame, layout: Rect) {
    let is_step_mode = app.run_mode == RunMode::Step;
    let text: Vec<Line<'_>> = vec![vec![
        Span::raw("Run mode: "),
        Span::styled("Step", style_active_text(is_step_mode)),
        Span::raw("/"),
        Span::styled("Continuous", style_active_text(!is_step_mode)),
    ]
    .into()];
    frame.render_widget(
        Paragraph::new(text)
            .style(Style::default().fg(Color::Yellow))
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
            Constraint::Max(20), // Registers and Stack
            Constraint::Fill(1), // Disassembled code
            Constraint::Max(48), // Memory viewer
        ])
        .split(main_layout[1]);
    let left_app_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Max(9),  // Registers
            Constraint::Fill(1), // Stack
        ])
        .split(app_layout[0]);
    let right_app_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Max(18), // Memory viewer
        ])
        .split(app_layout[2]);
    let bottom_bar_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Fill(2), // Help text
            Constraint::Fill(1), // Simulation state
        ])
        .split(main_layout[2]);

    render_top_bar(app, frame, main_layout[0]);
    render_registers_widget(app, frame, left_app_layout[0]);
    render_stack_widget(app, frame, left_app_layout[1]);
    render_disassembly_widget(app, frame, app_layout[1]);
    render_memory_widget(app, frame, right_app_layout[0]);
    render_help_text_widget(app, frame, bottom_bar_layout[0]);
    render_simulation_state_widget(app, frame, bottom_bar_layout[1]);
}
