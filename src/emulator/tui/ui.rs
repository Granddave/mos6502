use ratatui::{
    prelude::{Alignment, Frame},
    style::{Color, Style},
    widgets::{Block, BorderType, Borders, Paragraph},
};

use super::app::App;

pub fn render(app: &mut App, frame: &mut Frame) {
    frame.render_widget(
        Paragraph::new(format!(
            "Press `Esc`, `Ctrl-C` or `q` to stop running.\n\
Press `s` to step and `c` to run continuously until BRK instruction\n\
Press `r` to reset the CPU and memory\n\
\n\
{}",
            app.status()
        ))
        .block(
            Block::default()
                .title("6502 Emulator")
                .title_alignment(Alignment::Center)
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded),
        )
        .style(Style::default().fg(Color::Yellow))
        .alignment(Alignment::Left),
        frame.size(),
    )
}
