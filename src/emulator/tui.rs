use event::{Event, EventHandler};
use ratatui::{backend::CrosstermBackend, Terminal};

use app::App;
use update::update;

/// Emulator application
mod app;

/// Terminal event handler
mod event;

/// Terminal user interface
mod terminal;

/// Widget renderer
mod ui;

/// Application updater
mod update;

pub fn exec(bytes: &[u8], program_start: u16) -> anyhow::Result<()> {
    let mut app = App::new(bytes, program_start);

    let backend = CrosstermBackend::new(std::io::stderr());
    let terminal = Terminal::new(backend)?;
    let events = EventHandler::new(250);
    let mut tui = terminal::Terminal::new(terminal, events);
    tui.enter()?;

    while !app.should_quit() {
        tui.draw(&mut app)?;

        match tui.events.next()? {
            Event::Tick => {}
            Event::Key(key_event) => update(&mut app, key_event),
            Event::Mouse(_) => {}
            Event::Resize(_, _) => {}
        };
    }

    tui.exit()?;

    Ok(())
}
