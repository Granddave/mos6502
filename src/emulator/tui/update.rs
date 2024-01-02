use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use super::app::App;

pub fn update(app: &mut App, key_event: KeyEvent) {
    match key_event.code {
        KeyCode::Esc | KeyCode::Char('q') => app.quit(),
        KeyCode::Char('c') => {
            if key_event.modifiers == KeyModifiers::CONTROL {
                app.quit()
            } else {
                app.continue_execution();
            }
        }
        KeyCode::Char('s') => app.step_cpu(),
        KeyCode::Char('r') => app.reset(),
        _ => {}
    };
}
