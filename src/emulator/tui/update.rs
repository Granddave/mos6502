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
        KeyCode::Up => app.scroll_up(),
        KeyCode::Down => app.scroll_down(),
        KeyCode::PageUp => app.scroll_up_page(),
        KeyCode::PageDown => app.scroll_down_page(),
        KeyCode::Left => {
            app.selected_widget = app.selected_widget.prev();
        }
        KeyCode::Right => {
            app.selected_widget = app.selected_widget.next();
        }
        _ => {}
    };
}
