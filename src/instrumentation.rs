use tracing_chrome::{ChromeLayerBuilder, FlushGuard};
use tracing_subscriber::prelude::*;

/// Enable tracing to chrome://tracing or https://ui.perfetto.dev/
///
/// Make sure to store the guard in a variable in the scope to be instrumented, otherwise the trace
/// will be disabled immediately.
pub fn trace() -> FlushGuard {
    let (chrome_layer, _guard) = ChromeLayerBuilder::new().build();
    tracing_subscriber::registry().with(chrome_layer).init();
    return _guard;
}
