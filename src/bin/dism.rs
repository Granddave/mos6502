use anyhow::{Context, Result};
use tracing_chrome::ChromeLayerBuilder;
use tracing_subscriber::prelude::*;

use mos6502::disassembler::{disassemble_code, listing::Listing};

#[tracing::instrument]
fn main() -> Result<()> {
    let (chrome_layer, _guard) = ChromeLayerBuilder::new().build();
    tracing_subscriber::registry().with(chrome_layer).init();

    let input = std::env::args()
        .nth(1)
        .with_context(|| "No input file specified")?;
    let bytes = std::fs::read(input).with_context(|| "Unable to read file")?;

    let ast = disassemble_code(&bytes);

    let mut listing = Listing::default(ast);
    println!("{}", listing.generate());

    Ok(())
}
