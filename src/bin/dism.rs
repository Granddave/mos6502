use tracing_chrome::ChromeLayerBuilder;
use tracing_subscriber::prelude::*;

use mos6502::disassembler::{listing::Listing, Disassembler};

#[tracing::instrument]
fn main() {
    let (chrome_layer, _guard) = ChromeLayerBuilder::new().build();
    tracing_subscriber::registry().with(chrome_layer).init();

    let input = std::env::args().nth(1).expect("Filename missing");
    let bytes = std::fs::read(input).expect("Unable to read file");

    let mut disassembler = Disassembler::default(bytes);
    let ast = disassembler.disassemble_code();

    let mut listing = Listing::default(ast);
    println!("{}", listing.generate());
}
