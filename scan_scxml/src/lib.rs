//! Parser and model builder for SCAN's CONVINCE-XML specification format.

mod builder;
mod parser;
mod print_trace;

use std::path::Path;

pub use builder::ScxmlModel;
use log::info;
pub use print_trace::TracePrinter;
use rand::rngs::SmallRng;
pub use scan_core;
use scan_core::{
    CsModel, PmtlOracle, Scan,
    channel_system::{CsError, Event},
};

pub type ScxmlScan = Scan<Event, CsError, CsModel<SmallRng>, PmtlOracle>;
pub fn load(path: &Path) -> anyhow::Result<(ScxmlScan, ScxmlModel)> {
    let time = std::time::Instant::now();
    info!(target: "parser", "parse SCXML model");
    let parser = parser::Parser::parse(path)?;
    info!("parsing model completed in {:?}", time.elapsed());

    let time = std::time::Instant::now();
    info!(target: "build", "building SCXML model");
    let (cs, oracle, model) = builder::ModelBuilder::build(parser)?;
    info!("building model completed in {:?}", time.elapsed());
    let scan = Scan::new(cs, oracle);
    Ok((scan, model))
}
