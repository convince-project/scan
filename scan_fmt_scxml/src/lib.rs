/*This file is not part of our library, we added it to integrate our library with scan. */
//! Parser and model builder for SCAN's CONVINCE-XML specification format.

mod builder;
mod parser;
mod print_trace;

use std::path::Path;

pub use builder::ScxmlModel;
pub use print_trace::TracePrinter;
use rand::rngs::SmallRng;
pub use scan_core;
use scan_core::{
    CsModel, PmtlOracle, Scan,
    channel_system::{CsError, Event},
};

pub type ScxmlScan = Scan<Event, CsError, CsModel<SmallRng>, PmtlOracle>;
pub fn load(path: &Path) -> anyhow::Result<(ScxmlScan, ScxmlModel)> {
    let parser = parser::Parser::parse(path)?;
    let (cs, oracle, model) = builder::ModelBuilder::build(parser)?;
    let scan = Scan::new(cs, oracle);
    Ok((scan, model))
}
