//! Parser and model builder for SCAN's SCXML specification format.
//!
//! This crate is part of the [SCAN statistical model checker](https://convince-project.github.io/scan/)

#![forbid(unsafe_code)]

mod builder;
mod parser;
mod tracer;

use std::path::Path;

pub use builder::ScxmlModel;
use log::info;
pub use scan_core;
use scan_core::Scan;
use scan_pmtl::PmtlOracle;
pub use tracer::TracePrinter;

pub fn load(
    path: &Path,
    properties: &[String],
    all_properties: bool,
) -> anyhow::Result<(Scan<PmtlOracle>, ScxmlModel)> {
    let time = std::time::Instant::now();
    info!(target: "parser", "parse SCXML model");
    let parser = parser::Parser::parse(path)?;
    info!("parsing model completed in {:?}", time.elapsed());

    let time = std::time::Instant::now();
    info!(target: "build", "building SCXML model");
    let (cs, oracle, model) = builder::ModelBuilder::build(parser, properties, all_properties)?;
    info!("building model completed in {:?}", time.elapsed());
    let scan = Scan::new(cs, oracle);
    Ok((scan, model))
}
