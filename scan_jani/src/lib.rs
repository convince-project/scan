//! Parser and model builder for SCAN's JANI specification format.

mod builder;
mod parser;
mod tracer;

use anyhow::Context;
pub use builder::JaniModelData;
use builder::build;
use log::info;
use parser::Model;
use scan_core::program_graph::{Action, PgError};
use scan_core::{MtlOracle, PgModel, Scan};
use std::{fs::File, path::Path};
pub use tracer::TracePrinter;

pub type JaniScan = Scan<Action, PgError, PgModel, MtlOracle>;

pub fn load(path: &Path) -> anyhow::Result<(JaniScan, JaniModelData)> {
    let time = std::time::Instant::now();
    info!(target: "parser", "parsing JANI model file '{}'", path.display());
    let reader = File::open(path)
        .with_context(|| format!("failed to create reader from file '{}'", path.display()))?;
    let jani_model: Model = serde_json::de::from_reader(reader).with_context(|| {
        format!(
            "failed to parse model specification in '{}'",
            path.display(),
        )
    })?;
    info!("parsing complete in {:?}", time.elapsed());

    let time = std::time::Instant::now();
    info!(target: "build", "building JANI model");
    let (pg_model, oracle, jani_info) = build(jani_model)?;
    info!("building model completed in {:?}", time.elapsed());
    let scan = Scan::new(pg_model, oracle);

    Ok((scan, jani_info))
}
