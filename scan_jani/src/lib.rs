//! Parser and model builder for SCAN's JANI specification format.

mod builder;
mod parser;
mod tracer;

use anyhow::Context;
pub use builder::JaniModelData;
use builder::build;
use log::info;
use parser::Model;
use scan_core::program_graph::Action;
use scan_core::{MtlOracle, PgModel, Scan};
use std::{fs::File, io::Read, path::Path};
pub use tracer::TracePrinter;

pub type JaniScan = Scan<Action, PgModel, MtlOracle>;

pub fn load(path: &Path, properties: &[String]) -> anyhow::Result<(JaniScan, JaniModelData)> {
    let time = std::time::Instant::now();
    info!(target: "parser", "parsing JANI model file '{}'", path.display());
    // NOTE: See <https://github.com/serde-rs/json/issues/160>
    let mut file =
        File::open(path).with_context(|| format!("failed to open file '{}'", path.display()))?;
    let size = file.metadata().map(|data| data.len()).unwrap_or_default();
    let mut buf = String::new();
    // Reserve enough bytes in buf to avoid reallocation.
    buf.reserve(size as usize);
    file.read_to_string(&mut buf)
        .with_context(|| format!("failed to read file '{}' to string", path.display()))?;
    let jani_model: Model = serde_json::from_str(&buf).with_context(|| {
        format!(
            "failed to parse model specification in '{}'",
            path.display(),
        )
    })?;
    info!("parsing complete in {:?}", time.elapsed());

    let time = std::time::Instant::now();
    info!(target: "build", "building JANI model");
    let (pg_model, oracle, jani_info) = build(jani_model, properties)?;
    info!("building model completed in {:?}", time.elapsed());
    let scan = Scan::new(pg_model, oracle);

    Ok((scan, jani_info))
}
