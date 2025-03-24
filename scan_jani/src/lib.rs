//! Parser and model builder for SCAN's JANI specification format.

pub mod jani_builder;
pub mod jani_parser;
mod parser;

use anyhow::Context;
use parser::Model;
use std::{fs::File, path::Path};

pub use jani_builder::ModelBuilder;
pub use jani_parser::Parser;
use log::info;

pub fn parse(path: &Path) -> anyhow::Result<()> {
    info!(target: "parser", "parsing JANI model file '{}'", path.display());
    let reader = File::open(path)
        .with_context(|| format!("failed to create reader from file '{}'", path.display()))?;
    let _model: Model = serde_json::de::from_reader(reader).with_context(|| {
        format!(
            "failed to parse model specification in '{}'",
            path.display(),
        )
    })?;

    Ok(())
}
