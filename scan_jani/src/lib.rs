//! Parser and model builder for SCAN's JANI specification format.

mod builder;
mod parser;

use anyhow::Context;
use builder::{build, JaniModelData};
use log::info;
use parser::{Model, Sync};
use rand::SeedableRng;
use scan_core::CsModel;
use std::{fs::File, path::Path};

pub fn parse(path: &Path) -> anyhow::Result<(CsModel<rand::rngs::SmallRng>, JaniModelData)> {
    info!(target: "parser", "parsing JANI model file '{}'", path.display());
    let reader = File::open(path)
        .with_context(|| format!("failed to create reader from file '{}'", path.display()))?;
    let jani_model: Model = serde_json::de::from_reader(reader).with_context(|| {
        format!(
            "failed to parse model specification in '{}'",
            path.display(),
        )
    })?;

    build(jani_model, rand::rngs::SmallRng::from_os_rng())
}
