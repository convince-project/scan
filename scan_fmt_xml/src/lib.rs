//! Parser and model builder for SCAN's CONVINCE-XML specification format.

mod builder;
mod parser;

use std::path::Path;

pub use builder::ScxmlModel;
pub use scan_core;

pub fn load(path: &Path) -> anyhow::Result<ScxmlModel> {
    let parser = parser::Parser::parse(path)?;
    builder::ModelBuilder::build(parser)
}
