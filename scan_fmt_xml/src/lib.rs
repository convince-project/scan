//! Parser and model builder for SCAN's CONVINCE-XML specification format.

mod builder;
mod parser;

use std::path::Path;

pub use builder::ScxmlModel;
pub use scan_core;

pub fn load(path: &Path) -> anyhow::Result<ScxmlModel> {
    let parser = if path.is_file() {
        parser::Parser::parse(path)
    } else {
        parser::Parser::parse_folder(path)
    }?;
    builder::ModelBuilder::build(parser)
}
