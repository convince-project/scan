//! Parser and model builder for SCAN's CONVINCE-XML specification format.

mod builder;
mod parser;

pub use builder::ModelBuilder;
pub use parser::Parser;
pub use scan_core;
