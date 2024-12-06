//! Parser and model builder for SCAN's JANI specification format.

pub mod jani_parser;
pub mod jani_builder;

pub use jani_parser::Parser;
pub use jani_builder::ModelBuilder;