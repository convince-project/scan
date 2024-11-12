//! Implementation of *program graphs* (PG) and *channel systems* (CS) formalisms[^1]
//! for use in the SCAN model checker.
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

// #![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod channel_system;
mod grammar;
mod model;
mod mtl;
mod pmtl;
pub mod program_graph;
mod transition_system;

pub use grammar::*;
pub use model::*;
pub use mtl::*;
pub use pmtl::*;
pub use transition_system::*;
