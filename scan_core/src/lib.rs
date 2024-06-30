//! Implementation of *program graphs* (PG) and *channel systems* (CS) formalisms[^1]
//! for use in the SCAN model checker.
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod channel_system;
mod grammar;
pub mod program_graph;

pub use grammar::*;
