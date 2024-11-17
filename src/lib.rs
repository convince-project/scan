//! # SCAN (StatistiCal ANalyzer)
//!
//! SCAN is a statistical model checker
//! designed to verify large concurrent systems
//! for which standard verification techniques do not scale.
//!
//! SCAN uses Channel Systems (CS) as models,[^1]
//! and Metric Temporal Logic (MTL) as property specification language.
//!
//! SCAN is being developed to accept models specified in multiple, rich modeling languages.
//! At the moment the following languages are planned or implemented:
//!
//! - [x] [State Chart XML (SCXML)](https://www.w3.org/TR/scxml/).
//! - [ ] [Promela](https://spinroot.com/spin/Man/Manual.html)
//! - [ ] [JANI](https://jani-spec.org/)
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

// TODO list:
// - [ ] use fast hasher for hashmap and hashset
// - [ ] smallvec optimization
// - [ ] multi-threading

mod cli;
mod print_trace;

pub use cli::*;
pub use print_trace::*;
