//! Past-Metric Temporal Logic (pMTL) oracle.
//!
//! This crate is part of the [SCAN statistical model checker](https://convince-project.github.io/scan/)

#![warn(missing_docs)]
#![forbid(unsafe_code)]

mod oracle;

pub use oracle::{Pmtl, PmtlOracle};
