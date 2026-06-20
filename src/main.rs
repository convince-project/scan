//! The main binary crate for SCAN statistical model checker.
//!
//! This crate is part of the [SCAN statistical model checker](https://convince-project.github.io/scan/)

#![warn(missing_docs)]
#![forbid(unsafe_code)]

use clap::Parser;
use scan::Cli;

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    env_logger::Builder::new()
        .filter_level(cli.verbosity.log_level_filter())
        .init();
    cli.run()
}
