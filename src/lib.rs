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

mod progress;
mod report;
mod verify;

use std::{path::PathBuf, sync::Arc};

use anyhow::{Context, anyhow, bail};
use clap::{Parser, Subcommand, ValueEnum};
use progress::Bar;
use scan_core::Time;
use verify::VerifyArgs;

/// Supported model specification formats
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Format {
    /// SCXML format
    Scxml,
    /// JANI format
    Jani,
}

/// Possible serialization formats for verification report
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum ReportFormat {
    /// JSON-serialized report
    #[default]
    Json,
}

/// SCAN's available commands
#[derive(Subcommand)]
enum Commands {
    /// Validate the syntactical and semantical correctness of the model
    Validate,
    /// Verify the model
    Verify {
        /// Verify the model
        #[clap(flatten)]
        verify: VerifyArgs,
        /// Print serialized final verification report
        #[arg(long, value_enum)]
        serialize: Option<ReportFormat>,
        /// Print progress bars during verification
        #[arg(long, value_enum)]
        progress: Option<Bar>,
    },
    /// Produce execution traces
    Traces {
        /// How many traces to save
        #[arg(default_value_t = 1)]
        runs: usize,
        /// Max duration of execution (in model-time)
        #[arg(short, long, default_value_t = 10000)]
        duration: Time,
        /// Run the model execution on a single thread
        #[arg(long = "single-thread", default_value_t = false)]
        single_thread: bool,
    },
}

/// A statistical model checker for large concurrent systems
#[derive(Parser)]
#[deny(missing_docs)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Path of model's file or folder
    #[arg(value_hint = clap::ValueHint::AnyPath)]
    model: PathBuf,
    /// Format used to specify the model
    #[arg(short, long, value_enum)]
    format: Option<Format>,
    /// Comma-separated list of properties to verify
    #[arg(short, long, value_delimiter = ',')]
    properties: Vec<String>,
    /// Command to execute on the model
    #[command(subcommand)]
    command: Commands,
}

impl Cli {
    pub fn run(self) -> anyhow::Result<()> {
        if let Some(format) = self.format {
            match format {
                Format::Scxml => self.run_scxml(),
                Format::Jani => self.run_jani(),
            }
        } else if self.model.is_dir() {
            self.run_scxml()
        } else {
            let ext = self
                .model
                .extension()
                .ok_or(anyhow!("file extension unknown"))?;
            match ext
                .to_str()
                .ok_or(anyhow!("file extension not recognized"))?
            {
                "xml" => self.run_scxml(),
                "jani" => self.run_jani(),
                _ => bail!("unsupported file format"),
            }
        }
    }

    fn run_scxml(self) -> anyhow::Result<()> {
        use scan_scxml::*;

        let model = self
            .model
            .file_name()
            .ok_or_else(|| anyhow!("model path unknown"))?
            .to_str()
            .ok_or_else(|| anyhow!("path not valid Unicode"))?;

        let (scan, scxml_model) = load(&self.model, &self.properties)?;

        match self.command {
            Commands::Verify {
                verify,
                serialize,
                progress,
            } => {
                let mut handle = None;
                let properties = if self.properties.is_empty() {
                    &scxml_model.guarantees
                } else {
                    &self.properties
                };
                if let Some(bar) = progress {
                    let model = model.to_owned();
                    let scan = scan.clone();
                    let confidence = verify.confidence;
                    let precision = verify.precision;
                    let properties = properties.clone();
                    handle = Some(std::thread::spawn(move || {
                        bar.print_progress_bar(confidence, precision, properties, scan, model);
                    }));
                }
                let report = verify.verify(model.to_owned(), scan, properties)?;
                if let Some(handle) = handle {
                    handle.join().expect("terminate process");
                }
                if let Some(format) = serialize {
                    match format {
                        ReportFormat::Json => {
                            let report = serde_json::ser::to_string_pretty(&report)
                                .context(anyhow!("failed report serialization"))?;
                            println!("{report}");
                        }
                    }
                } else {
                    // Print final report
                    println!("{report}");
                }
                Ok(())
            }
            Commands::Validate => {
                // At this point the model has already been validated
                println!("model '{model}' successfully validated");
                Ok(())
            }
            Commands::Traces {
                runs,
                duration,
                single_thread,
            } => {
                let scxml_model = Arc::new(scxml_model);
                let tracer = TracePrinter::new(scxml_model);
                scan.trace(runs, tracer, duration, single_thread)?;
                Ok(())
            }
        }
    }

    fn run_jani(self) -> anyhow::Result<()> {
        use scan_jani::*;

        let model = self
            .model
            .file_name()
            .ok_or_else(|| anyhow!("model path unknown"))?
            .to_str()
            .ok_or_else(|| anyhow!("path not valid Unicode"))?;

        let (scan, jani_model) = load(&self.model, &self.properties)?;

        match self.command {
            Commands::Verify {
                verify,
                serialize,
                progress,
            } => {
                let mut handle = None;
                let properties = if self.properties.is_empty() {
                    &jani_model.guarantees
                } else {
                    &self.properties
                };
                if let Some(bar) = progress {
                    let model = model.to_owned();
                    let scan = scan.clone();
                    let confidence = verify.confidence;
                    let precision = verify.precision;
                    let guarantees = properties.clone();
                    handle = Some(std::thread::spawn(move || {
                        bar.print_progress_bar(confidence, precision, guarantees, scan, model);
                    }));
                }
                let report = verify.verify(model.to_owned(), scan, properties)?;
                if let Some(handle) = handle {
                    handle.join().expect("terminate process");
                }
                if let Some(format) = serialize {
                    match format {
                        ReportFormat::Json => {
                            let report = serde_json::ser::to_string_pretty(&report)
                                .context(anyhow!("failed report serialization"))?;
                            println!("{report}");
                        }
                    }
                } else {
                    // Print final report
                    println!("{report}");
                }
                Ok(())
            }
            Commands::Validate => {
                println!("model '{model}' successfully validated");
                Ok(())
            }
            Commands::Traces {
                runs,
                duration,
                single_thread,
            } => {
                let jani_model = Arc::new(jani_model);
                let tracer = TracePrinter::new(jani_model);
                scan.trace(runs, tracer, duration, single_thread)?;
                Ok(())
            }
        }
    }
}
