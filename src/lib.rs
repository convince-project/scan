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
//! - [x] [JANI](https://jani-spec.org/)
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

mod progress;
mod report;
mod trace;
mod verify;

use std::{path::PathBuf, sync::Arc};

use anyhow::{anyhow, bail};
use clap::{Parser, Subcommand, ValueEnum};
use progress::Bar;
use report::Report;
use scan_core::{Definition, Oracle, Scan, TransitionSystem};
use trace::TraceArgs;
use verify::VerifyArgs;

/// Supported model specification formats.
///
/// SCAN supports different model specification formats.
///
/// WARNING: formats can have varying levels of supports
/// and may not be interpreted as expected.
#[deny(missing_docs)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Format {
    /// SCXML format, passed as either the path to the main file,
    /// if present, or the path of the directory sub-tree containing the model's files.
    ///
    /// SCXML models are composed by separate .scxml files for SCXML automaton,
    /// an .xml file for pMTL properties and,
    /// optionally, an .xml main file describing the model.
    ///
    /// The model can be passed to SCAN as either the path to the main file,
    /// if present, or the path of a directory.
    /// In the latter case, SCAN will attempt to process appropriately
    /// all files in the given directory and its sub-directories.
    Scxml,
    /// JANI format, passed as the path to the .jani file.
    ///
    /// JANI models are composed by a single .jani file.
    ///
    /// The model has to be passed to SCAN as the path to the .jani file.
    ///
    /// WARNING: JANI support is still experimental.
    Jani,
}

/// SCAN's available commands.
#[deny(missing_docs)]
#[derive(Subcommand)]
enum Commands {
    /// Validate the syntactical and semantical correctness of the model, without running it.
    Validate,
    /// Verify properties of the given model.
    /// At least one property has to be verified.
    ///
    /// Examples:
    /// 'scan PATH/TO/MODEL verify PROPERTY' verifies the property PROPERTY over the model.
    /// 'scan PATH/TO/MODEL verify PROPERTY_1 PROPERTY_2' verifies the properties PROPERTY_1 and PROPERTY_2 together over the model.
    /// 'scan PATH/TO/MODEL verify --all' verifies all specified properties together over the model.
    #[clap(verbatim_doc_comment)]
    Verify {
        /// Args for model verification.
        #[clap(flatten)]
        args: VerifyArgs,
        /// Print progress bars during verification.
        ///
        /// By default, when it starts the verification process, SCAN only prints a terse message.
        /// For longer computations, it might be nice to see in real-time how the verification is proceeding.
        /// This flag has SCAN print progress bars and current statistics on the verification process,
        /// and make a best-effort attempt to estimate how long it will take to completion.
        #[arg(long, value_enum)]
        progress: Option<Bar>,
        /// Print JSON-serialized final verification report.
        ///
        /// By default, SCAN prints a user-friendly report at the end of verification.
        /// This flag has the report printed in JSON format instead.
        #[arg(long)]
        json: bool,
    },
    /// Produce execution traces and save them to file in csv format, separating executions that verify the given properties from those that do not.
    /// Executions are always run to completion, regardless of verification outcome.
    /// It is possible to verify no property at all, in which case all executions are successful but still executed to completion.
    ///
    /// Examples:
    /// 'scan PATH/TO/MODEL trace' executes the model once and writes the trace to disk, without verifying any property.
    /// 'scan PATH/TO/MODEL verify PROPERTY_1 PROPERTY_2' executes the model once and writes the trace to disk, classifying it according to verification outcome of the properties PROPERTY_1 and PROPERTY_2 together over the model.
    /// 'scan PATH/TO/MODEL verify --all' executes the model once and writes the trace to disk, and classifying it according to verification outcome of all specified properties together over the model.
    #[clap(verbatim_doc_comment)]
    Trace(TraceArgs),
}

const LONG_ABOUT: &str = "SCAN (StatistiCal ANalyzer) is a statistical model checker \
designed to verify large concurrent systems \
for which standard verification techniques do not scale.";

/// SCAN (StatistiCal ANalyzer) is a statistical model checker
/// designed to verify large concurrent systems
/// for which standard verification techniques do not scale.
#[derive(Parser)]
#[deny(missing_docs)]
#[command(version, about, long_about=LONG_ABOUT)]
pub struct Cli {
    /// Path of model's file or folder,
    /// depending on the used specification format.
    #[arg(value_hint = clap::ValueHint::AnyPath)]
    model: PathBuf,
    /// Format used to specify the model.
    ///
    /// SCAN supports different model specification formats,
    /// and, by default, it attempts to autodetect the correct one,
    /// but this can be specified to resolve ambiguity.
    ///
    /// WARNING: formats can have varying levels of supports
    /// and may not be interpreted as expected.
    #[arg(short, long, value_enum)]
    format: Option<Format>,
    /// Verbose output
    #[command(flatten)]
    pub verbosity: clap_verbosity_flag::Verbosity,
    /// Actions to execute on the model.
    #[command(subcommand)]
    command: Commands,
}

impl Cli {
    pub fn run(self) -> anyhow::Result<()> {
        let model = std::path::absolute(&self.model)?
            .file_name()
            .and_then(std::ffi::OsStr::to_str)
            .unwrap_or("model")
            .to_owned();

        if let Some(format) = self.format {
            match format {
                Format::Scxml => self.run_scxml(&model),
                Format::Jani => self.run_jani(&model),
            }
        } else if self.model.is_dir() {
            self.run_scxml(&model)
        } else {
            let ext = self
                .model
                .extension()
                .ok_or(anyhow!("file extension unknown"))?;
            match ext
                .to_str()
                .ok_or(anyhow!("file extension not recognized"))?
            {
                "xml" => self.run_scxml(&model),
                "jani" => self.run_jani(&model),
                _ => bail!("unsupported file format"),
            }
        }
    }

    fn run_scxml(self, model: &str) -> anyhow::Result<()> {
        use scan_scxml::*;

        match self.command {
            Commands::Verify {
                mut args,
                progress,
                json,
            } => {
                args.validate()?;
                eprint!("Processing {model}...");
                let (scan_def, scxml_model) = load(&self.model, &args.properties, args.all)?;
                eprintln!(" done");
                validate_properties(&args.properties, &scxml_model.guarantees)?;
                if args.all {
                    args.properties = scxml_model.guarantees.clone();
                }
                run_verification(model, &args, progress, &scan_def).print(json);
            }
            Commands::Validate => {
                eprint!("Processing {model}...");
                let (_scan, _scxml_model) = load(&self.model, &[], true)?;
                // At this point the model has been validated
                eprintln!(" done");
                println!("Model {model} successfully validated");
            }
            Commands::Trace(mut args) => {
                eprint!("Processing {model}...");
                let (scan_def, scxml_model) = load(&self.model, &[], args.all)?;
                eprintln!(" done");
                validate_properties(&args.properties, &scxml_model.guarantees)?;
                if args.all {
                    args.properties = scxml_model.guarantees.clone();
                }
                let scxml_model = Arc::new(scxml_model);
                let tracer = TracePrinter::new(scxml_model);
                eprint!("Trace computation in progress...");
                args.trace(&scan_def, tracer);
                eprintln!(" done");
            }
        }
        Ok(())
    }

    fn run_jani(self, model: &str) -> anyhow::Result<()> {
        use scan_jani::*;

        match self.command {
            Commands::Verify {
                mut args,
                progress,
                json,
            } => {
                args.validate()?;
                eprint!("Processing {model}...");
                let properties = args.properties.clone();
                let (scan, jani_model) = load(&self.model, &properties)?;
                eprintln!(" done");
                validate_properties(&args.properties, &jani_model.guarantees)?;
                if args.all {
                    args.properties = jani_model.guarantees;
                }
                run_verification(model, &args, progress, &scan).print(json);
            }
            Commands::Validate => {
                eprint!("Processing {model}...");
                let (_scan, _jani_model) = load(&self.model, &[])?;
                eprintln!(" done");
                println!("Model {model} successfully validated");
            }
            Commands::Trace(args) => {
                args.validate()?;
                eprint!("Processing {model}...");
                let (scan, jani_model) = load(&self.model, &[])?;
                eprintln!(" done");
                let jani_model = Arc::new(jani_model);
                let tracer = TracePrinter::new(jani_model);
                eprint!("Trace computation in progress...");
                args.trace(&scan, tracer);
                eprintln!(" done");
            }
        }
        Ok(())
    }
}

fn validate_properties(props: &[String], all_props: &[String]) -> anyhow::Result<()> {
    if let Some(prop) = props.iter().find(|prop| !all_props.contains(prop)) {
        Err(anyhow!(
            "no property named '{prop}' found in model.\n\nHint: maybe it is misspelled?"
        ))
    } else {
        Ok(())
    }
}

fn run_verification<Event, Ts, O>(
    model: &str,
    args: &VerifyArgs,
    progress: Option<Bar>,
    scan: &Scan<Event, Ts, O>,
) -> Report
where
    Ts: Definition + 'static + Sync,
    for<'def> <Ts as Definition>::I<'def>: TransitionSystem<Event>,
    Event: Clone + Send + Sync + 'static,
    O: Oracle + 'static,
{
    if let Some(bar) = progress {
        eprintln!(
            "Verifying {model} (-p {} -c {} -d {}) {:?}",
            args.precision, args.confidence, args.duration, args.properties
        );
        std::thread::scope(|s| {
            s.spawn(|| {
                bar.print_progress_bar(args.confidence, args.precision, &args.properties, scan);
            });
            args.verify(model.to_owned(), scan)
        })
    } else {
        eprint!(
            "Verifying {model} (-p {} -c {} -d {}) {:?}...",
            args.precision, args.confidence, args.duration, args.properties
        );
        let report = args.verify(model.to_owned(), scan);
        eprintln!(" done");
        report
    }
}

// From Clap tutorial <https://docs.rs/clap/latest/clap/_derive/_tutorial/index.html#testing>
#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
