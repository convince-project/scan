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
use scan_core::{Oracle, Scan, TransitionSystem};
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
    /// Verify properties of the given model
    ///
    /// EXAMPLE: scan PATH/TO/MODEL verify PROPERTY
    /// EXAMPLE: scan PATH/TO/MODEL verify PROPERTY ANOTHER_PROPERTY
    /// EXAMPLE: scan PATH/TO/MODEL verify --all
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
    /// Produce execution traces and save them to file in csv format.
    Trace(TraceArgs),
}

/// A statistical model checker for large concurrent systems.
///
/// SCAN (StatistiCal ANalyzer) is a statistical model checker
/// designed to verify large concurrent systems
/// for which standard verification techniques do not scale.
#[derive(Parser)]
#[deny(missing_docs)]
#[command(version, about, long_about)]
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
        let model = self
            .model
            .file_name()
            .map(|os_str| os_str.to_str().expect("path is valid Unicode"))
            .unwrap_or("model")
            .to_owned();

        if let Some(format) = self.format {
            match format {
                Format::Scxml => self.run_scxml(model),
                Format::Jani => self.run_jani(model),
            }
        } else if self.model.is_dir() {
            self.run_scxml(model)
        } else {
            let ext = self
                .model
                .extension()
                .ok_or(anyhow!("file extension unknown"))?;
            match ext
                .to_str()
                .ok_or(anyhow!("file extension not recognized"))?
            {
                "xml" => self.run_scxml(model),
                "jani" => self.run_jani(model),
                _ => bail!("unsupported file format"),
            }
        }
    }

    fn run_scxml(self, model: String) -> anyhow::Result<()> {
        use scan_scxml::*;

        match self.command {
            Commands::Verify {
                mut args,
                progress,
                json,
            } => {
                args.validate()?;
                eprint!("Processing model...");
                let (scan, scxml_model) = load(&self.model, &args.properties)?;
                eprintln!(" done");
                validate_properties(&args.properties, &scxml_model.guarantees)?;
                if args.all {
                    args.properties = scxml_model.guarantees.clone();
                }
                run_verification(model, args, progress, scan)?.print(json);
            }
            Commands::Validate => {
                eprint!("Processing model, please wait...");
                let (_scan, _scxml_model) = load(&self.model, &[])?;
                // At this point the model has been validated
                eprintln!(" done");
                println!("model '{model}' successfully validated");
            }
            Commands::Trace(args) => {
                eprint!("Processing model, please wait...");
                let (scan, scxml_model) = load(&self.model, &[])?;
                eprintln!(" done");
                let scxml_model = Arc::new(scxml_model);
                let tracer = TracePrinter::new(scxml_model);
                eprint!("Trace computation in progress...");
                args.trace(scan, tracer)?;
                eprintln!(" done");
            }
        }
        Ok(())
    }

    fn run_jani(self, model: String) -> anyhow::Result<()> {
        use scan_jani::*;

        match self.command {
            Commands::Verify {
                mut args,
                progress,
                json,
            } => {
                args.validate()?;
                eprint!("Processing model, please wait...");
                let (scan, jani_model) = load(&self.model, &args.properties)?;
                eprintln!(" done");
                validate_properties(&args.properties, &jani_model.guarantees)?;
                if args.all {
                    args.properties = jani_model.guarantees;
                }
                run_verification(model, args, progress, scan)?.print(json);
            }
            Commands::Validate => {
                eprint!("Processing model, please wait...");
                let (_scan, _jani_model) = load(&self.model, &[])?;
                eprintln!(" done");
                println!("model '{model}' successfully validated");
            }
            Commands::Trace(args) => {
                eprint!("Processing model, please wait...");
                let (scan, jani_model) = load(&self.model, &[])?;
                eprintln!(" done");
                let jani_model = Arc::new(jani_model);
                let tracer = TracePrinter::new(jani_model);
                eprint!("Trace computation in progress...");
                args.trace(scan, tracer)?;
                eprintln!(" done");
            }
        }
        Ok(())
    }
}

fn validate_properties(props: &[String], all_props: &[String]) -> anyhow::Result<()> {
    if let Some(prop) = props.iter().find(|prop| !all_props.contains(prop)) {
        Err(anyhow!(
            "no property named '{prop}' found in model.\n\nHint: maybe it is mispelled?"
        ))
    } else {
        Ok(())
    }
}

fn run_verification<E, Ts, O>(
    model: String,
    args: VerifyArgs,
    progress: Option<Bar>,
    scan: Scan<E, Ts, O>,
) -> anyhow::Result<Report>
where
    Ts: TransitionSystem<E> + 'static,
    E: Clone + Send + Sync + 'static,
    O: Oracle + 'static,
{
    if let Some(bar) = progress {
        let model_clone = model.to_owned();
        let scan_clone = scan.clone();
        let confidence = args.confidence;
        let precision = args.precision;
        let guarantees = args.properties.clone();
        let handle = std::thread::spawn(move || {
            bar.print_progress_bar(confidence, precision, guarantees, scan_clone, model_clone);
        });
        let report = args.verify(model.to_owned(), scan)?;
        handle.join().expect("terminate process");
        Ok(report)
    } else {
        eprint!("Verification in progress...");
        let report = args.verify(model, scan)?;
        eprintln!(" done!");
        Ok(report)
    }
}

// From Clap tutorial <https://docs.rs/clap/latest/clap/_derive/_tutorial/index.html#testing>
#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
