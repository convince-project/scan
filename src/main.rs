use clap::Parser as ClapParser;
use scan::*;
use scan_fmt_xml::{scan_core::*, ModelBuilder, Parser};
use std::{error::Error, path::PathBuf};

/// A statistical model checker for large concurrent systems
#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path of model's main XML file
    #[arg(value_hint = clap::ValueHint::DirPath)]
    model: PathBuf,
    /// Confidence
    #[arg(short, long, default_value = "0.95")]
    confidence: f64,
    /// Precision or half-width parameter
    #[arg(short, long, default_value = "0.01")]
    precision: f64,
    /// Search for a counterexample
    #[arg(short, long, default_value = "false")]
    counterexample: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let cli = Cli::parse();
    let parser = Parser::parse(cli.model.to_owned())?;
    let scxml_model = ModelBuilder::visit(parser)?;
    let props = Properties {
        guarantees: scxml_model.guarantees.clone(),
        assumes: scxml_model.assumes.clone(),
    };
    let confidence = cli.confidence;
    let precision = cli.precision;
    if cli.counterexample {
        println!(
            "Searching for counterexample with confidence={confidence}, precision={precision}"
        );
        if let Some(trace) = scxml_model
            .model
            .find_counterexample(props, confidence, precision)
        {
            println!("Counterexample trace:");
            scan::print_state(&scxml_model, scxml_model.model.labels());
            for (event, state) in trace {
                print_event(&scxml_model, event);
                print_state(&scxml_model, state);
            }
        } else {
            println!("No counter-example found");
        }
    } else {
        println!("Verifying model with confidence={confidence}, precision={precision}");
        let rate = scxml_model
            .model
            .par_adaptive(&props, confidence, precision);
        println!("Success rate: {rate}");
    }
    Ok(())
}
