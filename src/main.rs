use clap::{Parser as ClapParser, Subcommand};
use log::{info, trace};
use rand::seq::IteratorRandom;
use scan_fmt_xml::{scan_core::*, ModelBuilder, Parser};
use std::{error::Error, path::PathBuf};

/// A statistical model checker for large concurrent systems
#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Action to perform on model
    #[command(subcommand)]
    command: Commands,
    /// Path of model's main XML file
    model: PathBuf,
}

#[derive(Subcommand)]
enum Commands {
    /// Verify model (WIP)
    Verify,
    // {
    //     /// List test values
    //     #[arg(short, long)]
    //     runs: usize,
    // },
    /// Parse and print model XML file
    Parse,
    /// Build and print CS model from XML file
    Build,
    /// Execute model once and prints transitions
    Execute,
}

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let cli = Cli::parse();
    match &cli.command {
        Commands::Verify => {
            println!("Verifying model");
            let parser = Parser::parse(cli.model.to_owned())?;
            let scxml_model = ModelBuilder::visit(parser)?;
            let props = Properties {
                guarantees: scxml_model.guarantees,
                assumes: scxml_model.assumes,
            };
            let result = scxml_model.model.check_statistics(props);
            if let Some(trace) = result {
                println!("COUNTER-EXAMPLE:\n{trace:?}");
            } else {
                println!("No counter-example found");
            }
        }
        Commands::Parse => {
            println!("Parsing model");
            let parser = Parser::parse(cli.model.to_owned())?;
            println!("{parser:#?}");
            println!("Model successfully parsed");
        }
        Commands::Build => {
            println!("Building model");
            let parser = Parser::parse(cli.model.to_owned())?;
            let model = ModelBuilder::visit(parser)?;
            println!("{model:#?}");
            println!("Model successfully built");
        }
        Commands::Execute => {
            println!("Executing model");
            let mut rng = rand::thread_rng();
            let parser = Parser::parse(cli.model.to_owned())?;
            let model = ModelBuilder::visit(parser)?;
            let mut trans: u32 = 0;
            let mut events: u32 = 0;
            let mut cs = model.model.channel_system().to_owned();
            while let Some((pg_id, action, destination)) =
                cs.possible_transitions().choose(&mut rng)
            {
                let pg = model
                    .fsm_names
                    .get(&pg_id)
                    .cloned()
                    .unwrap_or_else(|| format!("{pg_id:?}"));
                trans += 1;
                trace!("TRS #{trans:05}: {pg} transition by {action:?} to {destination:?}");
                if let Some(event) = cs.transition(pg_id, action, destination)? {
                    events += 1;
                    info!(
                        "MSG #{events:05}: {pg} message {:?} on {:?}",
                        event.event_type, event.channel,
                    );
                }
            }
            println!("Model run to termination");
        }
    }
    Ok(())
}
