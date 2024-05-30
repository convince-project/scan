use clap::{Parser as ClapParser, Subcommand};
use scan_fmt_xml::{ModelBuilder, Parser};
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
    Verify {
        /// List test values
        #[arg(short, long)]
        runs: usize,
    },
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
        Commands::Verify { runs: _ } => {
            println!("Verifying model - NOT YET IMPLEMENTED");
        }
        Commands::Parse => {
            println!("Parsing model");
            let model = Parser::parse(cli.model.to_owned())?;
            println!("{model:#?}");
            println!("Model successfully parsed");
        }
        Commands::Build => {
            println!("Building model");
            let model = Parser::parse(cli.model.to_owned())?;
            let model = ModelBuilder::visit(model)?;
            println!("{model:#?}");
            println!("Model successfully built");
        }
        Commands::Execute => {
            println!("Executing model");
            let model = Parser::parse(cli.model.to_owned())?;
            let mut model = ModelBuilder::visit(model)?;
            println!("Transitions list:");
            let mut trans: u32 = 0;
            while let Some((pg_id, action, destination)) = model
                .cs
                .possible_transitions()
                .take(1)
                .collect::<Vec<_>>()
                .pop()
            {
                let pg = model
                    .fsm_names
                    .get(&pg_id)
                    .cloned()
                    .unwrap_or_else(|| format!("{pg_id:?}"));
                trans += 1;
                println!("#{trans:04}: PG {pg} by {action:?} to {destination:?}");
                model.cs.transition(pg_id, action, destination)?;
            }
            println!("Model run to termination");
        }
    }
    Ok(())
}
