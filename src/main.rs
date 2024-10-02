use clap::{Parser as ClapParser, Subcommand};
use log::{info, trace};
use rand::seq::IteratorRandom;
use scan_fmt_xml::{scan_core::*, ModelBuilder, Parser, ScxmlModel};
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
                guarantees: scxml_model.guarantees.clone(),
                assumes: scxml_model.assumes.clone(),
            };
            let result = scxml_model.model.check_statistics(props);
            if let Some(trace) = result {
                println!("COUNTER-EXAMPLE:");
                print_state(&scxml_model, scxml_model.model.labels());
                for (event, state) in trace {
                    print_event(&scxml_model, event);
                    print_state(&scxml_model, state);
                }
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

fn print_state(scxml_model: &ScxmlModel, state: Vec<bool>) {
    let mut first = true;
    for (pred, val) in scxml_model.predicates.iter().zip(&state) {
        if first {
            print!("[{pred}: {val:5}");
            first = false;
        } else {
            print!(" | {pred}: {val:5}");
        }
    }
    println!("]");
}

fn print_event(scxml_model: &ScxmlModel, event: channel_system::Event) {
    print!("{}", scxml_model.fsm_names.get(&event.pg_id).unwrap());
    if let Some((src, trg, event_idx, param)) = scxml_model.parameters.get(&event.channel) {
        let event_name = scxml_model.events.get(event_idx).unwrap();
        match event.event_type {
            channel_system::EventType::Send(val) => println!(
                " sends param {param}={val:?} of event {event_name} to {}",
                scxml_model.fsm_names.get(trg).unwrap()
            ),
            channel_system::EventType::Receive(val) => println!(
                " receives param {param}={val:?} from {}",
                scxml_model.fsm_names.get(src).unwrap()
            ),
            _ => unreachable!(),
        }
    } else if let Some(pg_id) = scxml_model.ext_queues.get(&event.channel) {
        match event.event_type {
            channel_system::EventType::Send(val) => {
                if let Val::Tuple(e) = val {
                    if let (Val::Integer(sent_event), Val::Integer(_origin)) = (&e[0], &e[1]) {
                        println!(
                            " sends event {} to {}",
                            scxml_model.events.get(&(*sent_event as usize)).unwrap(),
                            scxml_model.fsm_names.get(pg_id).unwrap()
                        );
                    } else {
                        panic!("events should be pairs");
                    }
                } else {
                    panic!("events should be pairs");
                }
            }
            channel_system::EventType::Receive(val) => {
                if let Val::Tuple(e) = val {
                    if let (Val::Integer(sent_event), Val::Integer(origin)) = (&e[0], &e[1]) {
                        println!(
                            " receives event {} from {}",
                            scxml_model.events.get(&(*sent_event as usize)).unwrap(),
                            scxml_model.fsm_indexes.get(&(*origin as usize)).unwrap(),
                        );
                    } else {
                        panic!("events should be pairs");
                    }
                } else {
                    panic!("events should be pairs");
                }
            }
            _ => unreachable!(),
        }
    } else if scxml_model.int_queues.contains(&event.channel) {
        match event.event_type {
            channel_system::EventType::Send(val) => {
                println!(" sends event {val:?} to its internal queue")
            }
            channel_system::EventType::Receive(val) => {
                println!(" processes event {val:?} from its internal queue")
            }
            channel_system::EventType::ProbeEmptyQueue => {
                println!("'s internal queue is empty")
            }
            _ => unreachable!(),
        }
    } else {
        match event.event_type {
            channel_system::EventType::Send(val) => {
                println!(" sends {val:?} to {:?}", event.channel)
            }
            channel_system::EventType::Receive(val) => {
                println!(" receives {val:?} from {:?}", event.channel)
            }
            channel_system::EventType::ProbeEmptyQueue => {
                println!(" probes that {:?} is empty", event.channel)
            }
            channel_system::EventType::ProbeFullQueue => {
                println!(" probes that {:?} is full", event.channel)
            }
        }
    }
}
