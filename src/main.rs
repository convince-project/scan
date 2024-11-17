use clap::Parser;
use scan::Cli;

fn main() {
    env_logger::init();
    let cli = Cli::parse();
    let result = cli.run();
    if let Err(err) = result {
        println!("ERROR: {err}");
    }
}
