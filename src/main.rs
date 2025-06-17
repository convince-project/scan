use clap::Parser;
use scan::Cli;

fn main() -> anyhow::Result<()> {
    human_panic::setup_panic!();
    let cli = Cli::parse();
    env_logger::Builder::new()
        .filter_level(cli.verbosity.log_level_filter())
        .init();
    cli.run()
}
