use clap::Parser;
use env_logger::Env;
use scan::Cli;

fn main() -> anyhow::Result<()> {
    let env = Env::default().default_filter_or("off");
    env_logger::Builder::from_env(env).init();
    Cli::parse().run()
}
