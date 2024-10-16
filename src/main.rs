use clap::Parser as ClapParser;
use indicatif::{ProgressBar, ProgressStyle};
use scan::*;
use scan_fmt_xml::scan_core::*;
use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicU32, Ordering},
        Arc,
    },
};

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
    #[arg(long, default_value = "false")]
    counterexample: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();
    let cli = Cli::parse();
    let scxml_model = scan_fmt_xml::load(&cli.model)?;
    let confidence = cli.confidence;
    let precision = cli.precision;
    if cli.counterexample {
        println!(
            "Searching for counterexample with confidence={confidence}, precision={precision}"
        );
        if let Some(trace) = scxml_model.model.find_counterexample(
            &scxml_model.guarantees,
            &scxml_model.assumes,
            confidence,
            precision,
        ) {
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
        println!(
            "Verifying model '{}' with confidence={confidence} and precision={precision}",
            cli.model
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("???")
        );
        let s = Arc::new(AtomicU32::new(0));
        let f = Arc::new(AtomicU32::new(0));
        let mag = (-precision.log10().floor()) as usize;
        std::thread::scope(|scope| {
            let s0 = Arc::clone(&s);
            let f0 = Arc::clone(&f);
            scope.spawn(move || {
                scxml_model.model.par_adaptive(
                    &scxml_model.guarantees,
                    &scxml_model.assumes,
                    confidence,
                    precision,
                    s0,
                    f0,
                );
            });
            let s1 = Arc::clone(&s);
            let f1 = Arc::clone(&f);
            scope.spawn(move || {
                let mut local_s = 0;
                let mut local_f = 0;
                let mut adaptive = adaptive_bound(local_s, local_f, confidence, precision);
                let style = ProgressStyle::with_template(
                    "[{elapsed_precise}] {percent:>2}% {wide_bar} {msg} ETA: {eta:<5}",
                )
                .unwrap();
                let bar = ProgressBar::new(adaptive.ceil() as u64).with_style(style);
                while adaptive > (local_s + local_f) as f64 {
                    let rate = local_s as f64 / (local_s + local_f) as f64;
                    let derived_precision = derive_precision(local_s, local_f, confidence);
                    bar.set_length(adaptive.ceil() as u64);
                    bar.set_position((local_s + local_f) as u64);
                    bar.set_message(format!("Rate: {rate:.0$}±{1:.0$}", mag, derived_precision));
                    std::thread::sleep(std::time::Duration::from_millis(100));
                    local_s = s1.load(Ordering::Relaxed);
                    local_f = f1.load(Ordering::Relaxed);
                    adaptive = adaptive_bound(local_s, local_f, confidence, precision);
                }
                bar.finish_and_clear();
            });
        });
        let local_s = s.load(Ordering::Relaxed);
        let local_f = f.load(Ordering::Relaxed);
        let rate = local_s as f64 / (local_s + local_f) as f64;
        println!("Success rate: {rate:.0$}", mag);
    }
    Ok(())
}
