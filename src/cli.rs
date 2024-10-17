use clap::Parser;
use indicatif::{ProgressBar, ProgressStyle};
use scan::*;
use scan_fmt_xml::{scan_core::*, ScxmlModel};
use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicU32, Ordering},
        Arc,
    },
};

/// A statistical model checker for large concurrent systems
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
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

impl Cli {
    pub fn run(&self) -> Result<(), Box<dyn std::error::Error>> {
        let scxml_model = scan_fmt_xml::load(&self.model)?;
        if self.counterexample {
            self.counterexample(scxml_model)
        } else {
            self.verify(scxml_model)
        }
    }

    fn counterexample(&self, scxml_model: ScxmlModel) -> Result<(), Box<dyn std::error::Error>> {
        println!(
            "Searching for counterexample with confidence={}, precision={}",
            self.confidence, self.precision
        );
        if let Some(trace) = scxml_model.model.find_counterexample(
            &scxml_model.guarantees,
            &scxml_model.assumes,
            self.confidence,
            self.precision,
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
        Ok(())
    }

    fn verify(&self, scxml_model: ScxmlModel) -> Result<(), Box<dyn std::error::Error>> {
        let model_name = self
            .model
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("???");
        println!(
            "Verifying model '{model_name}' with confidence {} and precision {}",
            self.confidence, self.precision
        );
        let s = Arc::new(AtomicU32::new(0));
        let f = Arc::new(AtomicU32::new(0));
        let mag = (-self.precision.log10().floor()) as usize;
        std::thread::scope(|scope| {
            let s0 = Arc::clone(&s);
            let f0 = Arc::clone(&f);
            scope.spawn(move || {
                scxml_model.model.par_adaptive(
                    &scxml_model.guarantees,
                    &scxml_model.assumes,
                    self.confidence,
                    self.precision,
                    s0,
                    f0,
                );
            });
            let s1 = Arc::clone(&s);
            let f1 = Arc::clone(&f);
            scope.spawn(move || {
                let mut local_s = 0;
                let mut local_f = 0;
                let mut bound = okamoto_bound(self.confidence, self.precision);
                let style = ProgressStyle::with_template(
                    "[{elapsed_precise}] {percent:>2}% {wide_bar} {msg} ETA: {eta:<5}",
                )
                .unwrap();
                let bar = ProgressBar::new(bound.ceil() as u64).with_style(style);
                while bound > (local_s + local_f) as f64 {
                    let rate = local_s as f64 / (local_s + local_f) as f64;
                    let derived_precision = derive_precision(local_s, local_f, self.confidence);
                    bar.set_length(bound.ceil() as u64);
                    bar.set_position((local_s + local_f) as u64);
                    bar.set_message(format!("Rate: {rate:.1$}±{:.1$}", derived_precision, mag));
                    // Sleep a while to limit update/refresh rate.
                    std::thread::sleep(std::time::Duration::from_millis(100));
                    local_s = s1.load(Ordering::Relaxed);
                    local_f = f1.load(Ordering::Relaxed);
                    bound = adaptive_bound(local_s, local_f, self.confidence, self.precision);
                }
                bar.finish_and_clear();
            });
        });
        let local_s = s.load(Ordering::Relaxed);
        let local_f = f.load(Ordering::Relaxed);
        let rate = local_s as f64 / (local_s + local_f) as f64;
        println!("Success rate {rate:.0$}", mag);
        Ok(())
    }
}
