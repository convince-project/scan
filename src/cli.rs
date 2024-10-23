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
        let confidence = self.confidence;
        let precision = self.precision;
        println!("SCANning '{model_name}' (confidence {confidence}; precision {precision})");
        let s = Arc::new(AtomicU32::new(0));
        let f = Arc::new(AtomicU32::new(0));
        let bar_s = s.to_owned();
        let bar_f = f.to_owned();
        std::thread::spawn(move || print_progress_bar(bar_s, bar_f, confidence, precision));
        scxml_model.model.par_adaptive(
            &scxml_model.guarantees,
            &scxml_model.assumes,
            confidence,
            precision,
            s.to_owned(),
            f.to_owned(),
        );
        let s = s.load(Ordering::Relaxed);
        let f = f.load(Ordering::Relaxed);
        let rate = s as f64 / (s + f) as f64;
        let mag = precision.log10().abs().ceil() as usize;
        println!(
            "Success rate {rate:.0$}±{precision} (confidence {confidence})",
            mag
        );
        Ok(())
    }
}

fn print_progress_bar(s: Arc<AtomicU32>, f: Arc<AtomicU32>, confidence: f64, precision: f64) {
    const FINE_BAR: &str = "█▉▊▋▌▍▎▏  ";
    let mut local_s = s.load(Ordering::Relaxed);
    let mut local_f = f.load(Ordering::Relaxed);
    let runs = local_s + local_f;
    let avg = if runs != 0 {
        local_s as f64 / runs as f64
    } else {
        0.5f64
    };
    let mut bound = adaptive_bound(avg, confidence, precision);
    let style = ProgressStyle::with_template(
        "[{elapsed_precise}] {percent:>2}% {wide_bar} {msg} ETA: {eta:<5}",
    )
    .unwrap()
    .progress_chars(FINE_BAR);
    let bar = ProgressBar::new(bound.ceil() as u64).with_style(style);
    // Magnitude of precision, to round results to sensible number of digits
    let mag = precision.log10().abs().ceil() as usize;
    while bound > (local_s + local_f) as f64 {
        // Check if new runs arrived
        if local_s + local_f > bar.position() as u32 {
            let runs = local_s + local_f;
            let avg = local_s as f64 / runs as f64;
            bound = adaptive_bound(avg, confidence, precision);
            bar.set_length(bound.ceil() as u64);
            bar.set_position(runs as u64);
            let derived_precision = derive_precision(local_s, local_f, confidence);
            bar.set_message(format!(
                "Success rate: {avg:.0$}±{derived_precision:.0$}",
                mag
            ));
            bar.tick();
        }
        // Sleep a while to limit update/refresh rate.
        std::thread::sleep(std::time::Duration::from_millis(32));
        local_s = s.load(Ordering::Relaxed);
        local_f = f.load(Ordering::Relaxed);
    }
    bar.finish_and_clear();
}
