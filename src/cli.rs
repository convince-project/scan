use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

use crate::PrintTrace;
use clap::Parser;
use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget, ProgressStyle};
use scan_fmt_xml::scan_core::*;

/// A statistical model checker for large concurrent systems
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Path of model's main XML file
    #[arg(value_hint = clap::ValueHint::DirPath, default_value = ".")]
    model: PathBuf,
    /// Confidence
    #[arg(short, long, default_value = "0.95")]
    confidence: f64,
    /// Precision or half-width parameter
    #[arg(short, long, default_value = "0.01")]
    precision: f64,
    /// Max length of execution trace
    #[arg(short, long, default_value = "1000000")]
    length: usize,
    /// Max duration of execution (in model-time)
    #[arg(short, long, default_value = "10000")]
    duration: Time,
    /// Saves execution traces in gz-compressed csv format
    #[arg(long = "save-traces", default_value = "false")]
    trace: bool,
}

impl Cli {
    pub fn run(&self) -> anyhow::Result<()> {
        let scxml_model = scan_fmt_xml::load(&self.model)?;
        let model_name = self
            .model
            .file_stem()
            .and_then(|s| s.to_str())
            .map_or("model".to_string(), |s| format!("'{s}'"));
        let confidence = self.confidence;
        let precision = self.precision;
        let run_status = scxml_model.model.run_status();
        let bar_state = run_status.clone();
        let bar_model_name = model_name.clone();
        let bar = std::thread::spawn(move || {
            print_progress_bar(bar_model_name, confidence, precision, bar_state)
        });
        if self.trace {
            std::fs::remove_dir_all("./traces").ok();
            std::fs::create_dir("./traces").expect("create traces dir");
            std::fs::create_dir("./traces/.temp").expect("create traces dir");
            std::fs::create_dir("./traces/success").expect("create success dir");
            std::fs::create_dir("./traces/failure").expect("create failure dir");
            std::fs::create_dir("./traces/undetermined").expect("create undetermined dir");
        }
        scxml_model.model.par_adaptive(
            confidence,
            precision,
            self.length,
            self.duration,
            self.trace.then_some(PrintTrace::new(&scxml_model)),
        );
        bar.join().expect("terminate bar process");

        Ok(())
    }
}

fn print_progress_bar(
    model_name: String,
    confidence: f64,
    precision: f64,
    bar_state: Arc<Mutex<RunStatus>>,
) {
    const FINE_BAR: &str = "█▉▊▋▌▍▎▏  ";
    let run_status = bar_state.lock().expect("lock state").clone();
    let name_len = run_status
        .guarantees
        .iter()
        .map(|(s, _)| s.len())
        .max()
        .unwrap_or_default()
        .max(8);

    let bars = MultiProgress::new();
    bars.set_draw_target(ProgressDrawTarget::stderr_with_hz(5));

    // Spinner
    let spinner_style =
        ProgressStyle::with_template("[{elapsed_precise}] [{spinner}] {msg}").unwrap();
    let spinner = ProgressBar::new_spinner()
        .with_style(spinner_style)
        .with_message(format!(
            "SCANning {model_name} (target confidence {confidence}, precision {precision})",
        ));
    let spinner = bars.add(spinner);

    // Progress bar
    let bound = okamoto_bound(confidence, precision);
    let progress_style = ProgressStyle::with_template(
        format!(
            "{{eta:>{}}} ETA [{{bar:40}}] {{percent:>3}}% ({{pos}}/{{len}})",
            name_len - 4
        )
        .as_str(),
    )
    .unwrap()
    .progress_chars("=> ");
    let progress_bar = ProgressBar::new(bound.ceil() as u64)
        .with_style(progress_style)
        .with_position(0)
        .with_message("Rate: N.A. (0/0)".to_string());
    let progress_bar = bars.add(progress_bar);

    // Property bars
    let mut bars_guarantees = Vec::new();
    let prop_style = ProgressStyle::with_template(
        format!("{{prefix:>{name_len}}} [{{bar:40.green.on_red}}] {{percent:>3}}% ({{msg}} fails)")
            .as_str(),
    )
    .unwrap()
    .progress_chars(FINE_BAR);

    // Guarantees property bars
    for (name, _) in run_status.guarantees.iter() {
        let bar = bars.add(
            ProgressBar::new(0)
                .with_style(prop_style.clone())
                .with_position(0)
                .with_prefix(name.clone())
                .with_message("0".to_string()),
        );
        bars_guarantees.push(bar);
    }

    // Overall property bar
    let overall_bar = bars.add(
        ProgressBar::new(0)
            .with_style(prop_style)
            .with_position(0)
            .with_prefix("TOTAL")
            .with_message("0".to_string()),
    );

    bars.set_move_cursor(true);
    loop {
        let run_status = bar_state.lock().expect("lock state").clone();
        let successes = run_status.successes;
        let failures = run_status.failures;
        let runs = (successes + failures) as u64;
        let rate = successes as f64 / runs as f64;
        let guarantees = run_status.guarantees.clone();
        if run_status.running {
            if runs > progress_bar.position() {
                // Status spinner
                spinner.tick();

                let bound = adaptive_bound(rate, confidence, precision);
                // let derived_precision =
                //     derive_precision(run_status.successes, run_status.failures, confidence);
                progress_bar.set_length(bound.ceil() as u64);
                progress_bar.set_position(runs);
                let max_fail = run_status
                    .guarantees
                    .iter()
                    .map(|(_, x)| *x)
                    .max()
                    .unwrap_or_default()
                    .max(1);
                let digits = max_fail.to_string().chars().count();
                for (i, (_, guarantee)) in guarantees.iter().enumerate() {
                    let pos = runs - *guarantee as u64;
                    let bar = &mut bars_guarantees[i];
                    bar.set_position(pos);
                    bar.set_length(runs);
                    bar.set_message(format!("{guarantee:<0$}", digits));
                }

                // Overall property bar
                let overall = guarantees.iter().map(|(_, g)| *g).sum::<u32>() as u64;
                let pos = runs - overall;
                overall_bar.set_position(pos);
                overall_bar.set_length(runs);
                overall_bar.set_message(format!("{overall:<0$}", digits));
            }
        } else {
            bars.set_move_cursor(false);
            spinner.finish_and_clear();
            progress_bar.finish_and_clear();
            bars_guarantees.iter().for_each(|b| b.finish_and_clear());
            overall_bar.finish_and_clear();
            let mag = (precision.log10().abs().ceil() as usize).max(2);
            println!(
                "SCAN results for {model_name} (confidence {confidence}, precision {precision})"
            );
            println!("Completed {runs} runs with {successes} successes, {failures} failures)",);
            for (name, f) in guarantees.iter() {
                println!(
                    "{name:>0$} success rate: {2:.1$}",
                    name_len,
                    mag,
                    ((runs - *f as u64) as f64) / (runs as f64)
                );
            }
            println!("{0:>1$} success rate: {rate:.2$}", "Overall", name_len, mag);
            break;
        }
        // Sleep a while to limit update/refresh rate.
        std::thread::sleep(std::time::Duration::from_millis(180));
    }
    // Magnitude of precision, to round results to sensible number of digits
}
