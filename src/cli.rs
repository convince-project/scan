use clap::{Parser, ValueEnum};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use scan_fmt_xml::TracePrinter;
use scan_fmt_xml::scan_core::{Scan, adaptive_bound, okamoto_bound};
use std::{path::PathBuf, sync::Arc};

/// Supported model specification formats
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Format {
    /// SCXML format
    Scxml,
    /// JANI format
    Jani,
}

/// A statistical model checker for large concurrent systems
#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Path of model's main XML file
    #[arg(value_hint = clap::ValueHint::DirPath, default_value = ".")]
    path: PathBuf,
    /// Format used to specify the model
    #[arg(value_enum, short, long, default_value = "scxml")]
    format: Format,
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
    duration: u32,
    /// Saves execution traces in gz-compressed csv format
    #[arg(long = "traces", default_value = "false")]
    traces: bool,
    /// ASCII compatible output
    #[arg(long, default_value = "false")]
    ascii: bool,
}

impl Cli {
    pub fn run(&self) -> anyhow::Result<()> {
        match self.format {
            Format::Scxml => self.run_scxml(),
            Format::Jani => self.run_jani(),
        }
    }

    fn run_scxml(&self) -> anyhow::Result<()> {
        let (scan, scxml_model) = scan_fmt_xml::load(&self.path)?;
        let scxml_model = Arc::new(scxml_model);
        let confidence = self.confidence;
        let precision = self.precision;
        let length = self.length;
        let duration = self.duration;
        let tracer = if self.traces {
            Some(TracePrinter::new(scxml_model.clone()))
        } else {
            None
        };
        scan.adaptive(confidence, precision, duration, tracer);
        self.print_progress_bar(&scxml_model.guarantees, &scan);
        // check.join().expect("terminate bar process");

        Ok(())
    }

    fn run_jani(&self) -> anyhow::Result<()> {
        use scan_jani::*;

        let (cs_model, _model) = parse(&self.path)?;
        let confidence = self.confidence;
        let precision = self.precision;
        let length = self.length;
        let duration = self.duration;
        // let bar_state = Arc::clone(&cs_model.run_status());
        // TODO: JANI needs a tracer too
        let tracer: Option<TracePrinter> = None;
        let check = std::thread::spawn(move || {
            // cs_model.par_adaptive(confidence, precision, length, duration, tracer);
        });
        // self.print_progress_bar(&[], bar_state);
        check.join().expect("terminate bar process");

        Ok(())
    }

    fn print_progress_bar<E, Err, Ts>(&self, guarantee_names: &[String], scan: &Scan<E, Err, Ts>)
    where
        Ts: scan_fmt_xml::scan_core::TransitionSystem<E, Err> + 'static,
        Err: std::error::Error + Send + Sync,
        E: Send + Sync,
    {
        const FINE_BAR: &str = "█▉▊▋▌▍▎▏  ";
        const ASCII_BAR: &str = "#--";
        const ASCII_SPINNER: &str = "|/-\\";

        let model_name = self
            .path
            .file_stem()
            .and_then(|s| s.to_str())
            .map_or("model".to_string(), |s| format!("'{s}'"));

        let bars = MultiProgress::new();

        // Spinner
        let spinner_style = if self.ascii {
            ProgressStyle::with_template("{elapsed_precise} {spinner} {msg}")
                .unwrap()
                .tick_chars(ASCII_SPINNER)
        } else {
            ProgressStyle::with_template("{elapsed_precise} {spinner} {msg}").unwrap()
        };
        let spinner = ProgressBar::new_spinner()
            .with_style(spinner_style)
            .with_message(format!(
                "SCANning {model_name} (target confidence {}, precision {})",
                self.confidence, self.precision
            ));
        let spinner = bars.add(spinner);

        // Progress bar
        let bound = okamoto_bound(self.confidence, self.precision).ceil() as u64;
        let progress_style = if self.ascii {
            ProgressStyle::with_template("{bar:50} {percent:>3}% ({pos}/{len}) ETA: {eta}")
                .unwrap()
                .progress_chars(ASCII_BAR)
        } else {
            ProgressStyle::with_template(
                "{bar:50.white.on_black} {percent:>3}% ({pos}/{len}) ETA: {eta}",
            )
            .unwrap()
            .progress_chars(FINE_BAR)
        };
        let progress_bar = ProgressBar::new(bound).with_style(progress_style);
        let progress_bar = bars.add(progress_bar);

        let line_style = ProgressStyle::with_template("Properties verification:").unwrap();
        let line = ProgressBar::new(0).with_style(line_style);
        let line = bars.add(line);

        // Property bars
        let mut bars_guarantees = Vec::new();
        let prop_style = if self.ascii {
            ProgressStyle::with_template("{bar:50} {percent:>3}% {prefix} {msg}")
                .unwrap()
                .progress_chars(ASCII_BAR)
        } else {
            ProgressStyle::with_template("{bar:50.green.on_red} {percent:>3}% {prefix} {msg}")
                .unwrap()
                .progress_chars(FINE_BAR)
        };

        // Guarantees property bars
        for name in guarantee_names.iter() {
            let bar = bars.add(
                ProgressBar::new(1)
                    .with_style(prop_style.clone())
                    .with_position(1)
                    .with_prefix(name.clone()),
            );
            bars_guarantees.push(bar);
        }

        // Overall property bar
        let overall_style = if self.ascii {
            ProgressStyle::with_template("{bar:50} {percent:>3}% {prefix} {msg}")
                .unwrap()
                .progress_chars(ASCII_BAR)
        } else {
            ProgressStyle::with_template("{bar:50.blue.on_red} {percent:>3}% {prefix} {msg}")
                .unwrap()
                .progress_chars(FINE_BAR)
        };
        let overall_bar = bars.add(
            ProgressBar::new(1)
                .with_style(overall_style)
                .with_position(1)
                .with_prefix("TOTAL"),
        );

        bars.set_move_cursor(true);
        while scan.running() {
            let successes = scan.successes();
            let failures = scan.failures();
            let runs = (successes + failures) as u64;
            let rate = successes as f64 / runs as f64;
            if runs > progress_bar.position() {
                // Status spinner
                spinner.tick();

                let bound = adaptive_bound(rate, self.confidence, self.precision);
                // let derived_precision =
                //     derive_precision(run_status.successes, run_status.failures, confidence);
                progress_bar.set_length(bound.ceil() as u64);
                progress_bar.set_position(runs);
                line.tick();
                let mut overall_fails = 0;
                let violations = scan.violations();
                for (i, bar) in bars_guarantees.iter().enumerate() {
                    let violations = violations.get(i).copied().unwrap_or(0);
                    overall_fails += violations;
                    let pos = runs - violations as u64;
                    bar.set_position(pos);
                    bar.set_length(runs);
                    if violations > 0 {
                        bar.set_message(format!("({violations} failed)"));
                    }
                    bar.tick();
                }

                // Overall property bar
                let pos = runs - overall_fails as u64;
                overall_bar.set_position(pos);
                overall_bar.set_length(runs);
                if overall_fails > 0 {
                    overall_bar.set_message(format!("({overall_fails} failed)"));
                }
                overall_bar.tick();
            }
            // Sleep a while to limit update/refresh rate.
            std::thread::sleep(std::time::Duration::from_millis(100));
        }
        bars.set_move_cursor(false);
        spinner.finish_and_clear();
        progress_bar.finish_and_clear();
        line.finish_and_clear();
        bars_guarantees.iter().for_each(|b| b.finish_and_clear());
        overall_bar.finish_and_clear();
        // Magnitude of precision, to round results to sensible number of digits
        let mag = (self.precision.log10().abs().ceil() as usize).max(2);
        println!(
            "SCAN results for {model_name} (confidence {}, precision {})",
            self.confidence, self.precision
        );
        let successes = scan.successes();
        let failures = scan.failures();
        let runs = (successes + failures) as u64;
        let rate = successes as f64 / runs as f64;
        println!(
            "Completed {runs} runs with {} successes, {} failures)",
            successes, failures
        );
        let violations = scan.violations();
        for (i, property) in guarantee_names.iter().enumerate() {
            let violations = violations.get(i).copied().unwrap_or(0);
            print!(
                "{property} success rate: {0:.1$}",
                ((runs - violations as u64) as f64) / (runs as f64),
                mag,
            );
            if violations > 0 {
                println!(" ({property} fails)");
            } else {
                println!();
            }
        }
        println!("Overall success rate: {rate:.0$}", mag);
    }
}
