use anyhow::Context;
use anyhow::anyhow;
use anyhow::bail;
use clap::{Parser, ValueEnum};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use scan_core::{Oracle, Scan, adaptive_bound, okamoto_bound};
use serde::Serialize;
use std::{collections::HashMap, path::PathBuf, sync::Arc};

/// Supported model specification formats
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Format {
    /// SCXML format
    Scxml,
    /// JANI format
    Jani,
}

/// Type of verification report
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Output {
    /// Human-readable report
    Human,
    /// JSON-serialized report
    Json,
}

/// Verification progress bar
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Bar {
    /// Fancy Unicode progress bars
    Unicode,
    /// Basic ASCII progress bars
    Ascii,
}

#[derive(Serialize)]
struct Report {
    precision: f64,
    confidence: f64,
    duration: u32,
    rate: f64,
    runs: u32,
    successes: u32,
    failures: u32,
    property_failures: HashMap<String, u32>,
}

/// A statistical model checker for large concurrent systems
#[derive(Clone, Parser)]
#[deny(missing_docs)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Path of model's main XML file
    #[arg(value_hint = clap::ValueHint::AnyPath)]
    path: PathBuf,
    /// Format used to specify the model
    #[arg(value_enum, short, long)]
    format: Option<Format>,
    /// Confidence
    #[arg(short, long, default_value = "0.95")]
    confidence: f64,
    /// Precision or half-width parameter
    #[arg(short, long, default_value = "0.01")]
    precision: f64,
    /// Max duration of execution (in model-time)
    #[arg(short, long, default_value = "10000")]
    duration: u32,
    /// Saves execution traces in gz-compressed csv format
    #[arg(long = "traces", default_value = "false")]
    traces: bool,
    /// Output format of verification report
    #[arg(short, long, default_value = "human")]
    out: Output,
    /// Progress bar during verification
    #[arg(value_enum, short, long)]
    bar: Option<Bar>,
}

impl Cli {
    pub fn run(self) -> anyhow::Result<()> {
        if let Some(format) = self.format {
            match format {
                Format::Scxml => self.run_scxml(),
                Format::Jani => self.run_jani(),
            }
        } else if self.path.is_dir() {
            self.run_scxml()
        } else {
            let ext = self
                .path
                .extension()
                .ok_or(anyhow!("file extension unknown"))?;
            match ext
                .to_str()
                .ok_or(anyhow!("file extension not recognized"))?
            {
                "xml" => self.run_scxml(),
                "jani" => self.run_jani(),
                _ => bail!("unsupported file format"),
            }
        }
    }

    fn run_scxml(self) -> anyhow::Result<()> {
        use scan_scxml::*;

        let (scan, scxml_model) = load(&self.path)?;
        let scxml_model = Arc::new(scxml_model);
        let guarantees = scxml_model.guarantees.clone();
        let tracer = self.traces.then(|| TracePrinter::new(scxml_model));
        self.run_scan(scan, guarantees, tracer)
    }

    fn run_jani(self) -> anyhow::Result<()> {
        use scan_jani::*;

        let (scan, jani_model) = load(&self.path)?;
        let jani_model = Arc::new(jani_model);
        let guarantees = jani_model.guarantees.clone();
        let tracer = self.traces.then(|| TracePrinter::new(jani_model));
        self.run_scan(scan, guarantees, tracer)
    }

    fn run_scan<E, Err, Ts, Tr, O>(
        self,
        scan: Scan<E, Err, Ts, O>,
        guarantees: Vec<String>,
        tracer: Option<Tr>,
    ) -> anyhow::Result<()>
    where
        Ts: scan_core::TransitionSystem<E, Err> + 'static,
        Tr: scan_core::Tracer<E> + 'static,
        Err: std::error::Error + Clone + Send + Sync + 'static,
        E: Clone + Send + Sync + 'static,
        O: Oracle + 'static,
    {
        let mut handle = None;
        let model_name = self
            .path
            .file_stem()
            .and_then(|s| s.to_str())
            .map_or("model".to_string(), |s| format!("'{s}'"));
        if let Some(bar) = self.bar {
            let model_name = model_name.clone();
            let scan = scan.clone();
            let guarantees = guarantees.clone();
            handle = Some(std::thread::spawn(move || {
                print_progress_bar(
                    bar,
                    self.confidence,
                    self.precision,
                    &guarantees,
                    &scan,
                    model_name,
                );
            }));
        }
        scan.adaptive(self.confidence, self.precision, self.duration, tracer);
        if let Some(handle) = handle {
            handle.join().expect("terminate process");
        }
        match self.out {
            Output::Human => {
                // Print final report
                self.print_report(&scan, &guarantees, model_name);
            }
            Output::Json => {
                let report = self.json_report(&scan, guarantees)?;
                println!("{report}");
            }
        }
        Ok(())
    }

    fn json_report<E, Err, Ts, O>(
        &self,
        scan: &Scan<E, Err, Ts, O>,
        guarantees: Vec<String>,
    ) -> anyhow::Result<String>
    where
        Ts: scan_core::TransitionSystem<E, Err> + 'static,
        Err: std::error::Error + Send + Sync,
        E: Send + Sync,
        O: Oracle + 'static,
    {
        let successes = scan.successes();
        let failures = scan.failures();
        let runs = successes + failures;
        let rate = successes as f64 / runs as f64;
        let property_failures = guarantees
            .into_iter()
            .zip(scan.violations().into_iter().chain([0].into_iter().cycle()))
            .collect::<HashMap<String, u32>>();
        let report = Report {
            precision: self.precision,
            confidence: self.confidence,
            duration: self.duration,
            rate,
            runs,
            successes,
            failures,
            property_failures,
        };
        serde_json::ser::to_string_pretty(&report).context(anyhow!("failed report serialization"))
    }

    fn print_report<E, Err, Ts, O>(
        &self,
        scan: &Scan<E, Err, Ts, O>,
        guarantees: &[String],
        model_name: String,
    ) where
        Ts: scan_core::TransitionSystem<E, Err> + 'static,
        Err: std::error::Error + Send + Sync,
        E: Send + Sync,
        O: Oracle + 'static,
    {
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
        for (i, property) in guarantees.iter().enumerate() {
            let violations = violations.get(i).copied().unwrap_or(0);
            print!(
                "{property} success rate: {0:.1$}",
                ((runs - violations as u64) as f64) / (runs as f64),
                mag,
            );
            if violations > 0 {
                println!(" ({violations} fails)");
            } else {
                println!();
            }
        }
        println!("Overall success rate: {rate:.0$}", mag);
    }
}

fn print_progress_bar<E, Err, Ts, O>(
    bar: Bar,
    confidence: f64,
    precision: f64,
    guarantees: &[String],
    scan: &Scan<E, Err, Ts, O>,
    model_name: String,
) where
    Ts: scan_core::TransitionSystem<E, Err> + 'static,
    Err: std::error::Error + Send + Sync,
    E: Send + Sync,
    O: Oracle + 'static,
{
    const FINE_BAR: &str = "█▉▊▋▌▍▎▏  ";
    const ASCII_BAR: &str = "#--";
    const ASCII_SPINNER: &str = "|/-\\";

    let bars = MultiProgress::new();

    // Spinner
    let spinner_style = if let Bar::Ascii = bar {
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
            confidence, precision
        ));
    let spinner = bars.add(spinner);

    // Progress bar
    let bound = okamoto_bound(confidence, precision).ceil() as u64;
    let progress_style = if let Bar::Ascii = bar {
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

    let line_style = ProgressStyle::with_template("Property rates:").unwrap();
    let line = ProgressBar::new(0).with_style(line_style);
    let line = if !guarantees.is_empty() {
        bars.add(line)
    } else {
        line
    };

    // Property bars
    let prop_style = if let Bar::Ascii = bar {
        ProgressStyle::with_template("{bar:50} {percent:>3}% {prefix} {msg}")
            .unwrap()
            .progress_chars(ASCII_BAR)
    } else {
        ProgressStyle::with_template("{bar:50.green.on_red} {percent:>3}% {prefix} {msg}")
            .unwrap()
            .progress_chars(FINE_BAR)
    };

    // Guarantees property bars
    let mut bars_guarantees = Vec::new();
    for name in guarantees.iter() {
        let bar = bars.add(
            ProgressBar::new(1)
                .with_style(prop_style.clone())
                .with_position(1)
                .with_prefix(name.clone()),
        );
        bars_guarantees.push(bar);
    }

    let overall_line_style = ProgressStyle::with_template("Overall system rate:").unwrap();
    let overall_line = ProgressBar::new(0).with_style(overall_line_style);
    let overall_line = bars.add(overall_line);

    // Overall property bar
    let overall_bar = bars.add(
        ProgressBar::new(1)
            .with_style(prop_style)
            .with_position(1)
            .with_prefix("TOTAL"),
    );

    bars.set_move_cursor(true);
    while scan.running() || (scan.successes() == 0 && scan.failures() == 0) {
        let successes = scan.successes();
        let failures = scan.failures();
        let runs = (successes + failures) as u64;
        let rate = successes as f64 / runs as f64;
        if runs > progress_bar.position() {
            // Status spinner
            spinner.tick();

            let bound = adaptive_bound(rate, confidence, precision);
            // let derived_precision =
            //     derive_precision(run_status.successes, run_status.failures, confidence);
            progress_bar.set_length(bound.ceil() as u64);
            progress_bar.set_position(runs);
            if !guarantees.is_empty() {
                line.tick();
            }
            let mut overall_fails = 0;
            let violations = scan.violations();
            for (i, bar) in bars_guarantees.iter().enumerate() {
                let violations = violations.get(i).copied().unwrap_or(0);
                overall_fails += violations;
                let pos = runs.checked_sub(violations as u64).unwrap_or_default();
                bar.set_position(pos);
                bar.set_length(runs);
                if violations > 0 {
                    bar.set_message(format!("({violations} failed)"));
                }
                bar.tick();
            }

            // Overall property bar
            overall_line.tick();
            let pos = runs.checked_sub(overall_fails as u64).unwrap_or_default();
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

    // Clean up terminal
    bars.set_move_cursor(false);
    spinner.finish_and_clear();
    progress_bar.finish_and_clear();
    line.finish_and_clear();
    bars_guarantees.iter().for_each(|b| b.finish_and_clear());
    overall_line.finish_and_clear();
    overall_bar.finish_and_clear();
}
