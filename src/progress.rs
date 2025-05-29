use clap::ValueEnum;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use scan_core::{Oracle, Scan, adaptive_bound, okamoto_bound};

/// Verification progress bar
#[derive(Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub(crate) enum Bar {
    /// Fancy Unicode progress bars
    #[default]
    Unicode,
    /// Basic ASCII progress bars
    Ascii,
}

impl Bar {
    pub(crate) fn print_progress_bar<E, Ts, O>(
        &self,
        confidence: f64,
        precision: f64,
        guarantees: Vec<String>,
        scan: Scan<E, Ts, O>,
        model_name: String,
    ) where
        Ts: scan_core::TransitionSystem<E>,
        E: Send + Sync,
        O: Oracle,
    {
        const FINE_BAR: &str = "█▉▊▋▌▍▎▏  ";
        const ASCII_BAR: &str = "#--";
        const ASCII_SPINNER: &str = "|/-\\";

        let bars = MultiProgress::new();

        // Spinner
        let spinner_style = if let Bar::Ascii = self {
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
        let progress_style = if let Bar::Ascii = self {
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
        let prop_style = if let Bar::Ascii = self {
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
}
