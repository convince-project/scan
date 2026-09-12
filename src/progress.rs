use clap::ValueEnum;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use scan_core::{Oracle, Scan, adaptive_bound, okamoto_bound};

/// Verification progress bar
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
#[deny(missing_docs)]
pub(crate) enum Bar {
    /// Colorful Unicode progress bars
    Fancy,
    /// Plain ASCII progress bars
    Plain,
}

impl Bar {
    pub(crate) fn print_progress_bar<O: Oracle>(
        &self,
        confidence: f64,
        precision: f64,
        guarantees: &String,
        scan: &Scan<O>,
    ) {
        const FINE_BAR: &str = "█▉▊▋▌▍▎▏  ";
        const ASCII_BAR: &str = "#--";
        let progress_chars = if let Bar::Plain = self {
            ASCII_BAR
        } else {
            FINE_BAR
        };
        let property_template = if let Bar::Plain = self {
            "[{wide_bar}] {percent:>3}%"
        } else {
            "[{wide_bar:.green.on_red}] {percent:>3}%"
        };

        let bars = MultiProgress::new();

        // Property bars
        let property_header_style = ProgressStyle::with_template("{prefix}: {msg}").unwrap();
        let property_style = ProgressStyle::with_template(property_template)
            .unwrap()
            .progress_chars(progress_chars);

        // Guarantees property bars
        let header = bars.add(
            ProgressBar::new(0)
                .with_style(property_header_style.clone())
                .with_prefix(guarantees.to_owned())
                .with_message("0/0"),
        );
        header.tick();
        let property = bars.add(ProgressBar::new(0).with_style(property_style.clone()));
        property.tick();

        // Spinner
        // Trailing spaces because bar does not overwrite after itself
        let spinner_style =
            ProgressStyle::with_template("{elapsed_precise} {msg}: {pos}/{len} ({eta})   ")
                .unwrap();
        let spinner = ProgressBar::new(0)
            .with_style(spinner_style)
            .with_message("verification progress");
        let spinner = bars.add(spinner);
        spinner.tick();

        // Progress bar
        let bound = okamoto_bound(confidence, precision).ceil() as u64;
        let progress_style = ProgressStyle::with_template("[{wide_bar}] {percent:>3}%")
            .unwrap()
            .progress_chars(progress_chars);
        let progress_bar = ProgressBar::new(bound).with_style(progress_style);
        let progress_bar = bars.add(progress_bar);
        progress_bar.tick();

        bars.set_move_cursor(true);
        while scan.running() || (scan.successes() == 0 && scan.failures() == 0) {
            let successes = scan.successes();
            let failures = scan.failures();
            let runs = (successes + failures) as u64;
            let rate = successes as f64 / runs as f64;
            if runs > progress_bar.position() {
                let bound = adaptive_bound(rate, confidence, precision);
                // let derived_precision =
                //     derive_precision(run_status.successes, run_status.failures, confidence);
                // Status spinner
                let pos = runs.saturating_sub(failures as u64);
                header.set_message(format!("{pos}/{failures}"));
                header.tick();
                property.set_position(pos);
                property.set_length(runs);
                property.tick();

                // task progress bar
                spinner.set_position(runs);
                spinner.set_length(bound.ceil() as u64);
                spinner.tick();
                progress_bar.set_position(runs);
                progress_bar.set_length(bound.ceil() as u64);
                progress_bar.tick();
            } else {
                // update time/eta
                spinner.tick();
            }
            // Sleep a while to limit update/refresh rate.
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        // Clean up terminal
        bars.set_move_cursor(false);
        header.finish_and_clear();
        property.finish_and_clear();
        spinner.finish_and_clear();
        progress_bar.finish_and_clear();
    }
}
