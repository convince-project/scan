use scan_core::Time;
use serde::Serialize;
use std::{collections::HashMap, fmt::Display};

#[derive(Serialize)]
pub(crate) struct Report {
    pub(crate) model: String,
    pub(crate) precision: f64,
    pub(crate) confidence: f64,
    pub(crate) duration: Time,
    pub(crate) rate: f64,
    pub(crate) runs: u32,
    pub(crate) successes: u32,
    pub(crate) failures: u32,
    pub(crate) property_failures: HashMap<String, u32>,
}

impl Report {
    pub(crate) fn print(&self, json: bool) {
        if json {
            let report = serde_json::ser::to_string_pretty(&self).expect("report serialization");
            println!("{report}");
        } else {
            // Print final report
            println!("{self}");
        };
    }
}

impl Display for Report {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Magnitude of precision, to round results to sensible number of digits
        let mag = (self.precision.log10().abs().ceil() as usize).max(2);
        writeln!(
            f,
            "SCAN results for {} (confidence {}, precision {})",
            self.model, self.confidence, self.precision
        )?;
        writeln!(
            f,
            "Completed {} runs with {} successes, {} failures)",
            self.runs, self.successes, self.failures
        )?;
        for (property, &violations) in self.property_failures.iter() {
            write!(
                f,
                "{property} success rate: {0:.1$}",
                ((self.runs - violations) as f64) / (self.runs as f64),
                mag,
            )?;
            if violations > 0 {
                writeln!(f, " ({violations} fails)")?;
            } else {
                writeln!(f)?;
            }
        }
        write!(f, "Overall success rate: {:.1$}", self.rate, mag)?;
        Ok(())
    }
}
