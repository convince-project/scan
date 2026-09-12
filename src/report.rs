use serde::Serialize;
use std::fmt::Display;

#[derive(Debug, Clone, Serialize)]
pub(crate) struct Report {
    pub(crate) model: String,
    pub(crate) property: String,
    pub(crate) precision: f64,
    pub(crate) confidence: f64,
    pub(crate) rate: f64,
    pub(crate) runs: u32,
    pub(crate) successes: u32,
    pub(crate) failures: u32,
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
        let property = &self.property;
        writeln!(
            f,
            "Completed {} runs with {} successes and {} failures",
            self.runs, self.successes, self.failures
        )?;
        write!(
            f,
            "Property {property} success rate: {0:.1$}",
            ((self.runs - self.failures) as f64) / (self.runs as f64),
            mag,
        )?;
        write!(f, "Overall success rate: {:.1$}", self.rate, mag)?;
        Ok(())
    }
}
