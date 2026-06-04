// An efficient statistical model checker for nondeterminism and rare events,
// Carlos E. Budde, Pedro R. D’Argenio, Arnd Hartmanns, Sean Sedwards.
// International Journal on Software Tools for Technology Transfer (2020) 22:759–780
// https://doi.org/10.1007/s10009-020-00563-2

/// Computes Okamoto bound for given confidence and precision.
pub fn okamoto_bound(confidence: f64, precision: f64) -> f64 {
    (2f64 / (1f64 - confidence)).ln() * 0.5f64 / precision.powi(2)
}

/// Computes adaptive bound for given confidence, precision and (partial) experimental results.
pub fn adaptive_bound(avg: f64, confidence: f64, precision: f64) -> f64 {
    okamoto_bound(confidence, precision)
        * precision
            .mul_add(-2f64 / 3f64, (avg - 0.5f64).abs())
            .powi(2)
            .mul_add(-4f64, 1f64)
}

/// Computes precision for given experimental results and confidence
/// deriving it from adaptive bound through quadratic equation.
pub fn derive_precision(s: u32, f: u32, confidence: f64) -> f64 {
    let n = s + f;
    let avg = s as f64 / n as f64;
    let k = 2f64 * (2f64 / (1f64 - confidence)).ln();
    // Compute quadratic equation coefficients.
    let a = k.mul_add(4f64 / 9f64, n as f64);
    let b = -(4f64 / 3f64) * k * (avg - 0.5f64).abs();
    let c = k * (avg.powi(2) - avg);
    // Take (larger positive) quadratic equation solution.
    (a.mul_add(-4f64 * c, b.powi(2)).sqrt() - b) / (2f64 * a)
}
