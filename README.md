# SCAN

SCAN (StatistiCal ANalyzer) is a statistical model checker
designed to verify large concurrent systems
for which standard verification techniques do not scale.

## Developement status

SCAN is currently under developement at DIBRIS, University of Genoa (UniGe)
in the context of the [CONVINCE project](https://convince-project.eu/).
There is no released version yet.

## Documentation

API docs for the library crates are hosted at [https://convince-project.github.io/scan/scan](https://convince-project.github.io/scan/scan).

There is currently no user documentation/manual yet.

## Formalism

SCAN uses Channel Systems (CS) as models,[^1]
and Metric Temporal Logic (MTL) as property specification language.

[^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

## Modeling specification languages

SCAN is being developed to accept models specified in multiple, rich modeling languages.
At the moment the following languages are planned or implemented:

- [x] State Charts specified in [SCXML format](https://www.w3.org/TR/scxml/).
- [ ] [Promela](https://spinroot.com/spin/Man/Manual.html)
- [ ] [JANI](https://jani-spec.org/)

## Build instructions

Currently, the only way to use SCAN is to build it from sources.

### Install a Rust toolchain.

SCAN is entirely written in [Rust](https://www.rust-lang.org/),
so, to build it, you will need to install a recent version of the Rust toolchain.
The easiest and recommended way to do so is by installing [rustup](https://rustup.rs/)
either following the instructions on its homepage or through your OS's package manager.

### Building and running

From the repo's root folder, build SCAN with

```
cargo build --release
```

(without the `--release` flag, the compiler will build in debug mode).
Cargo, Rust's build manager, will take care of importing the required dependencies.

Run SCAN via Cargo with

```
cargo run --release -- --help
```

or by running the executable file directly, with

```
target/release/scan --help
```

and the CLI help will show the available functionalities and commands' syntax.

To build SCAN's documentation API (with suggested flags) and have it open in a browser tab, run:

```
cargo doc --no-deps --workspace --open
```

Run all unit and integration tests with:

```
cargo test --workspace
```
