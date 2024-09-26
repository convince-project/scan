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

There is no user documentation/manual yet.

## Formalism

Internally, SCAN uses Channel Systems (CS) as models,[^1]
and Metric Temporal Logic (MTL) as property specification language.

[^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

SCAN is being developed to accept models specified in multiple, rich modeling languages.
At the moment the following languages are planned or (partially) implemented:

- [x] State Charts specified in [SCXML format](https://www.w3.org/TR/scxml/).
- [ ] [Promela](https://spinroot.com/spin/Man/Manual.html)
- [ ] [JANI](https://jani-spec.org/)

## Build prerequisites

SCAN is entirely written in [Rust](https://www.rust-lang.org/),
so, to build it, you need to install a recent version of the Rust toolchain.
The easiest and recommended way to do so is by installing [rustup](https://rustup.rs/)
either following the instructions on its homepage or through your OS's package manager.
Do not forget to set your `PATH` correctly, if required.

## Installation and usage

Currently, the only way to use SCAN is to build it from sources.

To install and use SCAN on your system,
the easiest way is to use the `cargo install` command.
Follow the instructions from the [Build prerequisites](#build-prerequisites) section to install the required build dependencies.
Then install Scan directly from this repository with:

```
cargo install --git https://github.com/convince-project/scan
```

Cargo will build and install SCAN on your system.

After installation, SCAN can be used as a CLI tool.
To print the help screen, use

```
scan --help
```

which will show the available functionalities and commands' syntax.

The general syntax to run SCAN is

```
scan <MODEL> <COMMAND>
```

where `MODEL` is the path to your model file and `COMMAND=parse|build|execute`.

It can be helpful to run SCAN with logging activated.
Use

```
RUST_LOG=<LOG_LEVEL> scan <MODEL> <COMMAND>
```
where `LOG_LEVEL=error|warn|info|debug|trace`.

## Development

For development's purposes, you will want to build and run SCAN from source code.
Use Cargo's usual `build` and `run` commands for that.
For example, from the repo's root folder, build SCAN with

```
cargo build
```

Cargo, Rust's build manager, will take care of importing the required dependencies.

Run SCAN via Cargo with

```
cargo run -- [ARGS]
```

or by running the executable file directly, with

```
target/debug/scan [ARGS]
```

Install your local version of Scan on your system for more ease of use, with:

```
cargo install --path <SCAN_REPO_PATH>
```

To build SCAN's documentation API (with suggested flags) and have it open in a browser tab, run:

```
cargo doc --no-deps --workspace --open
```

Run all unit and integration tests with:

```
cargo test --workspace
```
