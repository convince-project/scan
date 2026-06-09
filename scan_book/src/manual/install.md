# Installation

Currently, the only way to obtain SCAN is to build it from sources.

## Build prerequisites

SCAN is entirely written in [Rust](https://www.rust-lang.org/),
so, to build it, you need to install a recent version of the Rust toolchain.
The easiest and recommended way to do so is by installing [rustup](https://rustup.rs/)
either following the instructions on its homepage or through your OS's package manager.
Do not forget to set your `PATH` correctly, if required.

## Installing with Cargo

To install and use SCAN on your system,
the easiest way is to use the `cargo install` command, with:

```console
cargo install smc_scan --locked
```

Cargo will build and install SCAN on your system
(the `--locked` option is not required,
but it is recommended as it improves build reproducibility
by enforcing the use of specified versions for dependencies).

Cargo will build and install SCAN on your system,
after which it can be used as a command-line tool.
The same command updates SCAN to the latest version.

After installation, type

```console
scan
```

to verify that the installation completed successfully
by displaying the in-line help.

## Installing specific versions

To install a specific SCAN version, e.g., v0.1.0, use:

```console
cargo install smc_scan --version 0.1.0 --locked
```

It is also possible to install SCAN from the latest commit directly from this repository with:

```console
cargo install --git https://github.com/convince-project/scan
```

(see `cargo install --help` for more options).

## Features

By default, SCAN includes the SCXML and JANI frontends,
while the Promela frontend is disabled through the use of [Cargo features](https://doc.rust-lang.org/cargo/reference/features.html).
The available features for SCAN are:

- `scxml` to include the SCXML frontend (enabled by default);
- `jani` to include the JANI frontend (enabled by default);
- `promela` to include the Promela frontend (disabled by default);

To change which frontends to include at build time,
explicitly select the desired features with the `--features` option as follows:

```console
cargo install smc_scan --locked --features FEATURES
```

where `FEATURES` is a (comma-separated, or space-separated inside quotes) list.
Alternatively, if you want all available features, install SCAN with:

```console
cargo install smc_scan --locked --all-features
```

Be aware that each feature imports extra dependencies and increases build time.

## Build optimization

Even though by default the `cargo install` builds are highly-optimized,
there exist some further advanced build optimizations set via local configurations.
It can be difficult (and inappropriate) to enable these compiler optimizations on the SCAN side,
so we prefer to leave it to (advanced) individual users to decide what makes sense for their use cases.
Empirically, such optimization have been shown to yield up to, but no more than, a 10% performance improvement on typical SCAN use cases.

An excellent, though unofficial, reference on Rust performance optimization is [The Rust Performance Book](https://nnethercote.github.io/perf-book/),
specifically the [Build configuration](https://nnethercote.github.io/perf-book/build-configuration.html) section.
