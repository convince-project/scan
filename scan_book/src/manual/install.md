# Installation

## Build prerequisites

SCAN is entirely written in [Rust](https://www.rust-lang.org/),
so, to build it, you need to install a recent version of the Rust toolchain.
The easiest and recommended way to do so is by installing [rustup](https://rustup.rs/)
either following the instructions on its homepage or through your OS's package manager.
Do not forget to set your `PATH` correctly, if required.

## Installing with Cargo

To install and use SCAN on your system,
the easiest way is to use the `cargo install` command, with:

```
cargo install --git https://github.com/convince-project/scan
```

Cargo will build and install SCAN on your system,
after which it can be used as a command-line tool.

Successively, the same command updates SCAN to the latest version.