# The User Interface

SCAN provides a command line interface.

To print the help screen, use

```
scan --help
```

which will show the available functionalities and commands' syntax.

The general syntax to run SCAN is

```
scan [OPTIONS] [MODEL]
```

where `MODEL` is the path to the directory containing the model's `scxml` files.
As the default directory is the current one,
just invoking `scan` inside a model's directory starts the verification with default settings.
If the model has a main `xml` file, passing its path instead of the folder's allows for finer control over which processes the model is using and where their definition can be found.

## Settings and Options

SCAN accepts the following parameters:

- `--confidence` sets the statistical confidence that the produced result is accurate
- `--precision` sets the target precision of the result

Toghether, these settings also determine how many executions are required to be performed.

The following parameters are to be set by the developer according to the use case:

- `--length` sets the maximum length a trace can reach before the execution is stopped
- `--duration` sets the maximum duration (in model time) that the execution can take before being stopped

As these settings may vary depending on the use case,
SCAN sets reasonably large default values,
but they can be changed if necessity arises.

The following option are available:

- `--save-traces` has all the traces produced during verification saved in a `./traces/` folder,
and further classified into `success/`, `failure/` and `undetermined/` sub-folders based on the outcome of the execution.
Traces are saved into `gz`-compressed `csv` format.
Since traces can take up a large amount of disk space,
the option is disabled by default and care is reccommended when enabling it.

## Logging

It can be helpful to run SCAN with logging activated.
Use

```
RUST_LOG=<LOG_LEVEL> scan [OPTIONS] [MODEL]
```

where `LOG_LEVEL=[error|warn|info|debug|trace]`
(from the highest-level to the lowest-level log entries).
