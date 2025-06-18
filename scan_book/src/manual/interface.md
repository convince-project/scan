# The User Interface

SCAN provides a command line interface.

To print the help screen, use

```bash
$ scan --help
```

which will show the available functionalities and commands' syntax.

The general syntax to run SCAN is

```bash
$ scan [OPTIONS] <MODEL> <COMMAND>
```

where `MODEL` can be:

- For SCXML models, the path to either the main `xml` file, or to the directory containing all the model's files.
- For JANI models, the path to the JANI model's file.

SCAN tries to auto-detect the model format,
but the `--format [scxml|jani]` flag can be used to specify it explicitly.

Available commands are:

- `validate` to check the syntax and semantics of the model is correct, without executing any verification.
- `verify` to verify properties over the model.
- `trace` to save execution traces to file.

Each of these commands has its own options and help section,
which can be displayed with `scan help [COMMAND]`.`

## Global flags

SCAN accepts the following global flags:

- `--format` sets which model specification format is being used.
Possible values: `[scxml|jani]`.
- `--verbose` increases the log verbosity (can be invoked multiple times as `-vv` or `-vvv` etc.).
  Normally, this is unnecessary save for debugging purposes.
- `--quiet` disables logging completely.

An alternative way to set the logging level is to use:

```bash
$ RUST_LOG=<LOG_LEVEL> scan [OPTIONS] [MODEL]
```

where `LOG_LEVEL=[error|warn|info|debug|trace]`
(from the highest-level to the lowest-level log entries).

## Validate

This command has no options or flags:
it just parses the model specification and builds the internal model representation
to check that there are no issues with the specification.
Just run it with:

```bash
$ scan <MODEL> validate
```

## Verify

The command runs a verification task on the input model and uses statistical methods to estimate the probability that the specified properties are satisfied.

The syntax for `verify` is:

```bash
$ scan [GLOBAL_OPTIONS] <MODEL> verify [OPTIONS] [PROPERTIES]...
```

where `PROPERTIES` is a list of space-separated properties to verify (as named in the model specification).
A most useful flag is `--all` in alternative to the list of properties,
which triggers verification of all properties specified in the model.

__Example:__ The simplest way to verify all specified properties of an SCXML model contained in a directory
is to enter the directory containing the model and call the following command.

```bash
user@host:path/to/model$ scan . verify --all
```

The following flags control the statistical parameters of the verification:

- `--confidence` sets the statistical confidence that the produced result is accurate.
- `--precision` sets the target precision of the result.

Together, `--confidence` and `--precision` also determine how many executions are required to be performed,
via the new adaptive sampling method.
This determines the number of necessary samples also based on the outcomes of the previous samples,
so that this number cannot be known a-priori but has to be continually recalculated during the verification task.

The `--duration` flag's value sets the maximum duration (in model time) that the execution can take before being stopped.
As this may vary depending on the input model and SCAN has no means to determine it,
SCAN sets a reasonably large default value,
but for best results the developer should set a more appropriate value.

By default, SCAN only prints a short message informing the user that a verification task is underway,
then it hangs silently until the task is finished (if ever).
This is a fine behavior for use by other tools or in scripts,
but less than ideal for human users.

For long verification tasks, the user can enable a more informative interface updated in real time and featuring progress bars with the flag `--progress [plain|fancy]`,
where `plain` is just ascii (for universal compatibility) and `fancy` uses colors and Unicode characters.
The interface will show the progress towards task completion and make a best-effort attempt to estimate how long it is still missing.
Moreover, it will show the proportion of violations for each property and for the overall model.
At the end of the execution, the progress bars will be cleared out to print the final verification report.

At the end of a verification task, the outcome is printed as a plain message.
For toolchain integration, SCAN can output the report serialized in JSON format instead,
by passing the `--json` flag.

By default, SCAN runs taking full advantage of the host system's multi-threading capabilities.
Verification can be run on a single thread by passing the `--single-thread` flag.
Notice that there is no guarantee that SCAN (and its dependencies) will not still use multiple threads to run other tasks,
such as, for example, parsing. 

## Trace

This command executes the model and saves the execution traces to disk as gz-compressed csv files.

The syntax for `trace` is

```bash
$ scan [GLOBAL_OPTIONS] <MODEL> trace [OPTIONS] <TRACES>
```

where `TRACES` is the number of traces that are requested.

The available options are `--duration` and `--single-thread`,
with the same meaning as for the `verify` command.

The traces produced during verification are saved in a `./traces_NN/` directory,
with `NN` progressive indexing,
and further classified into `success/`, `failure/` and `undetermined/` sub-directories based on the outcome of the execution.
Traces are saved into `gz`-compressed `csv` format.
Since traces can take up a large amount of disk space,
care is recommended when running this command.
