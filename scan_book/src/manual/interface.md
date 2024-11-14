# The User Interface

SCAN provides a command line interface.
To print the help screen, use

```
scan --help
```

which will show the available functionalities and commands' syntax:

```
A statistical model checker for large concurrent systems
                                                                                                        
Usage: scan [OPTIONS] <MODEL>
                                                                                                        
Arguments:
  <MODEL>  Path of model's main XML file
                                                                                                        
Options:
  -c, --confidence <CONFIDENCE>  Confidence [default: 0.95]
  -p, --precision <PRECISION>    Precision or half-width parameter [default: 0.01]
      --trace                    Print execution trace
      --length <LENGTH>          Max length of execution trace before it is stopped [default: 10000]
  -h, --help                     Print help
  -V, --version                  Print version
```

The general syntax to run SCAN is

```
scan [OPTIONS] <MODEL>
```

where `MODEL` is the path to the folder containing the model's `scxml` files.
SCAN will try to correctly interpret all files inside the folder.

If the model has a main `xml` file, passing its path instead of the folder's allows for finer control over which processes the model is using and where their definition can be found.

The `confidence` and `precision` parameters control the statistical confidence that the produced result is accurate up to the given precision,
but also determine how many executions are required to be performed.

The `length` parameter set the maximum length a trace can reach before the execution is considered complete.

## Logging

It can be helpful to run SCAN with logging activated.
Use

```
RUST_LOG=<LOG_LEVEL> scan <MODEL> <COMMAND>
```

where `LOG_LEVEL=[error|warn|info|debug|trace]`
(from the highest-level to the lowest-level log entries).
