# Introduction

SCAN (StatistiCal ANalyzer) is a statistical model checker
designed to verify large concurrent systems
for which standard verification techniques do not scale.

SCAN has a modular architecture that supports multiple front-ends,
allowing multiple modeling and property specification languages.
Currently, and with different levels of maturity, SCAN supports:
- SCXML as modeling language, with past-MTL as property specification language and state/event time traces.
- JANI as modeling language, with (limited) LTL as property specification language.
- Promela as modelling language (experimental).
- ... (support for other languages is underway)

SCAN is currently under development at DIBRIS, University of Genoa
in the context of the [CONVINCE project](https://convince-project.eu/).

While SCAN can also be as a standalone model checker,
it closely integrates with the other tools in the CONVINCE toolchain.
In particular, AS2FM can target the SCXML format accepted by SCAN,
to make verification of robotic systems more accessible to robotic systems developers.

<a href="crates/scan/index.html">API docs for the library crates.</a>
