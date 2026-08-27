# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.1] - Unreleased

### Changed

- SCXML and JANI frontends switch from `HashMap`s and `HashSet`s to `BTreeMap`s and `BTreeSet`s to improve input-determinism

### Fixed

- SCXML: Fix ECMAScript parser occasionally failing to recognize `Math` functions

## [0.3.0] - 2026-07-13

### New

- `scan_pmtl` and `scan_mtl` crates for oracles.

### Changed

- JANI: Complete makeover of JANI model building.
- **BREAKING**: Feature-gate language front-ends (SCXML and JANI active by default).
- **BREAKING**: Autodetect end of execution and remove `duration` parameter.
- **BREAKING** Core: New simplified `scan` API using only CS as models.
- **BREAKING** Core: Streamlined tracer API decoupling/deduplicating file handling.

## [0.2.0] - 2026-04-23

### Changed

- **BREAKING** SCXML: Use ECMAScript enumeration syntax (i.e., the property syntax) for enumeration values.

### Fixed

- SCXML: Resolve string values in traces.
- SCXML: Fix inconsistent string representation, possibly affecting verification results.

## [0.1.0] - 2026-04-13

First versioned release.
