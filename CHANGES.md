# 2.0.0-rc

## Added

- A new pragma system for extensions:
  `(*# PRAGMA #*)` at the beginning of the input file can enable theory
  extensions.
- Nested Protocol extension, via `NestedProtocols` pragma (by Benito Echarren Serrano)
- Refinement Type extension, via `RefinementTypes` pragma (by Fangyi Zhou)

## Changed

- New, improved command line interface with cmdliner
- Reorganise code layout
- Recursions immediately after a choice is permitted under some circumstances

## Fixed

- Non-distinct choice prefixes now raise an error
- Degenerate recursions in protocols will be removed
- Catch an uncaught exception when user enters a non-existent protocol from CLI
- Merging [end] and [\mu t.t] after projection is now possible

# 1.1.0 (2020-01-10)

## Added

- Export code generation APIs in Lib

## Changed

- Modernised Web Interface
- Change signature of `generate_fsm` in Lib
- Annotate signature of `project_role`, `generate_fsm` with names
- Update ppxlib dependency version
- Remove js_of_ocaml dependencies

## Deprecated

## Fixed

- Fix usage in executable
- Fix non-monadic code generation

## Removed

## Security

# 1.0.0 (2019-12-04)

Initial Release
