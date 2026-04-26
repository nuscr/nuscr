# RuScr

RuScr is a Rust prototype port of the basic nuScr core. It intentionally
lives in this subdirectory so it can be developed on a branch without
touching the OCaml implementation.

## Implemented in this prototype

- Parsing for basic global protocol declarations.
- Core interactions: messages, recursion, continue, choice, and `do`.
- Recursive `do` expansion into generated recursion variables.
- Global type construction and validation for core role/recursion errors.
- Projection from global types to local types.
- Local type to EFSM conversion.
- Basic CLI actions:
  - `--enum`
  - `--show-global-type`
  - `--show-global-type-mpstk`
  - `--show-global-type-tex`
  - `--generate-sexp`
  - `--project`
  - `--project-mpstk`
  - `--project-tex`
  - `--fsm`

## Out of scope for the first milestone

Pragmas, refinement types, nested protocols, protobuf output, and OCaml/Go/F*
code generation are imported as fixtures but treated as expected-fail areas.

## Development

```sh
cargo test
cargo run -- --enum tests/fixtures/cram-tests/core/mpstk.t/TwoBuyer.nuscr
cargo run -- --project B1@TwoBuyer tests/fixtures/cram-tests/core/mpstk.t/TwoBuyer.nuscr
```
