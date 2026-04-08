# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Is

nuScr is an OCaml toolkit for manipulating and validating Scribble-style multiparty session type (MPST) protocols. It parses `.nuscr` protocol files, validates them, projects global types to local types per role, generates finite state machines, and produces code in OCaml, Go, and F*.

## Build and Development Commands

```bash
dune build                          # Build everything
dune exec nuscr -- [args]           # Run the CLI
dune runtest                        # Run full test suite
dune runtest test/cram-tests/core/  # Run a specific test directory
dune build @fmt --auto-promote      # Format code (or use utils/fmt.sh)
dune build @doc                     # Build documentation
```

To run a single cram test file:
```bash
dune runtest test/cram-tests/core/mpstk.t/
```

Code formatter is `ocamlformat` v0.29.0. Config is in `.ocamlformat`. For upgrade steps, see [`.claude/commands/update-ocamlformat.md`](.claude/commands/update-ocamlformat.md).

## Architecture

### Data Flow

```
.nuscr file → parse → validate → get_global_type → project_role → generate_fsm → codegen
```

The public API is in `lib/nuscrlib.mli`. All significant transformations go through this interface.

### Key Modules

**`lib/syntaxtree/`** — Parsing layer
- `parser.mly` / `lexer.ml`: Menhir grammar + sedlex lexer
- `syntax.ml`: AST types for the concrete syntax
- `symtable.ml`: Symbol table for name resolution

**`lib/mpst/`** — Core type theory
- `gtype.ml`: Global types (protocol-level, omniscient view)
- `ltype.ml`: Local types (per-role projection)
- `efsm.ml`: Extended finite state machines
- `expr.ml`: Expressions used in guards and payloads
- `message.ml`: Message label and payload abstractions

**`lib/codegen/`** — Code generation backends
- `ocaml/`, `go/`, `fstar/`: Language-specific generators
- `codegen.ml`: Shared utilities

**`lib/utils/`** — Shared utilities
- `names.ml`: Typed name wrappers (RoleName, ProtocolName, etc.)
- `err.ml`: Error types
- `loc.ml`: Source location tracking
- `pragma.ml`: Protocol annotations/pragmas
- `solver.ml`: Z3 integration for refinement type checking

**`bin/main.ml`** — CLI entry point via `cmdliner`

**`web/`** — Browser interface compiled with `js_of_ocaml`

### Test Structure

Tests are Cram-style (`.t` files) in `test/cram-tests/core/`. They invoke the `nuscr` CLI and check output. Example `.nuscr` protocol files are in `examples/`.

### Key Abstractions

- **Global type**: Full protocol from an omniscient view; the primary artifact parsed from `.nuscr` files
- **Local type**: Projection of a global type to a single role's perspective
- **EFSM**: Finite state machine derived from a local type, used for code generation
- **Pragma**: Annotations on protocols that enable extensions (e.g., refinement types, nested protocols)
- **Names**: All names (roles, protocols, variables) are wrapped in distinct OCaml types in `names.ml` to prevent mixing
