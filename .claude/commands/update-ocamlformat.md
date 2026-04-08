# Updating ocamlformat

The ocamlformat version is pinned in two places and both must be updated together:

1. `.ocamlformat` — change the `version = X.Y.Z` line
2. `.github/workflows/main.yml` — change `opam install ocamlformat=X.Y.Z`

After updating the version, install it and reformat the whole codebase:

```bash
opam install ocamlformat=X.Y.Z
dune build @fmt --auto-promote
```

Then verify the build and tests still pass:

```bash
dune build
dune runtest
```
