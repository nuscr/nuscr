𝝼Scr -- New Scribble [![CircleCI](https://circleci.com/gh/nuscr/nuscr.svg?style=svg)](https://circleci.com/gh/nuscr/nuscr)
=====================

A toolkit to manipulate Scribble-style multiparty protocols, based on
classical multiparty session type theory. The toolkit provides means
to define global protocols, project to local protocols, convert local
protocols to a CFSM representation, and generate OCaml code for
protocol implementations.

You can quickly try the live version [here](https://nuscr.github.io/nuscr/).

## How to install

The easiest way to install is to use [opam](https://opam.ocaml.org) via

```
  opam install nuscr
```

then you can check your installation via

```
  nuscr --help
```

which prints help and usage information.

## Development

We use the [dune](https://dune.readthedocs.io/en/stable) build system.
To contribute, clone the repo and run
```
  dune build
```
to build.

You can run the executable by
```
  dune exec nuscr -- [args]
```
where `[args]` are the arguments to the main CLI program.

You can run the tests via
```
  dune runtest
```

When you submit a pull request, the continuous integration system will build,
run tests and check the code formatting. You may find `utils/fmt.sh` helpful
for formatting the code.

Please also remember to update the `CHANGES.md` file in order to keep track of
the changes in each version.

## Goals and Non-goals

Our vision of nuScr is to provide a simple and lightweight implementation based
on multiparty session types (MPST).
We aim to make it easy for researchers to extend the original MPST, and to
prototype implementations.
A future goal is to provide a certified implementation (extracted from Coq).

With that said, nuScr is not ready for production (better use the old
Scribble), and will not be compatible with the old Scribble.
The old Scribble had been extended beyond the original MPST to model checking
with automata, whereas we wish to stick to the top-down approach of original
MPST.
NuScr is likely to be not so user-friendly in the current form, and may remain
so in the future.
