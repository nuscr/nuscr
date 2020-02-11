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
  nuscr -version
```

which prints the version installed.

## Development

We use the [dune](https://dune.readthedocs.io/en/stable) build system.
To contribute, clone the repo and run 
```
  dune build
```
to build
You can run the the executable by
```
  dune exec nuscr -- [args]
```
where `[args]` are the arguments to the main CLI program.

You can run tests via
```
  dune runtest
```

When you submit a pull request, the continuous integration system will build, run tests and check the code formatting.
Please also remember to update the `CHANGES.md` file for keeping a changelog.
