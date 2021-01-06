Generate OCaml code for Adders Client
  $ nuscr --gencode-ocaml-monadic=C@Adder Adder.nuscr > C.ml
Generate OCaml code for Adders Server
  $ nuscr --gencode-ocaml-monadic=S@Adder Adder.nuscr > S.ml
Compile
  $ dune build ./AdderMonadic.exe
  Info: Creating file dune-project with this contents:
  | (lang dune 2.7)
