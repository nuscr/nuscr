Generate OCaml code for Adders Client
  $ nuscr --gencode-ocaml=C@Adder Adder.nuscr > C.ml
Generate OCaml code for Adders Server
  $ nuscr --gencode-ocaml=S@Adder Adder.nuscr > S.ml
Compile
  $ dune build ./Adder.exe
  Info: Creating file dune-project with this contents:
  | (lang dune 2.7)
