Generate OCaml code for Adders Client
  $ nuscr --gencode-ocaml=C@Adder Adder.scr > C.ml
Generate OCaml code for Adders Server
  $ nuscr --gencode-ocaml=S@Adder Adder.scr > S.ml
Compile
  $ dune build ./Adder.exe
  Info: Creating file dune-project with this contents:
  | (lang dune 2.7)
