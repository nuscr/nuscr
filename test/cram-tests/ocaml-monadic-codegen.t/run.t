Generate OCaml code for Adders Client
  $ nuscr --gencode-ocaml-monadic=C@Adder Adder.scr > C.ml
Generate OCaml code for Adders Server
  $ nuscr --gencode-ocaml-monadic=S@Adder Adder.scr > S.ml
Compile
  $ dune build ./AdderMonadic.exe
  Info: Creating file dune-project with this contents:
  | (lang dune 2.7)
  File "C.ml", line 31, characters 23-30:
  31 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
  File "C.ml", line 68, characters 23-30:
  68 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
  File "S.ml", line 93, characters 23-30:
  93 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
  File "C.ml", line 31, characters 23-30:
  31 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
  File "C.ml", line 68, characters 23-30:
  68 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
  File "S.ml", line 93, characters 23-30:
  93 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
