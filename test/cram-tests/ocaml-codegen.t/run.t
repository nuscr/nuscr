Generate OCaml code for Adders Client
  $ nuscr --gencode-ocaml=C@Adder Adder.scr > C.ml
Generate OCaml code for Adders Server
  $ nuscr --gencode-ocaml=S@Adder Adder.scr > S.ml
Compile
  $ dune build ./Adder.exe
  Info: Creating file dune-project with this contents:
  | (lang dune 2.7)
  File "C.ml", line 25, characters 23-30:
  25 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
  File "C.ml", line 49, characters 23-30:
  49 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
  File "S.ml", line 64, characters 23-30:
  64 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
  File "C.ml", line 25, characters 23-30:
  25 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
  File "C.ml", line 49, characters 23-30:
  49 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
  File "S.ml", line 64, characters 23-30:
  64 |           | (env, `bye payload) ->
                              ^^^^^^^
  Warning 27: unused variable payload.
