open! Core_kernel
open! Migrate_parsetree

(* open Efsm *)

open! Ast_407
open Parsetree

let gen_ast (_proto, _role) (_start, _g) : structure =
  let loc = Location.none in
  let vb = [%stri let hello = "hello"] in
  [vb]

let migrate = Versions.migrate Versions.ocaml_407 Versions.ocaml_current

let gen_code (proto, role) (start, g) =
  let buffer = Buffer.create 4196 in
  let formatter = Format.formatter_of_buffer buffer in
  let ast = gen_ast (proto, role) (start, g) in
  let ast = migrate.copy_structure ast in
  Pprintast.structure formatter ast ;
  Format.pp_print_flush formatter () ;
  Buffer.contents buffer
