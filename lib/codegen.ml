open! Core_kernel

(* open Efsm *)

open Ocaml_common

let gen_ast (_proto, _role) (_start, _g) : Parsetree.structure =
  let open Ast_helper in
  let open Location in
  let vb =
    Vb.mk (Pat.var (mknoloc "hello")) (Exp.constant (Const.string "Hello"))
  in
  [Str.value Asttypes.Nonrecursive [vb]]

let gen_code (proto, role) (start, g) =
  let buffer = Buffer.create 4196 in
  let formatter = Format.formatter_of_buffer buffer in
  let ast = gen_ast (proto, role) (start, g) in
  Pprintast.structure formatter ast ;
  Format.pp_print_flush formatter () ;
  Buffer.contents buffer
