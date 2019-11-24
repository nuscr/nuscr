open! Core_kernel
open! Migrate_parsetree

(* open Efsm *)

open! Ast_407
open Parsetree
open Efsm

let state_action_type (g : G.t) (st : int) =
  let merge_state_action_type aty1 aty2 =
    match (aty1, aty2) with
    | `Terminal, aty2 -> aty2
    | aty1, `Terminal -> aty1
    | `Send, `Send -> `Send
    | `Recv, `Recv -> `Recv
    | `Send, `Recv -> `Mixed
    | `Recv, `Send -> `Mixed
    | aty1, `Mixed -> aty1
    | `Mixed, aty2 -> aty2
  in
  let f (_, a, _) acc =
    let aty =
      match a with
      | SendA _ -> `Send
      | RecvA _ -> `Recv
      | Epsilon -> failwith "Impossible"
    in
    merge_state_action_type aty acc
  in
  G.fold_succ_e f g st `Terminal

let gen_ast (_proto, _role) (_start, _g) : structure =
  let loc = Location.none in
  let vb = [%stri let hello = "hello"] in
  [vb]

let migrate = Versions.migrate Versions.ocaml_407 Versions.ocaml_current

let gen_code (proto, role) (start, g) =
  let buffer = Buffer.create 4196 in
  let formatter = Format.formatter_of_buffer buffer in
  let ast = gen_ast (proto, role) (start, g) in
  let ast = migrate.Versions.copy_structure ast in
  Pprintast.structure formatter ast ;
  Format.pp_print_flush formatter () ;
  Buffer.contents buffer
