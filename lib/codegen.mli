(** Code generation from EFSM *)

(** This module transforms a EFSM generated from local types into OCaml code. *)

open Syntax
open! Ppxlib_ast

val gen_ast : name * name -> int * Efsm.t -> Parsetree.structure
(** Generate AST presentation of a projected EFSM *)

val gen_code : name * name -> int * Efsm.t -> string
(** Generate string representation of a projected EFSM *)
