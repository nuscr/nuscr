(** Code generation from EFSM *)

(** This module transforms a EFSM generated from local types into OCaml code. *)

open Names
open! Ppxlib_ast

val gen_ast :
     ?monad:bool
  -> ProtocolName.t * RoleName.t
  -> Efsm.state * Efsm.t
  -> Parsetree.structure
(** Generate AST presentation of a projected EFSM *)

val gen_code :
  ?monad:bool -> ProtocolName.t * RoleName.t -> Efsm.state * Efsm.t -> string
(** Generate string representation of a projected EFSM *)
