(** EFSM analysis utilities for Rust monitor code generation *)

open! Base
open Names
open Message
open Gtype
open Efsm

val upper_camel_case : string -> string

val find_payload_vars : message -> (VariableName.t * Expr.payload_type) list

val rm_silent_var : rec_var_info -> rec_var list Map.M(Int).t
(** Strip the [bool] silent-flag from [rec_var_info], raising on any silent
    var. Must be called before [compute_var_map]. *)

val compute_var_map :
     state
  -> G.t
  -> rec_var list Map.M(Int).t
  -> (VariableName.t * Expr.payload_type) list Map.M(Int).t
(** Walk the EFSM from [start], returning the set of variables in scope at
    each reachable state. Takes the output of [rm_silent_var], not raw
    [rec_var_info]. *)

val collect_labels : G.t -> Set.M(String).t

val collect_labels_with_fields :
     G.t
  -> (VariableName.t * Expr.payload_type) list Map.M(String).t
(** Collect every unique label in the EFSM and, for each, the union of all
    named payload fields across all edges that use that label. *)

val collect_accepts_arms :
     G.t
  -> ((VariableName.t * Expr.payload_type) list * Expr.t list) Map.M(String).t
(** Group EFSM edges by "dir:label" key. For each group, collect the union of
    payload variables and the list of message guards. *)
