(** Endpoint finite state machines (EFSM) *)
open! Base

open Names

(** Annotation for refined actions, used when RefinementTypes pragma is
    enabled *)
type refinement_action_annot =
  { silent_vars: (VariableName.t * Expr.payload_type) list
        (** List of silent variables and their types *)
  ; rec_expr_updates: Expr.t list
        (** List of updates to recursion variables *) }
[@@deriving ord, sexp_of]

(** Transitions in the EFSM *)
type action =
  | SendA of RoleName.t * Gtype.message * refinement_action_annot
      (** Sending a [message] to [name] *)
  | RecvA of RoleName.t * Gtype.message * refinement_action_annot
      (** Receiving a [message] from [name] *)
  | Epsilon  (** Not used *)

(** Type of states in EFSM *)
type state = int

(** EFSM graph representation *)
module G :
  Graph.Sig.P
    with type V.t = state
     and type E.label = action
     and type E.t = state * action * state

(** Type of the EFSM *)
type t = G.t

val of_local_type : Ltype.t -> state * t
(** Construct an EFSM from a local type *)

type var_info_entry =
  | RecursionV of VariableName.t * Expr.payload_type * Expr.t
  | RecursionVUpdate of VariableName.t * Expr.t

type var_info = var_info_entry list Map.M(Int).t

val of_local_type_with_var_info : Ltype.t -> state * t * var_info

val show : t -> string
(** Produce a DOT representation of EFSM, which can be visualised by Graphviz *)
