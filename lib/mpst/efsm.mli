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

(** Information regarding recursion variables *)
type rec_var_info = (bool * Gtype.rec_var) list Map.M(Int).t

(** Type of the EFSM, rec_var_info is only populated when refinement types
    are enabled*)
type t = G.t * rec_var_info

val of_local_type : Ltype.t -> state * t
(** Construct an EFSM from a local type *)

val show : t -> string
(** Produce a DOT representation of EFSM, which can be visualised by Graphviz *)

val state_action_type :
     G.t
  -> state
  -> [`Send of RoleName.t | `Recv of RoleName.t | `Mixed | `Terminal]
(** Returns whether a state in the EFSM is a sending state (with only [SendA]
    outgoing actions), a receiving state (with only [RecvA] outgoing
    actions), a mixed state (with a mixture of [SendA] and [RecvA] actions),
    or a terminal state (without outgoing actions) *)

val find_all_payloads : G.t -> Set.M(PayloadTypeName).t

val find_all_roles : G.t -> Set.M(RoleName).t
