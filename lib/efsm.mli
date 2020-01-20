(** Endpoint finite state machines (EFSM) *)

open Syntax
open Names

(** Transitions in the EFSM *)
type action =
  | SendA of RoleName.t * message  (** Sending a [message] to [name] *)
  | RecvA of RoleName.t * message  (** Receiving a [message] from [name] *)
  | Epsilon  (** Not used *)

type state = int
(** Type of states in EFSM *)

(** EFSM graph representation *)
module G :
  Graph.Sig.P
    with type V.t = state
     and type E.label = action
     and type E.t = state * action * state

type t = G.t
(** Type of the EFSM *)

val of_local_type : Ltype.t -> state * t
(** Construct an EFSM from a local type *)

val show : t -> string
(** Produce a DOT representation of EFSM, which can be visualised by Graphviz *)
