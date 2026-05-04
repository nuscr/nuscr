open Names

(** Payload carried by a message. [PValue] represents ordinary data,
    optionally bound to a variable name for refinements. [PDelegate]
    represents protocol delegation to a role. *)
type payload =
  | PValue of VariableName.t option * Expr.payload_type
  | PDelegate of ProtocolName.t * RoleName.t
[@@deriving eq, sexp_of, show, ord]

(** A message in a global type carries a label, and a list of payloads. *)
type message = {label: LabelName.t; payload: payload list}
[@@deriving eq, sexp_of, show, ord]

val equal_pvalue_payload : payload -> payload -> bool
(** Compare payloads including value payload variable names. *)

val typename_of_payload : payload -> PayloadTypeName.t
(** Return the payload type name for value payloads. Delegated payloads are
    not currently supported by code generation and raise
    {!Err.UnImplemented}. *)

val of_syntax_payload : Syntax.payloadt -> payload
(** Convert a parsed syntax payload into the MPST payload representation. *)

val of_syntax_message : Syntax.message -> message
(** Convert a parsed syntax message into the MPST message representation. *)

val payloads_compatible : payload list -> payload list -> bool
(** Check whether two payload lists can share a guarded duplicate label. The
    lists must have the same arity, variable names, and base payload types.
*)

val extract_message_guard : message -> Expr.t option
(** Extract the conjunction of payload-local refinement predicates for a
    message. Non-local predicates are ignored here. *)

val guards_disjoint : payload list -> Syntax.expr -> Syntax.expr -> bool
(** Use the refinement solver to decide whether two guards are disjoint under
    the base types of the supplied payloads. *)

val split_guard :
  Base.Set.M(VariableName).t -> Expr.t -> Expr.t option * Expr.t option
(** Split a guard into predicates whose free variables are all in the
    provided payload-variable set, and predicates that mention other
    variables. *)
