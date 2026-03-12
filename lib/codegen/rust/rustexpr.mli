(** Compile nuscr expressions and types to Rust syntax fragments *)

open Names
open Message

val rust_show_expr : Expr.t -> string
(** Compile an expression AST to a Rust expression string *)

val rust_type_of_payload_type : Expr.payload_type -> string
(** Map a payload type to its Rust type name, stripping refinements *)

val rust_validate_identifier : VariableName.t -> unit
(** Raise if the variable name clashes with a Rust keyword *)

val rust_value_pattern_of_payload : VariableName.t option -> Expr.payload_type -> string
(** Build a Rust pattern for matching a Value enum variant *)

val rust_payload_slice_pattern : payload list -> string
(** Build a Rust slice pattern for a message's payload list *)

val rust_payload_constraints : payload list -> string option
(** Extract refinement predicates from payloads, conjoined with &&.
    Returns [None] when no payloads carry refinements. *)
