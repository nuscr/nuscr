open Names

type payload =
  | PValue of VariableName.t option * Expr.payload_type
  | PDelegate of ProtocolName.t * RoleName.t
[@@deriving eq, sexp_of, show, ord]

(** A message in a global type carries a label, and a list of payloads. *)
type message = {label: LabelName.t; payload: payload list}
[@@deriving eq, sexp_of, show, ord]

val equal_pvalue_payload : payload -> payload -> bool

val typename_of_payload : payload -> PayloadTypeName.t

val of_syntax_payload : Syntax.payloadt -> payload

val of_syntax_message : Syntax.message -> message
