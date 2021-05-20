open! Base
open Names
open Syntaxtree

(** Global types *)

type payload =
  | PValue of VariableName.t option * Expr.payload_type
  | PDelegate of ProtocolName.t * RoleName.t
[@@deriving eq, sexp_of, show, ord]

type message = {label: LabelName.t; payload: payload list}
[@@deriving eq, sexp_of, show, ord]

val equal_pvalue_payload : payload -> payload -> bool

val typename_of_payload : payload -> PayloadTypeName.t

(** Recursion variable *)
type rec_var =
  { rv_name: VariableName.t  (** Variable Name *)
  ; rv_roles: RoleName.t list  (** Which roles know this variable *)
  ; rv_ty: Expr.payload_type  (** What type does the variable carry *)
  ; rv_init_expr: Expr.t
        (** What is the initial expression assigned at the beginning of
            recursion *) }
[@@deriving sexp_of, eq]

(** The type of global types *)
type t =
  | MessageG of message * RoleName.t * RoleName.t * t
      (** [MessageG (msg, sender, receiver, t)] starts by sending message
          [msg] from [sender] to [receiver] and continues as [t] *)
  | MuG of TypeVariableName.t * rec_var list * t
  | TVarG of TypeVariableName.t * Expr.t list * t Lazy.t
  | ChoiceG of RoleName.t * t list
      (** [ChoiceG (name, ts)] expresses a choice located at participant
          [name] between the [ts] *)
  | EndG  (** Empty global type *)
  | CallG of RoleName.t * ProtocolName.t * RoleName.t list * t
      (** [CallG (caller, protocol, participants, t)] - [caller] calls
          [protocol], inviting [participants] to carry out the roles in
          [protocol] (dynamic roles in nested protocols are not included) *)
[@@deriving sexp_of]

(** Mapping of protocol name to the roles ('static' participants, dynamic
    participants) participating in the protocol, the names of the nested
    protocols defined inside it and its global type*)
type global_t =
  ((RoleName.t list * RoleName.t list) * ProtocolName.t list * t)
  Map.M(ProtocolName).t

val show : t -> string
(** Provides a textual representation of a global type *)

val show_global_t : global_t -> string
(** Provides a textual representation of a global type with nested protocols *)

val call_label :
  RoleName.t -> ProtocolName.t -> RoleName.t list -> LabelName.t
(** Generates a unique label for a protocol call based on the caller, the
    protocol called and the participants involved *)

val of_protocol : Syntax.global_protocol -> t
(** Turn a raw protocol (from the parser) into a global type, optional
    argument [refined] determines whether refinement types are enabled. *)

val global_t_of_module : Syntax.scr_module -> global_t
(** Turn scribble module (from the parser) into a global type *)

val normalise : t -> t
(** Normalise a global type. This mainly collapses nested choice on the same
    participant and unfolds fixpoints *)

val normalise_global_t : global_t -> global_t
(** Apply normalisation to all protocols in global_t *)

val validate_refinements_exn : t -> unit
(** Validate refinements in the given global type, requires [RefinementTypes]
    pragma *)

val show_rec_var : rec_var -> string
(** Convert rec_var to string *)
