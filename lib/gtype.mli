open! Base
open Names

(** Global types *)

type expr =
  | Var of VariableName.t
  | Int of int
  | Bool of bool
  | String of string
  | Binop of Syntax.binop * expr * expr
  | Unop of Syntax.unop * expr
[@@deriving sexp_of, eq, ord, show]

type payload_type =
  | PTSimple of PayloadTypeName.t
  | PTRefined of VariableName.t * PayloadTypeName.t * expr
[@@deriving sexp_of, eq, ord]

type payload =
  | PValue of VariableName.t option * payload_type
  | PDelegate of ProtocolName.t * RoleName.t
[@@deriving eq, sexp_of, show, ord]

type message = {label: LabelName.t; payload: payload list}
[@@deriving eq, sexp_of, show, ord]

val equal_pvalue_payload : payload -> payload -> bool

val payload_typename_of_payload_type : payload_type -> PayloadTypeName.t

(** The type of global types *)
type t =
  | MessageG of message * RoleName.t * RoleName.t * t
      (** [MessageG (msg, sender, receiver, t)] starts by sending message
          [msg] from [sender] to [receiver] and continues as [t] *)
  | MuG of TypeVariableName.t * t  (** Fixpoint *)
  | TVarG of TypeVariableName.t * t Lazy.t  (** Recursive variable *)
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

val of_protocol : ?refined:bool -> Syntax.global_protocol -> t
(** Turn a raw protocol (from the parser) into a global type, optional
    argument [refined] determines whether refinement types are enabled. *)

val global_t_of_module : Syntax.scr_module -> global_t
(** Turn scribble module (from the parser) into a global type *)

val global_t_of_ast : Syntax.scr_module -> global_t
(** Turn scribble ast (from the parser) into a global type *)

val normalise : t -> t
(** Normalise a global type. This mainly collapses nested choice on the same
    participant and unfolds fixpoints *)

val normalise_global_t : global_t -> global_t
(** Apply normalisation to all protocols in global_t *)

val replace_recursion_with_nested_protocols : global_t -> global_t
(** Replace the MuG type with explicit calls to nested protocols*)
