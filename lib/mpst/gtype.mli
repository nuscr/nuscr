open! Base
open Names

(** Global types *)

(* An abstract type for a parse tree *)
type scr_module = Syntax.scr_module

(* An abstract type for a global protocol in the parse tree *)
type global_protocol = Syntax.global_protocol

(** Recursion variable *)
type rec_var =
  { rv_name: VariableName.t  (** Variable Name *)
  ; rv_roles: RoleName.t list  (** Which roles know this variable *)
  ; rv_ty: Expr.payload_type  (** What type does the variable carry *)
  ; rv_init_expr: Expr.t
        (** What is the initial expression assigned at the beginning of
            recursion *) }
[@@deriving sexp_of, eq, show]

(** The type of global types. See also {!LiteratureSyntax.global} for a
    simpler syntax. *)
type t =
  | MessageG of Message.message * RoleName.t * RoleName.t * t
      (** [MessageG (msg, sender, receiver, t)] starts by sending message
          [msg] from [sender] to [receiver] and continues as [t] *)
  | MuG of TypeVariableName.t * rec_var list * t
      (** [MuG (type_var, rec_vars, g)] is a recursive type, corresponding to
          the syntax `\mu t. G`, where t is represented by [type_var] and G
          is represented by [t]. [rec_vars] are recursion parameters, used in
          RefinementTypes extension for parameterised recursion, an empty
          list is supplied when that feature is not used. *)
  | TVarG of TypeVariableName.t * Expr.t list * t Lazy.t
      (** [TVarG (type_var, exprs, g_lazy)] is a type variable, scoped inside
          a recursion. [type_var] is the name of the type variable, [exprs]
          are expressions supplied into paramterised recursion, used in
          RefinementTypes extension. Otherwise an empty list is supplied when
          that feature is not used. [g_lazy] provides a convenient way to
          access the type that the type variable recurses into. *)
  | ChoiceG of RoleName.t * t list
      (** [ChoiceG (name, ts)] expresses a choice located at participant
          [name] between the [ts] *)
  | EndG  (** Empty global type *)
  | CallG of RoleName.t * ProtocolName.t * RoleName.t list * t
      (** [CallG (caller, protocol, participants, t)] - [caller] calls
          [protocol], inviting [participants] to carry out the roles in
          [protocol] (dynamic roles in nested protocols are not included) *)
  | CombineG of t * t list
[@@deriving sexp_of, show]

(** Mapping of protocol name to the roles ('static' participants, dynamic
    participants) participating in the protocol, the names of the nested
    protocols defined inside it and its global type*)

type nested_global_info =
  { static_roles: RoleName.t list
  ; dynamic_roles: RoleName.t list
  ; nested_protocol_names: ProtocolName.t list
  ; gtype: t }

type nested_t = nested_global_info Map.M(ProtocolName).t

val show_nested_t : nested_t -> string
(** Provides a textual representation of a global type with nested protocols *)

val call_label :
  RoleName.t -> ProtocolName.t -> RoleName.t list -> LabelName.t
(** Generates a unique label for a protocol call based on the caller, the
    protocol called and the participants involved *)

val of_protocol : global_protocol -> t
(** Turn a raw protocol (from the parser) into a global type. *)

val nested_t_of_module : scr_module -> nested_t
(** Turn scribble module (from the parser) into a nested global type *)

val normalise : t -> t
(** Normalise a global type. This mainly collapses nested choice on the same
    participant and unfolds fixpoints *)

val normalise_nested_t : nested_t -> nested_t
(** Apply normalisation to all protocols in nested_t *)

val validate_refinements_exn : t -> unit
(** Validate refinements in the given global type, requires [RefinementTypes]
    pragma *)
