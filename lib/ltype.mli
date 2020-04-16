(** Local type management *)

open! Base
(** This module defines local types and basic operations on them. *)

open Names

(** Local types. *)
type t =
  | RecvL of Gtype.message * RoleName.t * t
      (** [RecvL (msg, name, t)] waits for message [msg] from [name] and
          continues as [t] *)
  | SendL of Gtype.message * RoleName.t * t
      (** [SendL (msg, name, t)] sends message [msg] to [name] and continues
          as [t] *)
  | ChoiceL of RoleName.t * t list
      (** [ChoiceL (name, ts)] is a choice (internal or external) from [name]
          between the [ts] *)
  | TVarL of TypeVariableName.t  (** Recursive variable *)
  | MuL of TypeVariableName.t * t  (** Fixpoint *)
  | EndL  (** Empty type *)
  | InviteCreateL of RoleName.t list * RoleName.t list * ProtocolName.t * t
      (** Sending invitations to existing roles and setting up dynamic
          pariticipants *)
  | AcceptL of
      (* accept role' from X in Proto_role'(roles...; new roles'...) *)
      RoleName.t
      * RoleName.t
      * ProtocolName.t
      * RoleName.t list
      * RoleName.t list
      * t

val show : t -> string
(** Converts a local type to a string. *)

val project : RoleName.t -> Gtype.t -> t
(** Project a global type into a particular role. *)

module type S = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  include Comparable.S with type t := t
end

module LocalProtocolId : S

(* TODO: add documentation or remove *)
val build_local_proto_lookup_table :
     Gtype.global_t
  -> ( LocalProtocolId.t
     , string
     , LocalProtocolId.comparator_witness )
     Base.Map.t

val show_lookup_table :
     ( LocalProtocolId.t
     , string
     , LocalProtocolId.comparator_witness )
     Base.Map.t
  -> string
