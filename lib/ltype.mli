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
      (* accept role'@Proto(roles...; new roles'...) from X *)
      RoleName.t
      * ProtocolName.t
      * RoleName.t list
      * RoleName.t list
      * RoleName.t
      * t

module type S = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  include Comparable.S with type t := t
end

module LocalProtocolId : S

type local_t =
  ( LocalProtocolId.t
  , RoleName.t list * t
  , LocalProtocolId.comparator_witness )
  Map.t

val show : t -> string
(** Converts a local type to a string. *)

val show_local_t : local_t -> string

val project : RoleName.t -> Gtype.t -> t
(** Project a global type into a particular role. *)

val project_global_t : Gtype.global_t -> local_t
(** Generate the local protocols for a given global_t *)

type local_proto_name_lookup =
  ( LocalProtocolId.t
  , ProtocolName.t
  , LocalProtocolId.comparator_witness )
  Map.t
(** Mapping from pair of (protocol name, role) to unique local protocol name *)

val build_local_proto_name_lookup : local_t -> local_proto_name_lookup

val show_lookup_table : local_proto_name_lookup -> string
