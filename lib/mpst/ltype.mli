(** Local type management *)

(** This module defines local types and basic operations on them. *)
open! Base

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
  | TVarL of TypeVariableName.t * Expr.t list  (** Recursive variable *)
  | MuL of TypeVariableName.t * (bool (* Silent? *) * Gtype.rec_var) list * t
      (** Fixpoint *)
  | EndL  (** Empty type *)
  | InviteCreateL of RoleName.t list * RoleName.t list * ProtocolName.t * t
      (** Send invitations to existing roles and set up/create dynamic
          pariticipants *)
  | AcceptL of
      RoleName.t
      * ProtocolName.t
      * RoleName.t list
      * RoleName.t list
      * RoleName.t
      * t  (** accept role'\@Proto(roles...; new roles'...) from X; t *)
  | SilentL of VariableName.t * Expr.payload_type * t
      (** Used with refinement types to indicate knowledge obtained via a
          global protocol. *)

module type S = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val create : ProtocolName.t -> RoleName.t -> t

  val get_role : t -> RoleName.t

  val get_protocol : t -> ProtocolName.t

  include Comparable.S with type t := t
end

(** Unique id identifying a local protocol *)
module LocalProtocolId : S

(** Mapping of local protocol id to the protocol's roles and local type *)
type local_t = (RoleName.t list * t) Map.M(LocalProtocolId).t

val show : t -> string
(** Converts a local type to a string. *)

val show_local_t : local_t -> string

val project : RoleName.t -> Gtype.t -> t
(** Project a global type into a particular role. *)

val project_global_t : Gtype.global_t -> local_t
(** Generate the local protocols for a given global_t *)

val ensure_unique_tvars : local_t -> local_t
(** Ensure that all the local variables in each local protocol are globally
    unique, renaming variables to resolve conflicts *)

(** Mapping from local protocol ids to their unique local protocol names *)
type local_proto_name_lookup = LocalProtocolName.t Map.M(LocalProtocolId).t

val build_local_proto_name_lookup : local_t -> local_proto_name_lookup
(** Builds a map containing the unique string representations for the unique
    local protocol ids *)

val show_lookup_table : local_proto_name_lookup -> string
(** Converts a local protocol name lookup table to a string *)

val lookup_local_protocol :
     local_proto_name_lookup
  -> ProtocolName.t
  -> RoleName.t
  -> LocalProtocolName.t
(** Return the unique local protocol name for a (role, protocol) pair *)

val lookup_protocol_id :
  local_proto_name_lookup -> LocalProtocolId.t -> LocalProtocolName.t
(** Look up the unique name for a local protocol id *)
