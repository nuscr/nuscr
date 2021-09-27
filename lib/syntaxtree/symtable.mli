open Names
open Syntax

type t

(** Record containing a protocol's signature *)
type protocol_decl =
  { proto_name: ProtocolName.t
  ; all_roles: RoleName.t list
  ; split_decl: RoleName.t list * RoleName.t list
  ; loc: Loc.t }

val show_roles : RoleName.t list * RoleName.t list -> string

val create : ProtocolName.t -> global_protocol list -> t option -> t

val lookup_protocol : t -> ProtocolName.t -> protocol_decl

val name_with_prefix : ProtocolName.t -> ProtocolName.t -> ProtocolName.t
