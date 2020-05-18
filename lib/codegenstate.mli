open Names
open! Base

module UniqueNameGen : sig
  type t

  val create : unit -> t

  val unique_name : t -> string -> t * string

  val curr_unique_name : t -> string -> string option
end

type codegen_result =
  { messages: (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; channels: (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; invite_channels:
      (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; results: (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; impl:
      ( LocalProtocolName.t
      , string
      , LocalProtocolName.comparator_witness )
      Map.t
  ; callbacks:
      ( LocalProtocolName.t
      , string
      , LocalProtocolName.comparator_witness )
      Map.t
  ; protocol_setup:
      (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; entry_point: string }

val gen_code :
     RootDirName.t
  -> ProtocolName.t
  -> Gtype.global_t
  -> Ltype.local_t
  -> codegen_result

val ensure_unique_identifiers : Gtype.global_t -> unit

val pkg_messages : PackageName.t

val pkg_channels : PackageName.t

val pkg_invitations : PackageName.t

val pkg_callbacks : PackageName.t

val pkg_roles : PackageName.t

val pkg_protocol : PackageName.t

val pkg_results : PackageName.t

val protocol_pkg_name : ProtocolName.t -> string

val messages_file_name : string

val channels_file_name : string

val results_file_name : string

val protocol_file_name : ProtocolName.t -> string

val invitations_file_name : ProtocolName.t -> string

val callbacks_file_name : LocalProtocolName.t -> string

val role_impl_file_name : LocalProtocolName.t -> string

val protocol_setup_file_name : ProtocolName.t -> string
