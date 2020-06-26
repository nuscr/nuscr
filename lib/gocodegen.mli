open! Base
open Names

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
