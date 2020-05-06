open Names
open! Base

type codegen_result =
  { channels: (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; invite_channels:
      (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; impl:
      ( LocalProtocolName.t
      , string
      , LocalProtocolName.comparator_witness )
      Map.t
  ; callbacks:
      ( LocalProtocolName.t
      , string
      , LocalProtocolName.comparator_witness )
      Map.t }

val gen_code :
     RootDirName.t (* -> ProtocolName.t *)
  -> Gtype.global_t
  -> Ltype.local_t
  -> codegen_result
