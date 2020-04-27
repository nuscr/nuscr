open Names
open! Base

val ensure_unique_identifiers : Gtype.global_t -> unit

val gen_protocol_msgs :
     Gtype.global_t
  -> (ProtocolName.t, string, ProtocolName.comparator_witness) Base.Map.t

val gen_protocol_channels :
     Gtype.global_t
  -> Ltype.local_t
  -> (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
