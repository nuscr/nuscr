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
(** Record containing the implementation of all the components of the Go
    protocol implementation *)

val gen_code :
     RootDirName.t
  -> ProtocolName.t
  -> Gtype.global_t
  -> Ltype.local_t
  -> codegen_result
(** Function which generates the Go implementation for the entry-point
    protocol and all the protocols in the Scribble module*)

val ensure_unique_identifiers : Gtype.global_t -> unit
(** Function which verifies that the Scribble module satisfies the
    constraints needed in order to be able to generate implementation. Some
    of these constraints may be too restrictive right now, but they should be
    easy to relax *)
