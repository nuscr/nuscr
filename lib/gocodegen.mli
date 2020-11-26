open! Base
open Names

(** Record containing the implementation of all the components of the Go
    protocol implementation *)
type codegen_result =
  { messages: string Map.M(ProtocolName).t
  ; channels: string Map.M(ProtocolName).t
  ; invite_channels: string Map.M(ProtocolName).t
  ; results: string Map.M(ProtocolName).t
  ; impl: string Map.M(LocalProtocolName).t
  ; callbacks: string Map.M(LocalProtocolName).t
  ; protocol_setup: string Map.M(ProtocolName).t
  ; entry_point: string }

val show_codegen_result :
  codegen_result -> ProtocolName.t -> RootDirName.t -> string
(** Convert the code generation result to a string, showing the contents of
    each generated implementation file *)

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
