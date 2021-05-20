open! Base
open Mpst
open Names

(** Record containing the implementation of all the components of the Go
    protocol implementation *)
type codegen_result =
  { messages: string
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

val generate_go_code :
     Syntaxtree.Syntax.scr_module
  -> protocol:ProtocolName.t
  -> out_dir:string
  -> go_path:string option
  -> string
(** [generate_code module protocol out_dir go_path] generates Golang
    implementation for [protocol]. The protocol implementation designed to be
    a subpackage within a project. [out_dir] is the path from the root of the
    project until the package inside which the protocol implementation
    (subpackage) should be generated - it is needed to generate imports.
    [go_path] is the path to the project root, which can optionally be
    provided in order to write the implementation to the file system. *)
