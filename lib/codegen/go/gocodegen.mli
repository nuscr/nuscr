open! Base
open Names

val generate_go_code :
     Syntax.scr_module
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
