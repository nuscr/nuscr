(** Main entry point of the library *)
open Names

(** {1 Parsing} *)

(** This section deals with parsing protocols. *)

val parse_string : string -> Syntax.scr_module
(** Parse a string into a {!Syntax.scr_module}. *)

val parse : string -> Stdio.In_channel.t -> Syntax.scr_module
(** Parse from an input channel. The first parameter is the filename, for use
    in error messages. *)

(** {1 Validation} *)

val validate_exn : Syntax.scr_module -> verbose:bool -> unit
(** [validate_exn module ~verbose] validates the module [module] by
    performing standard checks. If [verbose] is set to true, output messages
    in the process. *)

(** {1 Other operations} *)

val enumerate : Syntax.scr_module -> (ProtocolName.t * RoleName.t) list
(** [enumerate module] enumrates the roles occurring in [module]. The output
    is a list of pair [(protocol, role-name)]. *)

val project_role :
  Syntax.scr_module -> protocol:ProtocolName.t -> role:RoleName.t -> Ltype.t
(** [project_role module protocol role] computes the local type for role
    [role] in the protocol [protocol]. *)

val generate_fsm :
     Syntax.scr_module
  -> protocol:ProtocolName.t
  -> role:RoleName.t
  -> Efsm.state * Efsm.t
(** [generate_fsm module protocol role] computes the finite state machine of
    role [role] in protocol [protocol], in module [module]. It returns a pair
    [(v, g)] where [g] is the graph describing the fsm, and [v] is the root
    index. *)

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

val generate_ocaml_code :
     monad:bool
  -> Syntax.scr_module
  -> protocol:ProtocolName.t
  -> role:RoleName.t
  -> string
(** [generate_code ~monad module protocol role] generates event-style OCaml
    code for the [role] in [protocol], inside a [module] [monad] indicates
    whether the generated code uses a monad for transport (e.g. Lwt, Async) *)

val generate_ast :
     monad:bool
  -> Syntax.scr_module
  -> protocol:ProtocolName.t
  -> role:RoleName.t
  -> Ppxlib_ast.Parsetree.structure
(** [generate_ast ~monad module protocol role] is similar to [generate_code],
    except it returns an AST instead of a string *)
