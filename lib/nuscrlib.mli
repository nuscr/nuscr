(** Main entry point of the library *)
open Names

(** A type for parsed syntax tree *)
type scr_module

val load_pragmas : scr_module -> unit
(** Load pragmas defined in a parsed syntax tree *)

(** {1 Parsing} *)

(** This section deals with parsing protocols. *)

val parse_string : string -> scr_module
(** Parse a string into a {!scr_module}. *)

val parse : string -> Stdio.In_channel.t -> scr_module
(** Parse from an input channel. The first parameter is the filename, for use
    in error messages. *)

(** {1 Validation} *)

val validate_exn : scr_module -> unit
(** [validate_exn module] validates the module [module] by performing
    standard checks. If verbose is set to true in the config, debugging
    messages will be printed *)

(** {1 Other operations} *)

val protocols_names_of : scr_module -> ProtocolName.t list
(** [protocols_names_of module] returns the list of the names of protocols
    occuring in [module] *)

val enumerate : scr_module -> (ProtocolName.t * RoleName.t) list
(** [enumerate module] enumerates the roles occurring in [module]. The output
    is a list of pair [(protocol, role-name)]. *)

val project_role :
  scr_module -> protocol:ProtocolName.t -> role:RoleName.t -> Ltype.t
(** [project_role module protocol role] computes the local type for role
    [role] in the protocol [protocol]. *)

val get_global_type : scr_module -> protocol:ProtocolName.t -> Gtype.t
(** [get_global_type module ~protocol] gets the corresponding global type for
    a protocol in a module *)

val get_global_type_literature_syntax :
  scr_module -> protocol:ProtocolName.t -> LiteratureSyntax.global
(** [get_global_type_literature_syntax module ~protocol] gets the
    corresponding global type for a protocol in a module in literature syntax *)

val get_global_type_protobuf :
  scr_module -> protocol:ProtocolName.t -> string
(** [get_global_type_protobuf module ~protocol] gets the corresponding global
    type for a protocol in a module in protobuf format *)

val generate_fsm :
     scr_module
  -> protocol:ProtocolName.t
  -> role:RoleName.t
  -> Efsm.state * Efsm.t
(** [generate_fsm module protocol role] computes the finite state machine of
    role [role] in protocol [protocol], in module [module]. It returns a pair
    [(v, g)] where [g] is the graph describing the fsm, and [v] is the root
    index. *)

val generate_go_code :
  scr_module -> out_dir:string -> go_path:string option -> string
(** [generate_code module out_dir go_path] generates Golang implementation
    for [module]. The protocol implementation designed to be a subpackage
    within a project. [out_dir] is the path from the root of the project
    until the package inside which the protocol implementation (subpackage)
    should be generated - it is needed to generate imports. [go_path] is the
    path to the project root, which can optionally be provided in order to
    write the implementation to the file system. *)

val generate_ocaml_code :
     monad:bool
  -> scr_module
  -> protocol:ProtocolName.t
  -> role:RoleName.t
  -> string
(** [generate_code ~monad module protocol role] generates event-style OCaml
    code for the [role] in [protocol], inside a [module] [monad] indicates
    whether the generated code uses a monad for transport (e.g. Lwt, Async) *)

val generate_sexp : scr_module -> protocol:ProtocolName.t -> string
(** [generate_code ~monad module protocol role] generates event-style OCaml
    code for the [role] in [protocol], inside a [module] [monad] indicates
    whether the generated code uses a monad for transport (e.g. Lwt, Async) *)

val generate_ast :
     monad:bool
  -> scr_module
  -> protocol:ProtocolName.t
  -> role:RoleName.t
  -> Ppxlib_ast.Parsetree.structure
(** [generate_ast ~monad module protocol role] is similar to [generate_code],
    except it returns an AST instead of a string *)

val generate_fstar_code :
  scr_module -> protocol:ProtocolName.t -> role:RoleName.t -> string
(** Generate F* code, with support for refinement types *)

module Pragma = Pragma
module Expr = Expr
module Gtype = Gtype
module Ltype = Ltype
module Efsm = Efsm
module Err = Err
module Names = Names
module Loc = Loc
module LiteratureSyntax = LiteratureSyntax
module ProtobufConvert = ProtobufConvert
