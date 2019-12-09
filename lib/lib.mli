(** Main entry point of the library *)

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

val enumerate : Syntax.scr_module -> (Name.t * Name.t) list
(** [enumerate module] enumrates the roles occurring in [module]. The output
    is a list of pair [(protocol, role-name)]. *)

val project_role : Syntax.scr_module -> Name.t -> Name.t -> Ltype.t
(** [project_role module protocol role] computes the local type for role
    [role] in the protocol [protocol]. *)

val generate_fsm :
  Syntax.scr_module -> Name.t -> Name.t -> Efsm.state * Efsm.t
(** [generate_fsm module protocol role] computes the finite state machine of
    role [role] in protocol [protocol], in module [module]. It returns a pair
    [(v, g)] where [g] is the graph describing the fsm, and [v] is the root
    index. *)

val generate_code :
  monad:bool -> Syntax.scr_module -> Name.t -> Name.t -> string
(** [generate_code ~monad module protocol role] generates event-style OCaml
    code for the [role] in [protocol], inside a [module] [monad] indicates
    whether the generated code uses a monad for transport (e.g. Lwt, Async) *)

val generate_ast :
     monad:bool
  -> Syntax.scr_module
  -> Name.t
  -> Name.t
  -> Ppxlib_ast.Parsetree.structure
(** [generate_ast ~monad module protocol role] is similar to [generate_code],
    except it returns an AST instead of a string *)
