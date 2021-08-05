open Syntaxtree.Syntax

val expand_global_protocol : scr_module -> global_protocol -> global_protocol
(** Expand `do` constructs in global protocols into `rec` and `continues` *)

val ensure_no_nested_protocols : scr_module -> unit
(** Ensure no nested protocols are present in the module when
    [NestedProtocols] pragma is disabled *)

val validate_calls_in_protocols : scr_module -> unit
(** Ensure all calls to global/nested protocols are valid when
    [NestedProtocols] pragma is enabled *)

val rename_nested_protocols : scr_module -> scr_module
(** Rename only nested protocols so they have unique ids when
    [NestedProtocols] pragam is enabled. Global protocols and calls to global
    protocols (do <proto>(...);) remain the same *)
