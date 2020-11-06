val parse_pragmas_string : string -> Syntax.pragmas
(** Parse a string into {!Syntax.pragmas}. *)

val nested_protocol_enabled : Syntax.scr_module -> bool
(** Whether nested protocol pragma is enabled for a given module *)
