(** An Lexer error *)
exception LexError of string

val token : Stdlib.Lexing.lexbuf -> Parser.token
(** Obtain an token from a lexbuf *)
