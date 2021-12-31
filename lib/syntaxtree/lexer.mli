(** An Lexer error *)
exception LexError of string

val token : Sedlexing.lexbuf -> Parser.token
(** Obtain an token from a lexbuf *)
