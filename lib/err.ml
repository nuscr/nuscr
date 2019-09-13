type user_error =
  | LexerError of string
  | ParserError of (Lexing.position * Lexing.position)
  | Uncategorised of string

exception UserError of user_error
(** UserError is a user error and should be reported back so it can be fixed *)

let show_user_error = function
  | LexerError msg -> "Lexer error: " ^ msg
  | ParserError interval ->
      "Parser error: An error occurred at "
      ^ Syntax.render_pos_interval interval
  | Uncategorised msg -> "Error " ^ msg

exception Violation of string
(** A Violation is reported when an impossible state was reached. It has to
    be considered a bug even when the fix is to change the Violation to a
    user error *)
