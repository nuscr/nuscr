type user_error =
  | LexerError of string
  | ParserError of (Lexing.position * Lexing.position)
  | UnboundRecursionName of string * (Lexing.position * Lexing.position)
  | RedefinedRecursionName of
      string
      * (Lexing.position * Lexing.position)
      * (Lexing.position * Lexing.position)
  | Uncategorised of string

exception UserError of user_error
(** UserError is a user error and should be reported back so it can be fixed *)

let show_user_error = function
  | LexerError msg -> "Lexer error: " ^ msg
  | ParserError interval ->
      "Parser error: An error occurred at "
      ^ Syntax.render_pos_interval interval
  | UnboundRecursionName (name, interval) ->
      "Unbound name " ^ name ^ " in `continue` at "
      ^ Syntax.render_pos_interval interval
  | RedefinedRecursionName (name, interval1, interval2) ->
      "Redefined name " ^ name ^ " of `rec` at "
      ^ Syntax.render_pos_interval interval1
      ^ " and "
      ^ Syntax.render_pos_interval interval2
  | Uncategorised msg -> "Error " ^ msg

exception Violation of string
(** A Violation is reported when an impossible state was reached. It has to
    be considered a bug even when the fix is to change the Violation to a
    user error *)
