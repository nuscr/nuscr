open! Base
open Err
open Loc

(* pragmas *)
let parse_pragmas_from_lexbuf lexbuf : Syntax.pragmas =
  try Pragmaparser.pragmas Pragmalexer.pragma_lexer lexbuf with
  | Pragmalexer.LexError msg -> uerr (LexerError msg)
  | Pragmaparser.Error ->
      let err_interval =
        (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      uerr (ParserError (build err_interval))
  | e ->
      Err.violation
        ("Found a problem while parsing pragmas:" ^ Exn.to_string e)

let parse_pragmas_string string =
  parse_pragmas_from_lexbuf @@ Lexing.from_string string
