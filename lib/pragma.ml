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
      Err.Violation
        ("Found a problem while parsing pragmas:" ^ Exn.to_string e)
      |> raise

let parse_pragmas_string string =
  parse_pragmas_from_lexbuf @@ Lexing.from_string string

let nested_protocol_enabled ast =
  List.exists
    ~f:(fun (p, _) -> Poly.( = ) p Syntax.NestedProtocols)
    ast.Syntax.pragmas
