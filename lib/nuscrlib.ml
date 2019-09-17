open Syntax
open Err

let set_filename (fname : string) (lexbuf : Lexing.lexbuf) =
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname= fname} ;
  lexbuf

let process_ch fname (ch : in_channel) : string =
  let lexbuf = set_filename fname (Lexing.from_channel ch) in
  try
    let ast = Parser.scr_module Lexer.token lexbuf in
    show_scr_module ast
  with
  | Lexer.LexError msg -> Err.UserError (LexerError msg) |> raise
  | Parser.Error ->
      let err_interval =
        (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      Err.UserError (ParserError err_interval) |> raise
  | e -> Err.Violation ("Found a problem:" ^ Printexc.to_string e) |> raise

let process_file (fn : string) : string =
  let input = open_in fn in
  let res = process_ch fn input in
  close_in input ; res
