open! Core_kernel
open Syntax
open Err
open Gtype

let set_filename (fname : string) (lexbuf : Lexing.lexbuf) =
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname= fname} ;
  lexbuf

let process_ch fname (ch : In_channel.t) : string =
  let lexbuf = set_filename fname (Lexing.from_channel ch) in
  try
    let ast = Parser.scr_module Lexer.token lexbuf in
    (* show_scr_module ast *)
    let protocols = ast.protocols in
    try
      let g_types = List.map ~f:global_type_of_protocol protocols in
      String.concat ~sep:"\n" (List.map ~f:show_global_type g_types)
    with
    | _ -> "I'm sorry"
  with
  | Lexer.LexError msg -> Err.UserError (LexerError msg) |> raise
  | Parser.Error ->
      let err_interval =
        (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      Err.UserError (ParserError err_interval) |> raise
  | e -> Err.Violation ("Found a problem:" ^ Exn.to_string e) |> raise

let process_file (fn : string) : string =
  let input = In_channel.create fn in
  let res = process_ch fn input in
  In_channel.close input ; res
