open Syntax

let render_pos pos =
  Printf.sprintf "%d:%d"
    pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let render_pos_interval (startp, endp) : string =
  Printf.sprintf "%s to %s in: %s"
    (render_pos startp)
    (render_pos endp)
    (startp.Lexing.pos_fname)

let set_filename (fname : string) (lexbuf : Lexing.lexbuf) =
    ( lexbuf.Lexing.lex_curr_p <-
        { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }
    ; lexbuf
    )

let process_ch fname (ch : in_channel) : string =
  let lexbuf = set_filename fname (Lexing.from_channel ch) in
  try
    let ast = Parser.scr_module Lexer.token lexbuf in
    ast.decl.value.module_name.value |> qname_to_string
  with
  | Lexer.LexError msg -> Err.UserError ("Lexer error: " ^ msg) |> raise
  | Parser.Error ->
    let err_interval = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf in
    Err.UserError (Printf.sprintf
                     "Parser error: An error occured at:\n %s\n%!"
                     (render_pos_interval err_interval))
    |> raise
  | e  -> Err.Violation ("Found a problem:" ^ Printexc.to_string e) |> raise

let process_file (fn : string) : string =
    let input = open_in fn in
    let res = process_ch fn input in
    close_in input ; res
