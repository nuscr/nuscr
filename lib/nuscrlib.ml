open Syntax

let render_pos (pos : Lexing.position) : string =
  Printf.sprintf "line: %d, column %d"
    pos.Lexing.pos_lnum pos.Lexing.pos_bol

let render_pos_interval (startp, endp) : string =
  Printf.sprintf "from %s to %s"
    (render_pos startp)
    (render_pos endp)

let process_ch (ch : in_channel) : string =
  let lexbuf = Lexing.from_channel ch in
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
    let res = process_ch input in
    close_in input ; res
