open Syntax

let message = "//This is an invisible comment.\n(* This (* too *)*)\n module (* other stuff *)"

let render_pos (pos : Lexing.position) : string =
  Printf.sprintf "line: %d, column %d"
    pos.Lexing.pos_lnum pos.Lexing.pos_bol

let render_pos_interval (startp, endp) : string =
  Printf.sprintf "from %s to %s"
    (render_pos startp)
    (render_pos endp)

let process (prg : string) : string =
  let lexbuf = Lexing.from_string prg in
  try
    let ast = Parser.main Lexer.token lexbuf in
    match ast.value with
    | Con str -> str
  with
  | Lexer.LexError msg -> Err.UserError ("Lexer error: " ^ msg) |> raise
  | Parser.Error ->
    let err_interval = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf in
    Err.UserError (Printf.sprintf
                     "Parser error: An error occured at:\n %s\n%!"
                     (render_pos_interval err_interval))
    |> raise
  | e  -> Err.Violation ("Found a problem:" ^ Printexc.to_string e) |> raise
