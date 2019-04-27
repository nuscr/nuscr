open Syntax

let message = "I am using dune now"

let process (prg : string) : string =
  let lexbuf = Lexing.from_string prg in
  try
    let ast = Parser.main Lexer.token lexbuf in
    match ast.value with
    | Con str -> str
  with
  | Lexer.LexError msg -> Err.UserError ("Lexer error: " ^ msg) |> raise
  | Parser.Error ->
    Err.UserError (Printf.sprintf
                     "Parser error: At offset %d: syntax error.\n%!"
                     (Lexing.lexeme_start lexbuf))
    |> raise
