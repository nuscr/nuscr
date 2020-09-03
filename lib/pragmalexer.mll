{
  open Lexing
  open Pragmaparser

  exception LexError of string
}

(* some definitions of character classes *)

let underscore = '_' (* perhaps we could remove this *)
let prime = '\''
let letter = ['a'-'z' 'A'-'Z']

(* nuScr extension: add primes to symbols *)
let symbol = ['{' '}' '(' ')' '[' ']' ':' '/' '\\' '.' '#''&' '?' '!' '_' '\'' '|' ',' '=' '>' '<' '+' '-' '*']
let digit = ['0'-'9']

(* in scribble it can be empty, not here *)
(* let identifier = (letter|digit|underscore)* *)


(* nuScr extension, it can contain primes, it cannot be empty and the
   the primes cannot come up first (it is a bit weird that digits can
   come first in identifiers *)
let identifier = (letter|underscore|digit)(letter|digit|underscore|prime)*

(* Parse pragma, which should match the regex: "(*#" [^'#'] "#*)" *)
rule pragma_lexer = parse
(* whitespace *)
| ('\t'|' ')+ { pragma_lexer lexbuf}
| ('\n'|'\r') { new_line lexbuf ; pragma_lexer lexbuf}
(* pragma structures *)
| identifier as str { IDENT str }
| "," { COMMA }
| ":" { COLON }
| "#*)" { pragma_lexer lexbuf }
| "(*#" { pragma_lexer lexbuf }
| eof { EOI }
| _ as unrecog {
  let offset = Lexing.lexeme_start lexbuf in
  let str = Printf.sprintf "At offset %d: unexpected character('%c').\n" offset unrecog in
  LexError str |> raise }