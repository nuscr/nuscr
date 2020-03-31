{
  open Lexing
  open Parser

  exception LexError of string

  (* State for the pragma lexer to distinguish btw reading a pragma and
     skipping the program *)
  let in_pragma = ref false
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

(* let ext_identifier = '\"' (letter|digit|symbol)* '\"' *)

let ext_identifier = '\"' [^'\"' '\n']* '\"'

(* nuScr extension, removed \u000C *)
(* let whitespace = ('\t'|' '|'\r'|'\n')+ *)

(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)

rule line_comment = parse
| '\n' {  new_line lexbuf ; token lexbuf }
| _ { line_comment lexbuf }

and ignore_pragma = parse
| "#*)" { token lexbuf }
| _ { ignore_pragma lexbuf }

(* nuScr extension, nestable ml style comments *)
and ml_style_block n = parse
| "(*)" { ml_style_block n lexbuf }
| "*)" { if (n-1) = 0 then token lexbuf else ml_style_block (n-1) lexbuf }
| "(*" { ml_style_block (n+1) lexbuf }
| '\n' { new_line lexbuf ; ml_style_block n lexbuf }
| _ { ml_style_block n lexbuf }

and c_style_block = parse
| "*/" { token lexbuf }
| '\n' { new_line lexbuf ; c_style_block lexbuf }
| _ { c_style_block lexbuf }

and token = parse
(* whitespace *)
| ('\t'|' ')+ { token lexbuf}
| ('\n'|'\r') { new_line lexbuf ; token lexbuf}

(* comments and pragmas *)
| "//" { line_comment lexbuf }
| "(*)" { line_comment lexbuf }  (* nuScr extension: ml-style line comments *)
| "/*" { c_style_block lexbuf }
| "(*#" { ignore_pragma lexbuf }
| "(*" { ml_style_block 1 lexbuf }

(* symbols *)
| ',' { COMMA }
| ';' { SEMICOLON }
| ':' { COLON }
| '.' { DOT }
| '<' { LT }
| '>' { GT }
| '(' { LPAR }
| ')' { RPAR }
| '{' { LCURLY }
| '}' { RCURLY }
| '@' { ARROBA }

(* keywords *)

| "module" { MODULE_KW }
| "import" { RESERVED }
| "type" { TYPE_KW }
| "protocol" { PROTOCOL_KW }
| "global" { GLOBAL_KW }
| "nested" { NESTED_KW }
| "explicit" { EXPLICIT_KW }
| "aux" { AUX_KW }
| "role" { ROLE_KW }
| "sig" { SIG_KW }
| "as" { AS_KW }
| "connect" { RESERVED }
| "disconnect" { RESERVED }
| "wrap" { RESERVED }
| "and" { RESERVED }
| "from" { FROM_KW }
| "to" { TO_KW }
| "choice" { CHOICE_KW }
| "at" { AT_KW }
| "or" { OR_KW }
| "rec" { REC_KW }
| "continue" { CONTINUE_KW }
| "do" { DO_KW }
| "calls" { CALLS_KW }
| "new" { NEW_KW }


(* other *)
| eof
    { EOI }
| identifier as str { IDENT str }
| ext_identifier as quoted_str {
  let str = String.sub quoted_str 1 (String.length quoted_str - 2) in
  EXTIDENT str }
| _ as unrecog {
  let offset = Lexing.lexeme_start lexbuf in
  let str = Printf.sprintf "At offset %d: unexpected character('%c').\n" offset unrecog in
  LexError str |> raise }

and pragma_lexer = parse
(* whitespace *)
| ('\t'|' ')+ { pragma_lexer lexbuf}
| ('\n'|'\r') { new_line lexbuf ; pragma_lexer lexbuf}
(* pragam structures *)
| identifier as str
  { if !in_pragma then IDENT str else pragma_lexer lexbuf }
| ","
  { if !in_pragma then COMMA else pragma_lexer lexbuf }
| ":"
  { if !in_pragma then COLON else pragma_lexer lexbuf }
| "#*)" {  in_pragma := false ; pragma_lexer lexbuf }


| "(*#" { in_pragma := true ; pragma_lexer lexbuf }
| eof { EOI }
| _ { pragma_lexer lexbuf }
