open Sedlexing
open Parser

exception LexError of string

(* some definitions of character classes *)

let underscore = [%sedlex.regexp? '_'] (* perhaps we could remove this *)

let prime = [%sedlex.regexp? '\'']

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let digit = [%sedlex.regexp? '0' .. '9']

(* nuScr extension, it can contain primes, it cannot be empty and the the
   primes cannot come up first (it is a bit weird that digits can come first
   in identifiers *)
let identifier =
  [%sedlex.regexp?
    (letter | underscore | digit), Star (letter | digit | underscore | prime)]

(* let ext_identifier = '\"' (letter|digit|symbol)* '\"' *)
let ext_identifier = [%sedlex.regexp? '\"', Star (Compl ('\"' | '\n')), '\"']

(* nuScr extension, removed \u000C *)
(* let whitespace = ('\t'|' '|'\r'|'\n')+ *)

(* This rule looks for a single line, terminated with '\n' or eof. *)

let rec line_comment lexbuf =
  match%sedlex lexbuf with
  | '\n' -> token lexbuf
  | any -> line_comment lexbuf
  | _ -> failwith "Impossible"

(* nuScr extension, nestable ml style comments *)
and ml_style_block n lexbuf =
  match%sedlex lexbuf with
  | "(*)" -> ml_style_block n lexbuf
  | "*)" -> if n - 1 = 0 then token lexbuf else ml_style_block (n - 1) lexbuf
  | "(*" -> ml_style_block (n + 1) lexbuf
  | '\n' -> ml_style_block n lexbuf
  | any -> ml_style_block n lexbuf
  | _ -> failwith "Impossible"

and c_style_block lexbuf =
  match%sedlex lexbuf with
  | "*/" -> token lexbuf
  | '\n' -> c_style_block lexbuf
  | any -> c_style_block lexbuf
  | _ -> failwith "Impossible"

and token lexbuf =
  match%sedlex lexbuf with
  (* whitespace *)
  | Plus ('\t' | ' ') -> token lexbuf
  | '\n' | '\r' -> token lexbuf
  (* comments and pragmas *)
  | "//" -> line_comment lexbuf
  | "(*#" -> PRAGMA_START
  | "#*)" -> PRAGMA_END
  | "(*)" ->
      line_comment lexbuf (* nuScr extension: ml-style line comments *)
  | "/*" -> c_style_block lexbuf
  | "(*" -> ml_style_block 1 lexbuf
  | Plus '0' .. '9' ->
      let i = Latin1.lexeme lexbuf in
      INT (int_of_string i)
  (* symbols *)
  | ',' -> COMMA
  | ';' -> SEMICOLON
  | ':' -> COLON
  | '.' -> DOT
  | '=' -> EQUAL
  | '+' -> PLUS
  | '-' -> MINUS
  | "&&" -> AMPAMP
  | "||" -> BARBAR
  | "<>" -> LTGT
  | "<=" -> LTEQ
  | '<' -> LT
  | ">=" -> GTEQ
  | '>' -> GT
  | '(' -> LPAR
  | ')' -> RPAR
  | '{' -> LCURLY
  | '}' -> RCURLY
  | '[' -> LSQUARE
  | ']' -> RSQUARE
  | '@' -> ARROBA
  (* keywords *)
  | "not" -> NOT_KW
  | "true" -> TRUE_KW
  | "false" -> FALSE_KW
  | "module" -> MODULE_KW
  | "import" -> RESERVED
  | "type" -> TYPE_KW
  | "protocol" -> PROTOCOL_KW
  | "global" -> GLOBAL_KW
  | "nested" -> NESTED_KW
  | "explicit" -> EXPLICIT_KW
  | "aux" -> AUX_KW
  | "role" -> ROLE_KW
  | "sig" -> SIG_KW
  | "as" -> AS_KW
  | "connect" -> RESERVED
  | "disconnect" -> RESERVED
  | "wrap" -> RESERVED
  | "and" -> RESERVED
  | "from" -> FROM_KW
  | "to" -> TO_KW
  | "choice" -> CHOICE_KW
  | "at" -> AT_KW
  | "or" -> OR_KW
  | "rec" -> REC_KW
  | "continue" -> CONTINUE_KW
  | "do" -> DO_KW
  | "calls" -> CALLS_KW
  | "new" -> NEW_KW
  | "len" -> LEN_KW
  (* other *)
  | eof -> EOI
  | identifier ->
      let str = Utf8.lexeme lexbuf in
      IDENT str
  | ext_identifier ->
      let quoted_str = Utf8.lexeme lexbuf in
      let str = String.sub quoted_str 1 (String.length quoted_str - 2) in
      EXTIDENT str
  | _ ->
      let unrecog = Utf8.lexeme lexbuf in
      let offset = lexeme_start lexbuf in
      let str =
        Printf.sprintf "At offset %d: unexpected character(s)('%s').\n"
          offset unrecog
      in
      LexError str |> raise
