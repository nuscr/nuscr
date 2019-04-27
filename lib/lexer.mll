{
  open Parser

  exception LexError of string

}

(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)

rule line = parse
| ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
| eof
    (* Normal case: no data, eof. *)
    { None, false }
| ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }

(* This rule analyzes a single line and turns it into a stream of
   tokens. *)

and token = parse
(* keyworkds *)
| "module" { MODULE_KW }
| "import" { IMPORT_KW }
| "type" { TYPE_KW }
| "protocol" { PROTOCOL_KW }
| "global" { GLOBAL_KW }
| "local" { LOCAL_KW }
| "explicit" { EXPLICIT_KW }
| "aux" { AUX_KW }
| "role" { ROLE_KW }
| "accept" { ACCEPT_KW }
| "self" { SELF_KW }
| "sig" { SIG_KW }
| "as" { AS_KW }
| "connect" { CONNECT_KW }
| "disconnect" { DISCONNECT_KW }
| "wrap" { WRAP_KW }
| "from" { FROM_KW }
| "to" { TO_KW }
| "choice" { CHOICE_KW }
| "at" { AT_KW }
| "or" { OR_KW }
| "rec" { REC_KW }
| "continue" { CONTINUE_KW }
| "and" { AND_KW }
| "do" { DO_KW }

(* other *)
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOI }
| [^ '\n']* as str { IDENT str }
| _
    { raise (LexError (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
