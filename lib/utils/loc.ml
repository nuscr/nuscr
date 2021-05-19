open! Base
open Printf

type source_loc = Lexing.position * Lexing.position

let ghost_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

let sexp_of_source_loc _ = Sexp.Atom "<opaque>"

let show_position pos =
  sprintf "%d:%d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let show_source_loc (startp, endp) : string =
  sprintf "%s to %s in: %s" (show_position startp) (show_position endp)
    startp.Lexing.pos_fname

let build p = p

let equal_source_loc _ _ = true

let compare_source_loc _ _ = 0

type 'a located =
  { loc: source_loc
        [@printer
          fun fmt interval -> fprintf fmt "%s" (show_source_loc interval)]
  ; value: 'a }
[@@deriving show, eq, ord]

let sexp_of_located f v = f v.value
