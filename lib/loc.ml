open! Base
open Printf

type source_loc = Lexing.position * Lexing.position

let ghost_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

let sexp_of_source_loc _ = Sexp.Atom "<opaque>"

let render_pos pos =
  sprintf "%d:%d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let render_pos_interval (startp, endp) : string =
  sprintf "%s to %s in: %s" (render_pos startp) (render_pos endp)
    startp.Lexing.pos_fname

let build p = p

let equal_source_loc _ _ = true

let compare_source_loc _ _ = 0

type 'a located =
  { loc: source_loc
        [@printer
          fun fmt interval -> fprintf fmt "%s" (render_pos_interval interval)]
  ; value: 'a [@main] }
[@@deriving show, sexp_of, eq, ord]
