open! Base
open Printf
open Lexing

type t = position * position

let ghost_loc = (dummy_pos, dummy_pos)

let sexp_of_t _ = Sexp.Atom "<opaque>"

let show_position pos =
  sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let show (startp, endp) : string =
  let filename =
    if String.is_empty startp.pos_fname then ""
    else " in: " ^ startp.pos_fname
  in
  sprintf "%s to %s%s" (show_position startp) (show_position endp) filename

let pp fmt loc = Caml.Format.fprintf fmt "%s" (show loc)

let create p = p

let equal _ _ = true

let compare _ _ = 0

type 'a located = {loc: t; value: 'a} [@@deriving show, eq, ord]

let sexp_of_located f v = f v.value

let merge (start, _) (_, finish) = (start, finish)
