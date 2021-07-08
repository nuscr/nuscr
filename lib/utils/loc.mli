open! Base

type t

val ghost_loc : t

val sexp_of_t : 'a -> Sexp.t

val show : t -> string

val create : Lexing.position * Lexing.position -> t

type 'a located = {loc: t; value: 'a} [@@deriving show, sexp_of, eq, ord]
