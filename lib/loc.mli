type source_loc

val ghost_loc : source_loc

val sexp_of_source_loc : 'a -> Base.Sexp.t

val show_source_loc : source_loc -> Base.string

val build : Lexing.position * Lexing.position -> source_loc

type 'a located = {loc: source_loc; value: 'a}
[@@deriving show, sexp_of, eq, ord]
