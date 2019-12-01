type t [@@deriving show {with_path= false}, sexp_of]

val equal : t -> t -> bool

val of_string : string -> t

val rename : t -> string -> t

val user : t -> string

val where : t -> Loc.source_loc

val create : string -> Loc.source_loc -> t
