open! Base
module L = Loc

module type Name_S = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val equal : t -> t -> bool

  val of_string : string -> t

  val rename : t -> string -> t

  val user : t -> string

  val where : t -> Loc.source_loc

  val create : string -> Loc.source_loc -> t

  val compare : t -> t -> int
end

module Name_M = struct
  type t = string L.located [@@deriving show {with_path= false}, sexp_of]

  let equal n n' = String.equal n.L.value n'.L.value

  let of_string s = {L.value= s; loc= Loc.ghost_loc}

  let rename n s = {n with L.value= s}

  let user n = n.L.value

  let where n = n.L.loc

  let create value loc : t =
    let open Loc in
    {value; loc}

  let compare n n' = String.compare n.L.value n'.L.value
end
