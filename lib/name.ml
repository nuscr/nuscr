open! Base
module L = Loc

module type S = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val of_string : string -> t

  val rename : t -> string -> t

  val user : t -> string

  val where : t -> Loc.source_loc

  val create : string -> Loc.source_loc -> t

  include Comparable.S with type t := t
end

module Name = struct
  module M = struct
    type t = string L.located [@@deriving show {with_path= false}, sexp_of]

    let of_string s = {L.value= s; loc= Loc.ghost_loc}

    let rename n s = {n with L.value= s}

    let user n = n.L.value

    let where n = n.L.loc

    let create value loc : t =
      let open Loc in
      {value; loc}

    let compare n n' = String.compare n.L.value n'.L.value
  end

  include M
  include Comparable.Make (M)
end
