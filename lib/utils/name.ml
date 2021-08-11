open! Base

module type S = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val of_string : string -> t

  val rename : t -> string -> t

  val user : t -> string

  val where : t -> Loc.t

  val create : string -> Loc.t -> t

  include Comparable.S with type t := t
end

module Name = struct
  module M = struct
    type t = string Loc.located [@@deriving show {with_path= false}, sexp_of]

    let of_string s = {Loc.value= s; loc= Loc.ghost_loc}

    let rename n s = {n with Loc.value= s}

    let user n = n.Loc.value

    let where n = n.Loc.loc

    let create value loc : t = {Loc.value; Loc.loc}

    let compare n n' = String.compare n.Loc.value n'.Loc.value
  end

  include M
  include Comparable.Make (M)
end
