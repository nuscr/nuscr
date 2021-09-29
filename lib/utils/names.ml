open! Base

module type UntaggedName = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val of_string : string -> t

  val rename : t -> string -> t

  val update : t -> f:(string -> string) -> t

  val user : t -> string

  val where : t -> Loc.t

  val create : string -> Loc.t -> t

  include Comparable.S with type t := t
end

module type TaggedName = sig
  include UntaggedName

  val of_other_name : (module UntaggedName with type t = 'a) -> 'a -> t
end

module UntaggedName : UntaggedName = struct
  open Loc

  module M = struct
    type t = string located [@@deriving show {with_path= false}, sexp_of]

    let of_string s = {value= s; loc= ghost_loc}

    let rename n s = {n with value= s}

    let update n ~f = {n with value= f n.value}

    let user n = n.value

    let where n = n.loc

    let create value loc : t = {value; loc}

    let compare n n' = String.compare n.value n'.value
  end

  include M
  include Comparable.Make (M)
end

module Make () : TaggedName = struct
  include UntaggedName

  let of_other_name (type a) (module Other : UntaggedName with type t = a)
      (name : a) : t =
    create (Other.user name) (Other.where name)
end

module ProtocolName : TaggedName = Make ()

module PayloadTypeName : TaggedName = Make ()

module LabelName : TaggedName = Make ()

module RoleName : TaggedName = Make ()

module VariableName : TaggedName = Make ()

module TypeVariableName : TaggedName = Make ()

module LocalProtocolName : TaggedName = Make ()
