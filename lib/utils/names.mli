open! Base

module type UntaggedName = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val of_string : string -> t

  val rename : t -> string -> t

  val update : t -> f:(string -> string) -> t

  val user : t -> string

  val where : t -> Loc.t

  val create : string -> Loc.t -> t

  val hash : t -> int

  include Comparable.S with type t := t
end

module type TaggedName = sig
  include UntaggedName

  val of_other_name : (module UntaggedName with type t = 'a) -> 'a -> t
end

module Make () : TaggedName

module ProtocolName : TaggedName

module PayloadTypeName : TaggedName

module LabelName : TaggedName

module RoleName : TaggedName

module VariableName : TaggedName

module TypeVariableName : TaggedName

module LocalProtocolName : TaggedName
