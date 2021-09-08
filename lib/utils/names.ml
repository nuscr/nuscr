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

module UntaggedName : S = struct
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

module type TaggedName = sig
  include S

  val of_untagged : UntaggedName.t -> t

  val of_other_name : (module S with type t = 'a) -> 'a -> t
end

module Make () : TaggedName = struct
  include UntaggedName

  let of_untagged (name : UntaggedName.t) = name

  let of_other_name (type a) (module Other : S with type t = a) (name : a) :
      t =
    create (Other.user name) (Other.where name)
end

module ProtocolName : TaggedName = Make ()

module PayloadTypeName : TaggedName = Make ()

module LabelName : TaggedName = Make ()

module RoleName : TaggedName = Make ()

module VariableName : TaggedName = Make ()

module TypeVariableName : TaggedName = Make ()

module LocalProtocolName : TaggedName = Make ()

module ChannelStructName : TaggedName = Make ()

module ChannelName : TaggedName = Make ()

module InviteChannelStructName : TaggedName = Make ()

module InviteChannelName : TaggedName = Make ()

module CallbackName : TaggedName = Make ()

module CallbacksEnvName : TaggedName = Make ()

module MessageStructName : TaggedName = Make ()

module FileName : TaggedName = Make ()

(* TODO: Is it needed? *)
module ResultName : TaggedName = Make ()

module PackageName : TaggedName = Make ()

module ParameterName : TaggedName = Make ()

module RootDirName : TaggedName = Make ()

module EnumName : TaggedName = Make ()

module EnumTypeName : TaggedName = Make ()

module FunctionName : TaggedName = Make ()

module InterfaceName : TaggedName = Make ()
