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

module type TaggedName = sig
  include S

  val of_other_name : (module S with type t = 'a) -> 'a -> t
end

module ProtocolName : TaggedName

module PayloadTypeName : TaggedName

module LabelName : TaggedName

module RoleName : TaggedName

module VariableName : TaggedName

module TypeVariableName : TaggedName

module LocalProtocolName : TaggedName

module ChannelStructName : TaggedName

module ChannelName : TaggedName

module InviteChannelStructName : TaggedName

module InviteChannelName : TaggedName

module CallbackName : TaggedName

module CallbacksEnvName : TaggedName

module MessageStructName : TaggedName

module FileName : TaggedName

(* TODO: Is it needed? *)
module ResultName : TaggedName

module PackageName : TaggedName

module ParameterName : TaggedName

module RootDirName : TaggedName

module EnumName : TaggedName

module EnumTypeName : TaggedName

module FunctionName : TaggedName

module InterfaceName : TaggedName
