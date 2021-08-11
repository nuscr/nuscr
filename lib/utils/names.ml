open! Name

module type TaggedName = sig
  include S

  val of_name : Name.t -> t
end

module Make () : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
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

(* TODO: Is it needed? *)
module ResultName : TaggedName = Make ()

module PackageName : TaggedName = Make ()

module ParameterName : TaggedName = Make ()

module RootDirName : TaggedName = Make ()

module EnumName : TaggedName = Make ()

module EnumTypeName : TaggedName = Make ()

module FunctionName : TaggedName = Make ()

module InterfaceName : TaggedName = Make ()
