module type TaggedName = sig
  include Name.S

  val of_name : Name.Name.t -> t
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

(* TODO: Is it needed? *)
module ResultName : TaggedName

module PackageName : TaggedName

module ParameterName : TaggedName

module RootDirName : TaggedName

module EnumName : TaggedName

module EnumTypeName : TaggedName

module FunctionName : TaggedName

module InterfaceName : TaggedName
