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
