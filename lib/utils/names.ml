open! Name

module type TaggedName = sig
  include S

  val of_name : Name.t -> t
end

module ProtocolName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module PayloadTypeName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module LabelName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module RoleName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module VariableName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module TypeVariableName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module LocalProtocolName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module ChannelStructName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module ChannelName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module InviteChannelStructName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module InviteChannelName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module CallbackName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module CallbacksEnvName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module MessageStructName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

(* TODO: Is it needed? *)
module ResultName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module PackageName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module ParameterName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module RootDirName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module EnumName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module EnumTypeName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module FunctionName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end

module InterfaceName : TaggedName = struct
  include Name

  let of_name (name : Name.t) = name
end
