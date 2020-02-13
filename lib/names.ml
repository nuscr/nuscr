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
