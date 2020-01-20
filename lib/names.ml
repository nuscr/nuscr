open Name

module type TaggedName = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val equal : t -> t -> bool

  val of_string : string -> t

  val rename : t -> string -> t

  val user : t -> string

  val where : t -> Loc.source_loc

  val create : string -> Loc.source_loc -> t

  val compare : t -> t -> int

  val of_name : Name_M.t -> t
end

module ProtocolName : TaggedName = struct
  include Name_M

  let of_name (name : Name_M.t) = name
end

module PayloadTypeName : TaggedName = struct
  include Name_M

  let of_name (name : Name_M.t) = name
end

module LabelName : TaggedName = struct
  include Name_M

  let of_name (name : Name_M.t) = name
end

module RoleName : TaggedName = struct
  include Name_M

  let of_name (name : Name_M.t) = name
end

module VariableName : TaggedName = struct
  include Name_M

  let of_name (name : Name_M.t) = name
end

module TypeVariableName : TaggedName = struct
  include Name_M

  let of_name (name : Name_M.t) = name
end
