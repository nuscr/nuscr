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

module StateName : TaggedName = Make ()

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

module StateNameSet = struct
  open! Base

  type t = Base.Set.M(StateName).t [@@deriving sexp_of]

  let compare = Base.Set.compare_direct

  let equal = Base.Set.equal

  let singleton = Set.singleton (module StateName)

  let of_list = Set.of_list (module StateName)

  let union_list = Set.union_list (module StateName)

  let singleton_of_string name =
    Set.singleton (module StateName) (StateName.create name Loc.ghost_loc)

  let fresh_singleton =
    let r = ref 0 in
    fun () ->
      let c = !r in
      r := c + 1 ;
      singleton_of_string ("state_" ^ Int.to_string c)

  let empty = Set.empty (module StateName)

  let user v =
    "{"
    ^ Base.String.concat ~sep:","
        (Base.List.map (Base.Set.to_list v) ~f:StateName.user)
    ^ "}"

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare

    let sexp_of_t = sexp_of_t
  end)
end
