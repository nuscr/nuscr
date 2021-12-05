open! Name

module type TaggedName = sig
  include S

  val of_name : Name.t -> t
end

module ProtocolName : TaggedName

module PayloadTypeName : TaggedName

module LabelName : TaggedName

module RoleName : TaggedName

module VariableName : TaggedName

module TypeVariableName : TaggedName

module StateName : TaggedName

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

module StateNameSet : sig
  type t = Base.Set.M(StateName).t

  val sexp_of_t : t -> Base.Sexp.t

  val compare : ('a, 'b) Base.Set.t -> ('a, 'b) Base.Set.t -> int

  val equal : ('a, 'b) Base.Set.t -> ('a, 'b) Base.Set.t -> bool

  val singleton_of_string :
    string -> (StateName.t, StateName.comparator_witness) Base.Set.t

  val of_list :
       StateName.t list
    -> (StateName.t, StateName.comparator_witness) Base.Set.t

  val empty : (StateName.t, StateName.comparator_witness) Base.Set.t

  val union_list :
       (StateName.t, StateName.comparator_witness) Base.Set.t list
    -> (StateName.t, StateName.comparator_witness) Base.Set.t

  val singleton :
    StateName.t -> (StateName.t, StateName.comparator_witness) Base.Set.t

  val fresh_singleton :
    unit -> (StateName.t, StateName.comparator_witness) Base.Set.t

  val user : (StateName.t, 'a) Base.Set.t -> string

  type comparator_witness

  val comparator : (t, comparator_witness) Base.Comparator.t
end
