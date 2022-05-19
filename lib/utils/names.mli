open! Base

(** Identifiers and Namespaces *)

(** An UntaggedName is a module type, used as a basis to create TaggedName. A
    name is used to store identifiers of various kinds, including a string
    that is the identifier itself, and a location of its occurence. *)
module type UntaggedName = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val of_string : string -> t
  (** Creates a name from a string, with a default location (unspecified) *)

  val rename : t -> string -> t
  (** Change the identifier, without changing the location *)

  val update : t -> f:(string -> string) -> t
  (** Update the identifier with function [~f], without changing the location *)

  val user : t -> string
  (** Prints the identifier in a user-friendly format, i.e. without the
      location *)

  val where : t -> Loc.t
  (** Gives the location of the identifier *)

  val create : string -> Loc.t -> t
  (** Creates a name from an identifier and a location *)

  val hash : t -> int
  (** Hash (identifier only, location ignored) *)

  include Comparable.S with type t := t
end

(** A TaggedName is a module type to represent names of different kinds. One
    can use [Make ()] to create a new namespace module, so that different
    namespaces can be properly distinguished at the type level *)
module type TaggedName = sig
  include UntaggedName

  val of_other_name : (module UntaggedName with type t = 'a) -> 'a -> t
  (** Allows conversion between different tags *)
end

(** Creates a new namespace module *)
module Make () : TaggedName

module ProtocolName : TaggedName

module PayloadTypeName : TaggedName

module LabelName : TaggedName

module RoleName : TaggedName

module VariableName : TaggedName

module TypeVariableName : TaggedName

module LocalProtocolName : TaggedName
