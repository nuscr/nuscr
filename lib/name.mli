open! Base

module type S = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val of_string : string -> t

  val rename : t -> string -> t

  val user : t -> string

  val where : t -> Loc.source_loc

  val create : string -> Loc.source_loc -> t

  include Comparable.S with type t := t
end

(* Name_M is used for all identifiers in the concrete syntax tree,
 * for names in the abstract syntax tree, use the tagged names in the Names
 * module *)
module Name : S
