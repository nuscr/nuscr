module Make (N : Names.TaggedName) : sig
  type t

  val create : unit -> t

  val unique_name : t -> N.t -> t * N.t
  (** Create a unique name based on the provided name and the known names.
      Return the updated state and the unique name *)

  val curr_unique_name : t -> N.t -> N.t option
  (** Return the last unique name generated for the given name *)
end
