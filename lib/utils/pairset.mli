(** Set of pairs, compared using their memory address. Useful for
    constructing coinductive relations *)

open! Base

module type S = sig
  type t
end

type 'a t = ('a * 'a) Hash_set.t

val create : (module S with type t = 'a) -> 'a t
