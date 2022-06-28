open! Base

module type S = sig
  type t
end

module HackyPair (M : S) = struct
  type t = M.t * M.t

  let address obj = 2 * Caml.Obj.magic obj

  let compare (x1, y1) (x2, y2) =
    let x1 = address x1 in
    let y1 = address y1 in
    let x2 = address x2 in
    let y2 = address y2 in
    [%derive.ord: int * int] (x1, y1) (x2, y2)

  let hash (x, y) = [%hash: int * int] (address x, address y)

  let sexp_of_t (x, y) =
    Sexp.List
      [ Sexp.Atom (Int.to_string (address x))
      ; Sexp.Atom (Int.to_string (address y)) ]
end

type 'a t = ('a * 'a) Hash_set.t

let create (type a) (module T : S with type t = a) : (a * a) Hash_set.t =
  Hash_set.create (module HackyPair (T))
