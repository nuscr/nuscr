open! Base
module L = Loc

type t = string L.located [@@deriving show {with_path= false}, sexp_of]

let equal n n' = String.equal n.L.value n'.L.value

let of_string s = {L.value= s; loc= Loc.ghost_loc}

let rename n s = {n with L.value= s}

let user n = n.L.value

let where n = n.L.loc

let create value loc : t =
  let open Loc in
  {value; loc}

let compare n n' = String.compare n.L.value n'.L.value
