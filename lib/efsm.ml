open! Core_kernel
open Syntax
open Ltype
open Graph

type action = SendA of name * message | RecvA of name * message | Epsilon

let show_action = function
  | SendA (r, msg) -> sprintf "%s!%s" r (show_message msg)
  | RecvA (r, msg) -> sprintf "%s?%s" r (show_message msg)
  | Epsilon -> "ε"

module Label = struct
  type t = action

  let compare = compare

  let default = Epsilon
end

module G = Imperative.Digraph.ConcreteLabeled (Int) (Label)

module Display = struct
  include G

  let vertex_name = string_of_int

  let graph_attributes _ = []

  let default_vertex_attributes _ = []

  let vertex_attributes _ = []

  let default_edge_attributes _ = []

  let edge_attributes (_, a, _) = [`Label (show_action a)]

  let get_subgraph _ = None
end

module DotOutput = Graphviz.Dot (Display)

let is_internal_choice = function
  | ChoiceL (_, ltys) -> (
      let lty = List.hd_exn ltys in
      match lty with
      | SendL (_, _, _) -> true
      | RecvL (_, _, _) -> false
      | _ -> failwith "Impossible" )
  | _ -> failwith "Impossible"

let conv_ltype lty =
  let count = ref 0 in
  let fresh () =
    let n = !count in
    count := n + 1 ;
    n
  in
  let g = G.create () in
  let rec conv_ltype_aux tyvars = function
    | RecvL (m, n, l) ->
        let curr = fresh () in
        let next = conv_ltype_aux tyvars l in
        let e = (curr, RecvA (n, m), next) in
        G.add_edge_e g e ; curr
    | SendL (m, n, l) ->
        let curr = fresh () in
        let next = conv_ltype_aux tyvars l in
        let e = (curr, SendA (n, m), next) in
        G.add_edge_e g e ; curr
    | ChoiceL (_r, ltys) ->
        let curr = fresh () in
        let nexts = List.map ~f:(conv_ltype_aux tyvars) ltys in
        let es = List.map ~f:(fun n -> (curr, Epsilon, n)) nexts in
        List.iter ~f:(G.add_edge_e g) es ;
        curr
    | EndL -> fresh ()
    | MuL (tv, l) ->
        let n = fresh () in
        let start = conv_ltype_aux ((tv, n) :: tyvars) l in
        let e = (n, Epsilon, start) in
        G.add_edge_e g e ; start
    | TVarL tv -> List.Assoc.find_exn ~equal:String.equal tyvars tv
  in
  let start = conv_ltype_aux [] lty in
  (start, g)

let show_efsm g =
  DotOutput.fprint_graph Format.str_formatter g ;
  Format.flush_str_formatter ()
