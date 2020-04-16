open! Base
open Printf
open Ltype
open Names
open Graph

type action =
  | SendA of RoleName.t * Gtype.message
  | RecvA of RoleName.t * Gtype.message
  | Epsilon
[@@deriving ord]

let show_action = function
  | SendA (r, msg) ->
      sprintf "%s!%s" (RoleName.user r) (Gtype.show_message msg)
  | RecvA (r, msg) ->
      sprintf "%s?%s" (RoleName.user r) (Gtype.show_message msg)
  | Epsilon -> "ε"

module Label = struct
  type t = action

  let compare = compare_action

  let default = Epsilon
end

module G = Persistent.Digraph.ConcreteLabeled (Int) (Label)

type t = G.t

type state = int

module Display = struct
  include G

  let vertex_name = Int.to_string

  let graph_attributes _ = []

  let default_vertex_attributes _ = []

  let vertex_attributes _ = []

  let default_edge_attributes _ = []

  let edge_attributes (_, a, _) = [`Label (show_action a)]

  let get_subgraph _ = None
end

module DotOutput = Graphviz.Dot (Display)

type conv_env = {g: G.t; tyvars: (TypeVariableName.t * int) list}

let init_conv_env = {g= G.empty; tyvars= []}

(* Redirect all edges into st_remove to st_base, then remove st_remove *)
let merge_state g st_base st_remove =
  let f (src, action, _dst) g_ =
    let g_ = G.add_edge_e g_ (src, action, st_base) in
    g_
  in
  let g = G.fold_pred_e f g st_remove g in
  let g = G.remove_vertex g st_remove in
  g

(* Redirect all edges from st_remove to st_base, then remove st_remove *)
let merge_state_rev st_base g st_remove =
  let f (_src, action, dst) g_ =
    let g_ = G.add_edge_e g_ (st_base, action, dst) in
    g_
  in
  let g = G.fold_succ_e f g st_remove g in
  let g = G.remove_vertex g st_remove in
  g

let of_local_type lty =
  let count = ref 0 in
  let fresh () =
    let n = !count in
    count := n + 1 ;
    n
  in
  let rec conv_ltype_aux env =
    let {g; tyvars} = env in
    function
    | RecvL (m, n, l) ->
        let curr = fresh () in
        let env, next = conv_ltype_aux env l in
        let g = env.g in
        let g = G.add_vertex g curr in
        let e = (curr, RecvA (n, m), next) in
        let g = G.add_edge_e g e in
        ({env with g}, curr)
    | SendL (m, n, l) ->
        let curr = fresh () in
        let env, next = conv_ltype_aux env l in
        let e = (curr, SendA (n, m), next) in
        let g = env.g in
        let g = G.add_vertex g curr in
        let g = G.add_edge_e g e in
        ({env with g}, curr)
    | ChoiceL (_r, ltys) ->
        let curr = fresh () in
        let env, nexts = List.fold_map ~f:conv_ltype_aux ~init:env ltys in
        let g = env.g in
        let es = List.map ~f:(fun n -> (curr, Epsilon, n)) nexts in
        let g = G.add_vertex g curr in
        let g = List.fold ~f:G.add_edge_e ~init:g es in
        let g = List.fold ~f:(merge_state_rev curr) ~init:g nexts in
        ({env with g}, curr)
    | EndL ->
        let curr = fresh () in
        let g = G.add_vertex g curr in
        ({env with g}, curr)
    | MuL (tv, l) ->
        let new_st = fresh () in
        let g = G.add_vertex g new_st in
        let old_env = env in
        let env = {tyvars= (tv, new_st) :: tyvars; g} in
        let env, curr = conv_ltype_aux env l in
        let g = merge_state env.g curr new_st in
        ({old_env with g}, curr)
    | TVarL tv ->
        (env, List.Assoc.find_exn ~equal:TypeVariableName.equal env.tyvars tv)
    | _ ->
        (* TODO *)
        let open Err in
        unimpl "TODO"
  in
  let env, start = conv_ltype_aux init_conv_env lty in
  (start, env.g)

let show g =
  let buffer = Buffer.create 4196 in
  let formatter = Caml.Format.formatter_of_buffer buffer in
  DotOutput.fprint_graph formatter g ;
  Caml.Format.pp_print_flush formatter () ;
  Buffer.contents buffer
