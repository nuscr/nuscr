open! Base
open Printf
open Ltype
open Names
open Graph

type action =
  | SendA of RoleName.t * Gtype.message
  | RecvA of RoleName.t * Gtype.message
  | Epsilon
[@@deriving ord, sexp_of]

let show_action = function
  | SendA (r, msg) ->
      sprintf "%s!%s" (RoleName.user r) (Gtype.show_message msg)
  | RecvA (r, msg) ->
      sprintf "%s?%s" (RoleName.user r) (Gtype.show_message msg)
  | Epsilon -> "ε"

module Label = struct
  module M = struct
    type t = action

    let compare = compare_action

    let sexp_of_t = sexp_of_action
  end

  include M
  include Comparator.Make (M)

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

type conv_env =
  {g: G.t; tyvars: (TypeVariableName.t * int) list; non_deterministic: bool}

let init_conv_env = {g= G.empty; tyvars= []; non_deterministic= false}

(* (* Redirect all edges into st_remove to st_base, then remove st_remove *)
 * let merge_state g st_base st_remove =
 *   let f (src, action, _dst) g_ =
 *     let g_ = G.add_edge_e g_ (src, action, st_base) in
 *     g_
 *   in
 *   let g = G.fold_pred_e f g st_remove g in
 *   let g = G.remove_vertex g st_remove in
 *   g
 *
 * (* Redirect all edges from st_remove to st_base, then remove st_remove *)
 * let merge_state_rev st_base g st_remove =
 *   let f (_src, action, dst) g_ =
 *     let g_ = G.add_edge_e g_ (st_base, action, dst) in
 *     g_
 *   in
 *   let g = G.fold_succ_e f g st_remove g in
 *   let g = G.remove_vertex g st_remove in
 *   g
 *)


(** Construct the epsilon closure for a given NDA *)
let epsilon_closure g =
  let one_shot =
    let f node acc =
      let f edge acc =
        match edge with _, Epsilon, dst -> Set.add acc dst | _ -> acc
      in
      let data = G.fold_succ_e f g node (Set.singleton (module Int) node) in
      Map.add_exn ~key:node ~data acc
    in
    G.fold_vertex f g (Map.empty (module Int))
  in
  let iterate_once curr =
    let updated = ref false in
    let f ~key:_ ~data =
      let f acc node =
        let nexts = Map.find_exn curr node in
        let f acc new_node =
          if Set.mem acc new_node then acc
          else (
            updated := true ;
            Set.add acc new_node )
        in
        Set.fold ~init:acc ~f nexts
      in
      Set.fold ~init:data ~f data
    in
    (Map.mapi ~f curr, not !updated)
  in
  let compute_fixpoint () =
    let rec aux curr =
      let curr, complete = iterate_once curr in
      if complete then curr else aux curr
    in
    aux one_shot
  in
  compute_fixpoint ()

module IntSet = struct
  module M = struct
    type t = Set.M(Int).t

    let compare = Set.compare_direct

    let sexp_of_t = Set.sexp_of_m__t (module Int)
  end

  include M
  include Comparator.Make (M)
end

let powerset_construction (start, old_g) =
  let epsilons = epsilon_closure old_g in
  let count = ref 0 in
  let fresh () =
    let n = !count in
    count := n + 1 ;
    n
  in
  let rec aux (g, state_map) states =
    match Map.find state_map states with
    | Some _ -> (g, state_map)
    | None ->
        let st = fresh () in
        let g = G.add_vertex g st in
        let state_map = Map.add_exn ~key:states ~data:st state_map in
        let f acc old_node =
          let edges = G.succ_e old_g old_node in
          let f acc (_, lbl, dst) =
            match lbl with
            | Epsilon -> acc
            | _ ->
                let dst = Map.find_exn epsilons dst in
                let f = function
                  | None -> dst
                  | Some states -> Set.union states dst
                in
                Map.update acc lbl ~f
          in
          List.fold ~init:acc ~f edges
        in
        let out_edges =
          Set.fold ~init:(Map.empty (module Label)) ~f states
        in
        let f ~key:label ~data:states (g, state_map) =
          let g, state_map = aux (g, state_map) states in
          let g =
            G.add_edge_e g (st, label, Map.find_exn state_map states)
          in
          (g, state_map)
        in
        Map.fold out_edges ~init:(g, state_map) ~f
  in
  let start_states = Map.find_exn epsilons start in
  let g, state_map = aux (G.empty, Map.empty (module IntSet)) start_states in
  (Map.find_exn state_map start_states, g)

let of_local_type lty =
  let count = ref 0 in
  let fresh () =
    let n = !count in
    count := n + 1 ;
    n
  in
  let rec conv_ltype_aux env =
    let {g; tyvars; _} = env in
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
        ({env with g; non_deterministic= true}, curr)
    | EndL ->
        let curr = fresh () in
        let g = G.add_vertex g curr in
        ({env with g}, curr)
    | MuL (tv, l) ->
        let new_st = fresh () in
        let g = G.add_vertex g new_st in
        let env =
          {tyvars= (tv, new_st) :: tyvars; g; non_deterministic= true}
        in
        let env, curr = conv_ltype_aux env l in
        let g = env.g in
        let g = G.add_edge_e g (new_st, Epsilon, curr) in
        ({env with g}, curr)
    | TVarL tv ->
        (env, List.Assoc.find_exn ~equal:TypeVariableName.equal env.tyvars tv)
    | AcceptL _ | InviteCreateL _ ->
        raise (Err.Violation "Nested protocols are not supported in efsm")
  in
  let env, start = conv_ltype_aux init_conv_env lty in
  let g = env.g in
  if env.non_deterministic then powerset_construction (start, g)
  else (start, g)

let show g =
  let buffer = Buffer.create 4196 in
  let formatter = Caml.Format.formatter_of_buffer buffer in
  DotOutput.fprint_graph formatter g ;
  Caml.Format.pp_print_flush formatter () ;
  Buffer.contents buffer
