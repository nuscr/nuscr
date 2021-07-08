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

type var_info_entry =
  | PayloadV of VariableName.t * Expr.payload_type
  | SilentV of VariableName.t * Expr.payload_type
  | RecursionV of VariableName.t * Expr.payload_type * Expr.t
  | RecursionVUpdate of VariableName.t * Expr.t

type var_info = var_info_entry list Map.M(Int).t

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

let show g =
  let buffer = Buffer.create 4196 in
  let formatter = Caml.Format.formatter_of_buffer buffer in
  DotOutput.fprint_graph formatter g ;
  Caml.Format.pp_print_flush formatter () ;
  Buffer.contents buffer

type conv_env =
  { g: G.t
  ; tyvars: (TypeVariableName.t * int) list
  ; states_to_merge: (int * int) list
  ; var_info: var_info
  ; silent_var_buffer: (VariableName.t * Expr.payload_type) list }

let init_conv_env =
  { g= G.empty
  ; tyvars= []
  ; states_to_merge= []
  ; var_info= Map.empty (module Int)
  ; silent_var_buffer= [] }

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

(* (** Construct the epsilon closure for a given NDA *)
 * let epsilon_closure g =
 *   let rec compute_closure visited state =
 *     let edges = G.succ_e g state in
 *     List.fold edges ~init:visited ~f:(fun visited -> function
 *       | _, Epsilon, dst when not (Set.mem visited dst) ->
 *           let visited = Set.add visited dst in
 *           compute_closure visited dst
 *       | _ -> visited)
 *   in
 *   G.fold_vertex
 *     (fun v closures ->
 *       Map.add_exn closures ~key:v
 *         ~data:(compute_closure (Set.singleton (module Int) v) v))
 *     g
 *     (Map.empty (module Int))
 *

 * module IntSet = struct
 *   module M = struct
 *     type t = Set.M(Int).t
 *
 *     let compare = Set.compare_direct
 *
 *     let sexp_of_t = Set.sexp_of_m__t (module Int)
 *   end
 *
 *   include M
 *   include Comparator.Make (M)
 * end
 *)

(* let powerset_construction (start, old_g) =
 *   let epsilons = epsilon_closure old_g in
 *   let count = ref 0 in
 *   let fresh () =
 *     let n = !count in
 *     count := n + 1 ;
 *     n
 *   in
 *   let rec aux (g, state_map) states =
 *     match Map.find state_map states with
 *     | Some _ -> (g, state_map)
 *     | None ->
 *         let st = fresh () in
 *         let g = G.add_vertex g st in
 *         let state_map = Map.add_exn ~key:states ~data:st state_map in
 *         let f acc old_node =
 *           let edges = G.succ_e old_g old_node in
 *           let f acc (_, lbl, dst) =
 *             match lbl with
 *             | Epsilon -> acc
 *             | _ ->
 *                 let dst = Map.find_exn epsilons dst in
 *                 let f = function
 *                   | None -> dst
 *                   | Some states -> Set.union states dst
 *                 in
 *                 Map.update acc lbl ~f
 *           in
 *           List.fold ~init:acc ~f edges
 *         in
 *         let out_edges =
 *           Set.fold ~init:(Map.empty (module Label)) ~f states
 *         in
 *         let f ~key:label ~data:states (g, state_map) =
 *           let g, state_map = aux (g, state_map) states in
 *           let g =
 *             G.add_edge_e g (st, label, Map.find_exn state_map states)
 *           in
 *           (g, state_map)
 *         in
 *         Map.fold out_edges ~init:(g, state_map) ~f
 *   in
 *   let start_states = Map.find_exn epsilons start in
 *   let g, state_map = aux (G.empty, Map.empty (module IntSet)) start_states in
 *   (Map.find_exn state_map start_states, g)
 *)

let merge_state ~from_state ~to_state g =
  let subst x = if x = from_state then to_state else x in
  let g =
    G.fold_succ_e
      (fun (ori, label, dest) g ->
        let ori = subst ori in
        let dest = subst dest in
        match label with
        | Epsilon -> g
        | label -> G.add_edge_e g (ori, label, dest))
      g from_state g
  in
  let g =
    G.fold_pred_e
      (fun (ori, label, dest) g ->
        let ori = subst ori in
        let dest = subst dest in
        match label with
        | Epsilon -> g
        | label -> G.add_edge_e g (ori, label, dest))
      g from_state g
  in
  let g = G.remove_vertex g from_state in
  g

let of_local_type_with_var_info lty =
  let count = ref 0 in
  let fresh () =
    let n = !count in
    count := n + 1 ;
    n
  in
  let rec conv_ltype_aux env =
    let {g; tyvars; _} = env in
    let process_silent_var_buffer env st =
      let {silent_var_buffer; var_info; _} = env in
      let silent_var_buffer = List.rev silent_var_buffer in
      let var_info =
        List.fold ~init:var_info
          ~f:(fun acc (var, ty) ->
            Map.add_multi acc ~key:st ~data:(SilentV (var, ty)))
          silent_var_buffer
      in
      {env with var_info; silent_var_buffer= []}
    in
    let process_payload_var env st payloads =
      let {var_info; _} = env in
      let var_info =
        List.fold ~init:var_info
          ~f:(fun acc payload ->
            match payload with
            | Gtype.PDelegate _ -> Err.unimpl "Delegation not supported"
            | Gtype.PValue (Some var, ty) ->
                Map.add_multi acc ~key:st ~data:(PayloadV (var, ty))
            | Gtype.PValue (None, _) -> acc)
          payloads
      in
      {env with var_info}
    in
    function
    | RecvL (m, n, l) ->
        let curr = fresh () in
        let env, next = conv_ltype_aux env l in
        let g = env.g in
        let g = G.add_vertex g curr in
        let e = (curr, RecvA (n, m), next) in
        let env = process_silent_var_buffer env next in
        let env = process_payload_var env next m.Gtype.payload in
        let g = G.add_edge_e g e in
        ({env with g}, curr)
    | SendL (m, n, l) ->
        let curr = fresh () in
        let env, next = conv_ltype_aux env l in
        let e = (curr, SendA (n, m), next) in
        let g = env.g in
        let g = G.add_vertex g curr in
        let g = G.add_edge_e g e in
        let env = process_silent_var_buffer env next in
        let env = process_payload_var env next m.Gtype.payload in
        ({env with g}, curr)
    | ChoiceL (_r, ltys) ->
        let curr = fresh () in
        let env, nexts = List.fold_map ~f:conv_ltype_aux ~init:env ltys in
        let g = env.g in
        let es = List.map ~f:(fun n -> (curr, Epsilon, n)) nexts in
        let g = G.add_vertex g curr in
        let g = List.fold ~f:G.add_edge_e ~init:g es in
        let states_to_merge =
          List.map ~f:(fun next -> (curr, next)) nexts @ env.states_to_merge
        in
        ({env with g; states_to_merge}, curr)
    | EndL ->
        let curr = fresh () in
        let g = G.add_vertex g curr in
        ({env with g}, curr)
    | MuL (tv, _, l) ->
        let new_st = fresh () in
        let g = G.add_vertex g new_st in
        let env = {env with tyvars= (tv, new_st) :: tyvars; g} in
        let env, curr = conv_ltype_aux env l in
        let g = env.g in
        let g = G.add_edge_e g (new_st, Epsilon, curr) in
        let states_to_merge = (new_st, curr) :: env.states_to_merge in
        ({env with g; states_to_merge}, curr)
    | TVarL (tv, _) ->
        (env, List.Assoc.find_exn ~equal:TypeVariableName.equal env.tyvars tv)
    | AcceptL _ | InviteCreateL _ ->
        Err.violation "Nested protocols are not supported in efsm"
    | SilentL (v, ty, l) ->
        let env =
          {env with silent_var_buffer= (v, ty) :: env.silent_var_buffer}
        in
        conv_ltype_aux env l
  in
  let env, start = conv_ltype_aux init_conv_env lty in
  let {g; var_info; _} = env in
  if not @@ List.is_empty env.states_to_merge then
    let rec aux (start, g, var_info) = function
      (* TODO: Handle var_info *)
      | [] -> (start, g, var_info)
      | (s1, s2) :: rest ->
          let to_state = Int.min s1 s2 in
          let from_state = Int.max s1 s2 in
          let subst x = if x = from_state then to_state else x in
          let g = merge_state ~from_state ~to_state g in
          let start = subst start in
          let rest =
            List.map
              ~f:(fun (x, y) ->
                let x = subst x in
                let y = subst y in
                (x, y))
              rest
          in
          aux (start, g, var_info) rest
    in
    aux (start, g, var_info) env.states_to_merge
  else (start, g, var_info)

let of_local_type ltype =
  let start, g, _ = of_local_type_with_var_info ltype in
  (start, g)
