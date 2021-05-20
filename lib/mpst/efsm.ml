open! Base
open Printf
open Ltype
open Names
open Graph

type refinement_action_annot =
  { silent_vars: (VariableName.t * Expr.payload_type) list
  ; rec_expr_updates: Expr.t list }
[@@deriving ord, sexp_of]

let show_refinement_actions_annot {silent_vars; rec_expr_updates} =
  let silent_vars =
    if List.is_empty silent_vars then ""
    else
      let svars_s =
        String.concat ~sep:", "
          (List.map
             ~f:(fun (v, t) ->
               sprintf "%s: %s" (VariableName.user v)
                 (Expr.show_payload_type t) )
             silent_vars )
      in
      "<" ^ svars_s ^ ">"
  in
  let rec_expr_updates =
    if List.is_empty rec_expr_updates then ""
    else
      let expr_s =
        String.concat ~sep:", " (List.map ~f:Expr.show rec_expr_updates)
      in
      "[" ^ expr_s ^ "]"
  in
  silent_vars ^ rec_expr_updates

type action =
  | SendA of RoleName.t * Gtype.message * refinement_action_annot
  | RecvA of RoleName.t * Gtype.message * refinement_action_annot
  | Epsilon
[@@deriving ord, sexp_of]

let show_action = function
  | (SendA (r, msg, rannot) | RecvA (r, msg, rannot)) as a ->
      let symb =
        match a with SendA _ -> "!" | RecvA _ -> "?" | _ -> assert false
      in
      sprintf "%s%s%s%s" (RoleName.user r) symb (Gtype.show_message msg)
        (show_refinement_actions_annot rannot)
  | Epsilon -> "ε"

type rec_var_info = (bool * Gtype.rec_var) list Map.M(Int).t

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
  ; rec_var_info: rec_var_info
  ; silent_var_buffer: (VariableName.t * Expr.payload_type) list }

let init_conv_env =
  { g= G.empty
  ; tyvars= []
  ; states_to_merge= []
  ; rec_var_info= Map.empty (module Int)
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
        | label -> G.add_edge_e g (ori, label, dest) )
      g from_state g
  in
  let g =
    G.fold_pred_e
      (fun (ori, label, dest) g ->
        let ori = subst ori in
        let dest = subst dest in
        match label with
        | Epsilon -> g
        | label -> G.add_edge_e g (ori, label, dest) )
      g from_state g
  in
  let g = G.remove_vertex g from_state in
  g

let of_local_type_with_rec_var_info lty =
  let count = ref 0 in
  let fresh () =
    let n = !count in
    count := n + 1 ;
    n
  in
  let make_refinement_annotation env next =
    if Pragma.refinement_type_enabled () then
      let silent_vars = env.silent_var_buffer in
      let rec_expr_updates =
        match next with TVarL (_, rec_exprs) -> rec_exprs | _ -> []
      in
      ({env with silent_var_buffer= []}, {silent_vars; rec_expr_updates})
    else (env, {silent_vars= []; rec_expr_updates= []})
  in
  let rec conv_ltype_aux env =
    let {g; tyvars; _} = env in
    function
    | (RecvL (m, n, l) | SendL (m, n, l)) as curr_ty ->
        let curr = fresh () in
        let env, rannot = make_refinement_annotation env l in
        let env, next = conv_ltype_aux env l in
        let g = env.g in
        let g = G.add_vertex g curr in
        let action =
          match curr_ty with
          | SendL _ -> SendA (n, m, rannot)
          | RecvL _ -> RecvA (n, m, rannot)
          | _ -> assert false
        in
        let e = (curr, action, next) in
        let g = G.add_edge_e g e in
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
    | MuL (tv, rec_vars, l) ->
        let new_st = fresh () in
        let g = G.add_vertex g new_st in
        let env =
          { env with
            tyvars= (tv, new_st) :: tyvars
          ; g
          ; rec_var_info= Map.set env.rec_var_info ~key:new_st ~data:rec_vars
          }
        in
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
  let {g; rec_var_info; _} = env in
  if not @@ List.is_empty env.states_to_merge then
    let rec aux (start, g, rec_var_info) = function
      | [] -> (start, g, rec_var_info)
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
                (x, y) )
              rest
          in
          let rec_var_info =
            match Map.find rec_var_info from_state with
            | None -> rec_var_info
            | Some rv ->
                Map.update
                  ~f:(function
                    | None -> rv
                    | Some _ ->
                        Err.unimpl
                          "Multiple recursions with variables in choices" )
                  rec_var_info to_state
          in
          aux (start, g, rec_var_info) rest
    in
    aux (start, g, rec_var_info) env.states_to_merge
  else (start, g, rec_var_info)

let of_local_type ltype =
  let start, g, _ = of_local_type_with_rec_var_info ltype in
  (start, g)

let state_action_type g st =
  let merge_state_action_type aty1 aty2 =
    match (aty1, aty2) with
    | `Terminal, aty2 -> aty2
    | aty1, `Terminal -> aty1
    | `Send r1, `Send r2 -> if RoleName.equal r1 r2 then `Send r1 else `Mixed
    | `Recv r1, `Recv r2 -> if RoleName.equal r1 r2 then `Recv r1 else `Mixed
    | `Send _, `Recv _ -> `Mixed
    | `Recv _, `Send _ -> `Mixed
    | aty1, `Mixed -> aty1
    | `Mixed, aty2 -> aty2
  in
  let f (_, a, _) acc =
    let aty =
      match a with
      | SendA (r, _, _) -> `Send r
      | RecvA (r, _, _) -> `Recv r
      | Epsilon ->
          Err.violation
            "Epsilon transitions should not appear after EFSM generation"
    in
    merge_state_action_type aty acc
  in
  G.fold_succ_e f g st `Terminal

let find_all_payloads g =
  let f (_, a, _) acc =
    match a with
    | SendA (_, msg, _) | RecvA (_, msg, _) -> (
        let {Gtype.payload; _} = msg in
        let payloads = List.map ~f:Gtype.typename_of_payload payload in
        match payloads with
        | [] -> Set.add acc (PayloadTypeName.of_string "unit")
        | _ -> List.fold ~f:Set.add ~init:acc payloads )
    | _ ->
        Err.violation
          "Epsilon transitions should not appear after EFSM generation"
  in
  G.fold_edges_e f g
    (Set.singleton
       (module PayloadTypeName)
       (PayloadTypeName.of_string "string") )

let find_all_roles g =
  let f (_, a, _) acc =
    match a with
    | SendA (r, _, _) | RecvA (r, _, _) -> Set.add acc r
    | _ ->
        Err.violation
          "Epsilon transitions should not appear after EFSM generation"
  in
  G.fold_edges_e f g (Set.empty (module RoleName))
