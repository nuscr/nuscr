open! Base
open Names
open Gtype
open Efsm
open Message

let upper_camel_case s = Stdlib.String.capitalize_ascii s

let find_payload_vars m =
  List.filter_map m.payload ~f:(function
    | PValue (Some v, ty) -> Some (v, ty)
    | _ -> None )

let append_var lst ((v, _) as entry) =
  if List.exists lst ~f:(fun (v_, _) -> VariableName.equal v v_) then lst
  else lst @ [entry]

let rm_silent_var rec_var_info =
  Map.map rec_var_info ~f:(fun data ->
      List.map data ~f:(fun (is_silent, rv) ->
          if is_silent then
            Err.unimpl ~here:[%here]
              (Printf.sprintf
                 "Rust codegen for silent recursion variable '%s'"
                 (VariableName.user rv.rv_name) )
          else rv ) )

(* Walk the EFSM graph from [start], accumulating rec vars and named payload
   vars along each path. Each state is visited at most once: when choice
   branches merge into a single state, session-type merging guarantees
   identical variable scopes on every path, so the first visit suffices. *)
let compute_var_map start g rec_var_info =
  let rec aux acc (curr_st, vars) =
    match Map.find acc curr_st with
    | Some _ -> acc
    | None ->
        let rec_vars =
          Option.value ~default:[] (Map.find rec_var_info curr_st)
          |> List.map ~f:(fun rv -> (rv.rv_name, rv.rv_ty))
        in
        let vars = List.fold ~f:append_var ~init:vars rec_vars in
        let acc = Map.set acc ~key:curr_st ~data:vars in
        G.fold_succ_e
          (fun (_, action, next_st) acc ->
            match action with
            | SendA (_, m, _) | RecvA (_, m, _) ->
                let payload_vars = find_payload_vars m in
                let vars = List.fold ~f:append_var ~init:vars payload_vars in
                aux acc (next_st, vars)
            | Epsilon ->
                Err.violation ~here:[%here]
                  "Epsilon transitions should not appear in EFSM outputs" )
          g curr_st acc
  in
  aux (Map.empty (module Int)) (start, [])

let collect_labels g =
  let f (_, a, _) acc =
    match a with
    | SendA (_, m, _) | RecvA (_, m, _) ->
        Set.add acc (LabelName.user m.label)
    | Epsilon -> acc
  in
  G.fold_edges_e f g (Set.empty (module String))

(* ── unit tests ─────────────────────────────────────────────────── *)
open Syntax
let g = G.add_vertex G.empty 0

let make_rec_var_info key names =
  let data = List.map names ~f:(fun s ->
    { rv_name = VariableName.of_string s
    ; rv_roles = []
    ; rv_ty = Expr.PTInt
    ; rv_init_expr = Int 0 })
  in
  Map.set (Map.empty (module Int)) ~key ~data

(* refined = true only for var_string = Some "x" *)
let action ?(refined = false) var_string =
  let ty = match refined with
  | true -> Expr.PTRefined (
      VariableName.of_string "x",
      Expr.PTInt,
      Binop (Gt, Var (VariableName.of_string "x"), Int 0 )
    )
  | _ -> Expr.PTInt in
  let payload = Option.fold var_string ~init:[] ~f:(fun _ s ->
    [PValue (Some (VariableName.of_string s), ty)])
  in
  SendA (
    RoleName.of_string "A",
    { label = LabelName.of_string "L" ; payload },
    { silent_vars = [] ; rec_expr_updates = [] }
  )

let print_var_map var_map = 
  Map.iteri var_map ~f:(fun ~key:state ~data:vars ->
    let vars = List.map vars ~f:(fun (var,_) ->
      VariableName.user var) in
    let vars = String.concat ~sep:"," vars in
    Stdlib.Printf.printf "%d: [%s] " state vars)

let%expect_test "Empty graph" = 
  let start = 0 in
  let rec_var_info = Map.empty (module Int) in

  print_var_map @@ compute_var_map start g rec_var_info ;
  [@warning "-40"];
  [%expect {| 0: [] |}]

let%expect_test "Single send with payload" =
  let start = 0 in
  let g = G.add_vertex g 1 in
  let g = G.add_edge_e g @@ G.E.create 0 (action (Some "x")) 1 in
  let rec_var_info = Map.empty (module Int) in
  print_var_map @@ compute_var_map start g rec_var_info ;
  [@warning "-40"];
  [%expect {| 0: [] 1: [x] |}]


let%expect_test "Chain send with payload" =
  let start = 0 in
  let g = G.add_vertex g 1 in
  let g = G.add_vertex g 2 in
  let g = G.add_vertex g 3 in
  let g = G.add_edge_e g @@ G.E.create 0 (action (Some "x")) 1 in
  let g = G.add_edge_e g @@ G.E.create 1 (action (Some "y")) 2 in
  let g = G.add_edge_e g @@ G.E.create 2 (action (Some "z")) 3 in
  let rec_var_info = Map.empty (module Int) in
  print_var_map @@ compute_var_map start g rec_var_info ;
  [@warning "-40"];
  [%expect {| 0: [] 1: [x] 2: [x,y] 3: [x,y,z] |}]

let%expect_test "Single send with payload with refined type" =
  let start = 0 in
  let action = action (Some "x") ~refined:true in
  let g = G.add_vertex g 1 in
  let g = G.add_edge_e g @@ G.E.create 0 action 1 in
  let rec_var_info = Map.empty (module Int) in
  print_var_map @@ compute_var_map start g rec_var_info ;
  [@warning "-40"];
  [%expect {| 0: [] 1: [x] |}]

let%expect_test "Single send with rec var" =
  let start = 0 in
  let g = G.add_vertex g 1 in
  let g = G.add_edge_e g @@ G.E.create 0 (action None) 1 in
  let rec_var_info = make_rec_var_info 0 ["r"] in
  print_var_map @@ compute_var_map start g rec_var_info ;
  [@warning "-40"];
  [%expect {| 0: [r] 1: [r] |}]

let%expect_test "Single send with rec var + payload var" =
  let start = 0 in
  let g = G.add_vertex g 1 in
  let g = G.add_edge_e g @@ G.E.create 0 (action (Some "x")) 1 in
  let rec_var_info = make_rec_var_info 0 ["r"] in
  print_var_map @@ compute_var_map start g rec_var_info ;
  [@warning "-40"];
  [%expect {| 0: [r] 1: [r,x] |}]

(* The var_map does not contain "y" after the cycle, since 
   before the cycle it was undefined. *)
let%expect_test "Cycle with rec var + payload var" =
  let start = 0 in
  let g = G.add_vertex g 1 in
  let g = G.add_edge_e g @@ G.E.create 0 (action (Some "x")) 1 in
  let g = G.add_edge_e g @@ G.E.create 1 (action (Some "y")) 0 in
  let rec_var_info = make_rec_var_info 0 ["r"] in
  print_var_map @@ compute_var_map start g rec_var_info ;
  [@warning "-40"];
  [%expect {| 0: [r] 1: [r,x] |}]

let%expect_test "Branch with rec var + payload var" =
  let start = 0 in
  let g = G.add_vertex g 1 in
  let g = G.add_vertex g 2 in
  let g = G.add_edge_e g @@ G.E.create 0 (action (Some "x")) 1 in
  let g = G.add_edge_e g @@ G.E.create 0 (action (Some "y")) 2 in
  let rec_var_info = make_rec_var_info 2 ["r"] in
  print_var_map @@ compute_var_map start g rec_var_info ;
  [@warning "-40"];
  [%expect {| 0: [] 1: [x] 2: [y,r] |}]

let%expect_test "Unreachable state with rec var + payload var" =
  let start = 0 in
  let g = G.add_vertex g 1 in
  let g = G.add_vertex g 2 in
  let g = G.add_vertex g 3 in
  let g = G.add_edge_e g @@ G.E.create 0 (action (Some "x")) 1 in
  let g = G.add_edge_e g @@ G.E.create 2 (action (Some "y")) 3 in
  let rec_var_info = make_rec_var_info 1 ["r"] in
  print_var_map @@ compute_var_map start g rec_var_info ;
  [@warning "-40"];
  [%expect {| 0: [] 1: [x,r] |}]