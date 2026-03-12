open! Base
open Gtype
open Names
open Message
open Efsm

let upper_camel_case s:string =
  Stdlib.String.capitalize_ascii @@ Stdlib.String.lowercase_ascii s

let find_payload_vars m =
  List.filter_map m.payload ~f:(function
    | PValue (Some v, ty) -> Some (v, ty)
    | _ -> None)

let append_var lst ((v, _) as entry) =
  if List.exists lst ~f:(fun (v_, _) -> VariableName.equal v v_) then lst
  else lst @ [entry]

let rm_silent_var rec_var_info =
  Map.map rec_var_info ~f:(fun data ->
    List.map data ~f:(fun (is_silent, rv) ->
      if is_silent then
        Err.unimpl ~here:[%here]
          (Printf.sprintf "Rust codegen for silent recursion variable '%s'"
            (VariableName.user rv.rv_name))
      else rv))

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
        G.fold_succ_e (fun (_, action, next_st) acc ->
          match action with
          | SendA (_, m, _) | RecvA (_, m, _) ->
              let payload_vars = find_payload_vars m in
              let vars = List.fold ~f:append_var ~init:vars payload_vars in
              aux acc (next_st, vars)
          | Epsilon ->
              Err.violation ~here:[%here]
                "Epsilon transitions should not appear in EFSM outputs"
        ) g curr_st acc
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
