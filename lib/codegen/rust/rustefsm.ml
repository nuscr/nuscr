open! Base
open Names
open Gtype
open Efsm
open Message
open Rustexpr

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

let stripped_payload_fields m =
  List.filter_map m.payload ~f:(function
    | PValue (Some v, ty) ->
        let stripped = strip_trailing_underscores (VariableName.user v) in
        Some (VariableName.of_string stripped, ty)
    | _ -> None )

let collect_labels_with_fields g =
  let f (_, a, _) acc =
    match a with
    | SendA (_, m, _) | RecvA (_, m, _) ->
        let label = upper_camel_case (LabelName.user m.label) in
        let vars = stripped_payload_fields m in
        let existing = Option.value ~default:[] (Map.find acc label) in
        let merged = List.fold ~f:append_var ~init:existing vars in
        Map.set acc ~key:label ~data:merged
    | Epsilon -> acc
  in
  G.fold_edges_e f g (Map.empty (module String))

type step_branch =
  {sb_m: message; sb_rannot: refinement_action_annot; sb_dst: state}

let group_step_arms g =
  G.fold_edges_e
    (fun (src, a, dst) acc ->
      match a with
      | SendA (_, m, rannot) | RecvA (_, m, rannot) ->
          let dir = match a with SendA _ -> "Send" | _ -> "Recv" in
          let label = upper_camel_case (LabelName.user m.label) in
          let key = Printf.sprintf "%d:%s:%s" src dir label in
          let _, _, _, merged_payload, branches =
            Option.value ~default:(src, dir, label, [], [])
              (Map.find acc key)
          in
          let merged_payload =
            List.fold ~f:append_var ~init:merged_payload
              (find_payload_vars m)
          in
          Map.set acc ~key
            ~data:
              ( src
              , dir
              , label
              , merged_payload
              , branches @ [{sb_m= m; sb_rannot= rannot; sb_dst= dst}] )
      | Epsilon -> acc )
    g
    (Map.empty (module String))

let collect_accepts_arms g =
  G.fold_edges_e
    (fun (_, a, _) acc ->
      match a with
      | SendA (_, m, _) | RecvA (_, m, _) ->
          let dir = match a with SendA _ -> "Send" | _ -> "Recv" in
          let label = upper_camel_case (LabelName.user m.label) in
          let key = dir ^ ":" ^ label in
          let payload, guards =
            Option.value ~default:([], []) (Map.find acc key)
          in
          let payload =
            List.fold ~f:append_var ~init:payload (stripped_payload_fields m)
          in
          let guards =
            match extract_message_guard m with
            | None -> guards
            | Some e -> guards @ [e]
          in
          Map.set acc ~key ~data:(payload, guards)
      | Epsilon -> acc )
    g
    (Map.empty (module String))
