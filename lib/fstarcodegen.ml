open! Base
open Names
open Efsm

let show_vars = function
  | [] -> "(empty)"
  | vars ->
      let show (v, ty, is_silent) =
        let is_silent = if is_silent then "(silent)" else "" in
        let v = VariableName.user v in
        let ty = Expr.show_payload_type ty in
        is_silent ^ v ^ ": " ^ ty
      in
      let vars = List.map ~f:show vars in
      let vars = String.concat ~sep:"; " vars in
      "{" ^ vars ^ "}"

let compute_var_map g rec_var_info =
  let module VariableAnalysis = struct
    type vertex = G.E.vertex

    type edge = G.E.t

    type g = G.t

    type var_entry =
      VariableName.t * Expr.payload_type * (* is_silent *) bool
    [@@deriving eq]

    type data = var_entry list

    let direction = Graph.Fixpoint.Forward

    let equal = List.equal [%derive.eq: var_entry]

    let join vars1 vars2 =
      let rec aux acc vars1 vars2 =
        match (List.hd vars1, List.hd vars2) with
        | Some entry1, Some entry2 when [%derive.eq: var_entry] entry1 entry2
          ->
            aux (entry1 :: acc) (List.tl_exn vars1) (List.tl_exn vars2)
        | _ -> List.rev acc
      in
      aux [] vars1 vars2

    let analyze (_from, action, to_state) vars =
      match action with
      | SendA (_, m, rannot) | RecvA (_, m, rannot) ->
          let append lst ((v, _, _) as entry) =
            let rec aux acc = function
              | ((v_, _, _) as entry_) :: rest ->
                  if VariableName.equal v v_ then
                    if [%derive.eq: var_entry] entry entry_ then lst
                    else
                      raise
                      @@ Err.Violation
                           (Printf.sprintf "Clashing varaible %s"
                              (VariableName.user v))
                  else aux (entry_ :: acc) rest
              | [] -> List.rev (entry :: acc)
            in
            aux [] lst
          in
          let silent_vars =
            List.map ~f:(fun (v, ty) -> (v, ty, true)) rannot.silent_vars
          in
          let find_named_variables =
            List.filter_map ~f:(function
              | Gtype.PValue (Some v, ty) -> Some (v, ty, false)
              | _ -> None)
          in
          let rec_vars =
            Option.value ~default:[] (Map.find rec_var_info to_state)
          in
          let rec_vars =
            List.map
              ~f:(fun (is_silent, {Gtype.rv_name; Gtype.rv_ty; _}) ->
                (rv_name, rv_ty, is_silent))
              rec_vars
          in
          let concrete_vars = find_named_variables m.Gtype.payload in
          let vars = List.fold ~f:append ~init:vars silent_vars in
          let vars = List.fold ~f:append ~init:vars rec_vars in
          let vars = List.fold ~f:append ~init:vars concrete_vars in
          vars
      | Epsilon ->
          raise
            (Err.Violation
               "Epsilon transitions should not appear in EFSM outputs")
  end in
  let module Variables = Graph.Fixpoint.Make (G) (VariableAnalysis) in
  let compute_vars = Variables.analyze (fun _ -> []) g in
  G.fold_vertex
    (fun st acc ->
      let data = compute_vars st in
      if Config.verbose () then
        Stdio.print_endline
          (Printf.sprintf "State %d has variables: %s" st (show_vars data)) ;
      Map.add_exn acc ~key:st ~data)
    g
    (Map.empty (module Int))

let gen_code (_start, g, rec_var_info) =
  let _var_maps = compute_var_map g rec_var_info in
  assert false
