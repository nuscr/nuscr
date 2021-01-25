open! Base
open! Stdio
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

type var_entry = VariableName.t * Expr.payload_type * (* is_silent *) bool
[@@deriving eq]

let compute_var_map start g rec_var_info =
  let init = Map.empty (module Int) in
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
  let rec aux acc (curr_st, vars) =
    match Map.find acc curr_st with
    | Some _ -> acc
    | None ->
        let rec_vars =
          Option.value ~default:[] (Map.find rec_var_info curr_st)
        in
        let rec_vars =
          List.map
            ~f:(fun (is_silent, {Gtype.rv_name; Gtype.rv_ty; _}) ->
              (rv_name, rv_ty, is_silent))
            rec_vars
        in
        let vars = List.fold ~f:append ~init:vars rec_vars in
        let acc = Map.set acc ~key:curr_st ~data:vars in
        let next (_, action, next_st) acc =
          match action with
          | SendA (_, m, rannot) | RecvA (_, m, rannot) ->
              let silent_vars =
                List.map ~f:(fun (v, ty) -> (v, ty, true)) rannot.silent_vars
              in
              let find_named_variables =
                List.filter_map ~f:(function
                  | Gtype.PValue (Some v, ty) -> Some (v, ty, false)
                  | _ -> None)
              in
              let concrete_vars = find_named_variables m.Gtype.payload in
              let vars = List.fold ~f:append ~init:vars silent_vars in
              let vars = List.fold ~f:append ~init:vars concrete_vars in
              aux acc (next_st, vars)
          | Epsilon ->
              Err.violation
                "Epsilon transitions should not appear in EFSM outputs"
        in
        let acc = G.fold_succ_e next g curr_st acc in
        acc
  in
  let var_map = aux init (start, []) in
  if Config.verbose () then
    Map.iteri
      ~f:(fun ~key:st ~data ->
        print_endline
          (Printf.sprintf "State %d has variables: %s" st (show_vars data)))
      var_map ;
  var_map

let generate_state_defs var_maps =
  Map.iteri
    ~f:(fun ~key:st ~data ->
      let preamble = Printf.sprintf "type state%d =" st in
      let def =
        if List.is_empty data then " unit"
        else
          let content =
            String.concat ~sep:";\n"
              (List.map
                 ~f:(fun (name, ty, is_silent) ->
                   Printf.sprintf "%s: (%s%s)" (VariableName.user name)
                     (if is_silent then "erased" else "")
                     (Expr.show_payload_type ty))
                 data)
          in
          "\n{" ^ content ^ "}\n"
      in
      print_endline (preamble ^ def))
    var_maps

let gen_code (start, g, rec_var_info) =
  let var_maps = compute_var_map start g rec_var_info in
  let () = generate_state_defs var_maps in
  assert false
