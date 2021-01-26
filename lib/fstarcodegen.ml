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

let find_concrete_vars m =
  let find_named_variables =
    List.filter_map ~f:(function
      | Gtype.PValue (Some v, ty) -> Some (v, ty, false)
      | _ -> None)
  in
  let concrete_vars = find_named_variables m.Gtype.payload in
  concrete_vars

(** If any variable in [vars_to_bind] appears in [ty], bind them with a
    getter from state [st] *)
let bind_variables ty vars_to_bind st =
  match ty with
  | Expr.PTRefined (v, ty, e) ->
      let erased_vars_to_bind, concrete_vars_to_bind =
        List.partition_tf
          ~f:(fun (_, _, is_silent) -> is_silent)
          vars_to_bind
      in
      let bind_concrete_var var =
        Expr.Var
          (VariableName.of_string
             (Printf.sprintf "(Mkstate%d?.%s st)" st (VariableName.user var)))
      in
      let bind_erased_var var =
        Expr.Var
          (VariableName.of_string
             (Printf.sprintf "(reveal (Mkstate%d?.%s st))" st
                (VariableName.user var)))
      in
      let bind_var e vars_to_bind binder =
        let vars_to_bind =
          Set.of_list
            (module VariableName)
            (List.map ~f:(fun (v, _, _) -> v) vars_to_bind)
        in
        let vars_needed_binding = Set.inter vars_to_bind (Expr.free_var e) in
        let e =
          Set.fold ~init:e
            ~f:(fun e var ->
              Expr.substitute ~from:var ~replace:(binder var) e)
            vars_needed_binding
        in
        e
      in
      let e = bind_var e erased_vars_to_bind bind_erased_var in
      let e = bind_var e concrete_vars_to_bind bind_concrete_var in
      Expr.PTRefined (v, ty, e)
  | ty -> ty

let compute_var_map start g rec_var_info =
  let init = Map.empty (module Int) in
  let append lst ((v, _, _) as entry) =
    let rec aux acc = function
      | ((v_, _, _) as entry_) :: rest ->
          if VariableName.equal v v_ then
            if [%derive.eq: var_entry] entry entry_ then lst
            else Err.violationf "Clashing variable %s" (VariableName.user v)
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
              let concrete_vars = find_concrete_vars m in
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

let generate_record ~noeq name content =
  let noeq = if noeq then "noeq " else "" in
  let preamble = Printf.sprintf "%stype %s =\n" noeq name in
  let def =
    if List.is_empty content then "unit"
    else "{\n" ^ String.concat ~sep:";\n" content ^ "\n}\n"
  in
  print_endline (preamble ^ def)

let generate_state_defs var_maps =
  Map.iteri
    ~f:(fun ~key:st ~data ->
      let name = "state" ^ Int.to_string st in
      let content =
        List.map
          ~f:(fun (name, ty, is_silent) ->
            Printf.sprintf "%s: (%s%s)" (VariableName.user name)
              (if is_silent then "erased" else "")
              (Expr.show_payload_type ty))
          data
      in
      generate_record ~noeq:true name content)
    var_maps

let generate_send_choices g var_map =
  let generate_send_choice st =
    match state_action_type g st with
    | `Send ->
        let collect_action (_, action, _) acc =
          match action with
          | SendA (_, m, _) -> (
              let label = m.Gtype.label in
              let concrete_vars = find_concrete_vars m in
              match List.length concrete_vars with
              | 0 -> (label, Expr.PTUnit) :: acc
              | 1 ->
                  let _, ty, _ = List.hd_exn concrete_vars in
                  (label, ty) :: acc
              | _ -> Err.unimpl "handling more than 1 payload" )
          | _ ->
              Err.violation
                "A sending state should only have sending actions"
        in
        let acc = G.fold_succ_e collect_action g st [] in
        let preamble =
          Printf.sprintf "noeq type state%dChoice (st: state%d) =\n" st st
        in
        let def =
          String.concat ~sep:"\n"
            (List.map
               ~f:(fun (label, ty) ->
                 let vars_to_bind = Map.find_exn var_map st in
                 let ty = bind_variables ty vars_to_bind st in
                 Printf.sprintf "| Choice%d%s of %s" st
                   (LabelName.user label)
                   (Expr.show_payload_type ty))
               acc)
        in
        print_endline (preamble ^ def)
    | `Recv | `Terminal | `Mixed -> ()
  in
  G.iter_vertex generate_send_choice g

let generate_roles roles =
  let preamble = "type roles =\n" in
  let roles = Set.to_list roles in
  let def =
    String.concat ~sep:"\n"
      (List.map ~f:(fun r -> "| " ^ RoleName.user r) roles)
  in
  Stdio.print_endline (preamble ^ def)

let gen_code (start, g, rec_var_info) =
  let var_map = compute_var_map start g rec_var_info in
  let () = generate_state_defs var_map in
  let () = generate_send_choices g var_map in
  let () = generate_roles (find_all_roles g) in
  assert false
