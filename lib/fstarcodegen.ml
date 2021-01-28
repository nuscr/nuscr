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

module FstarNames = struct
  let send_state_callback_name st = Printf.sprintf "state%dSend" st

  let send_state_choice_name st = Printf.sprintf "state%dChoice" st

  let recv_state_callback_name st label =
    Printf.sprintf "state%dRecv%s" st (LabelName.user label)

  let state_record_name st = "state" ^ Int.to_string st

  let callback_record_name = "callbacks"

  let communication_record_name = "comms"

  let role_variant_name = "roles"

  let send_payload_fn_name p = "send_" ^ p

  let recv_payload_fn_name p = "recv_" ^ p

  let choice_ctor_name st label = "Choice" ^ Int.to_string st ^ label

  let run_state_fn_name st = "runState" ^ Int.to_string st

  let role_variant_ctor r = RoleName.user r |> String.capitalize
end

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
      let name = FstarNames.state_record_name st in
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
    | `Send _ ->
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
          Printf.sprintf "noeq type %s (st: %s) =\n"
            (FstarNames.send_state_choice_name st)
            (FstarNames.state_record_name st)
        in
        let def =
          String.concat ~sep:"\n"
            (List.map
               ~f:(fun (label, ty) ->
                 let vars_to_bind = Map.find_exn var_map st in
                 let ty = bind_variables ty vars_to_bind st in
                 Printf.sprintf "| %s of %s"
                   (FstarNames.choice_ctor_name st (LabelName.user label))
                   (Expr.show_payload_type ty))
               acc)
        in
        print_endline (preamble ^ def)
    | `Recv _ | `Terminal | `Mixed -> ()
  in
  G.iter_vertex generate_send_choice g

let generate_roles roles =
  let preamble = Printf.sprintf "type %s =\n" FstarNames.role_variant_name in
  let roles = Set.to_list roles in
  let def =
    String.concat ~sep:"\n"
      (List.map ~f:(fun r -> "| " ^ FstarNames.role_variant_ctor r) roles)
  in
  Stdio.print_endline (preamble ^ def)

let generate_transition_typedefs g var_map =
  let collect_transition st acc =
    match state_action_type g st with
    | `Send _ ->
        let new_entry =
          Printf.sprintf "%s: (st: %s) -> ML (%s st)"
            (FstarNames.send_state_callback_name st)
            (FstarNames.state_record_name st)
            (FstarNames.send_state_choice_name st)
        in
        new_entry :: acc
    | `Recv _ ->
        let collect_recv_transition (_, action, _) acc =
          match action with
          | RecvA (_, m, _) ->
              let label = m.Gtype.label in
              let concrete_vars = find_concrete_vars m in
              let recv_payload =
                match List.length concrete_vars with
                | 0 -> "unit"
                | 1 ->
                    let _, ty, _ = List.hd_exn concrete_vars in
                    let vars_to_bind = Map.find_exn var_map st in
                    let ty = bind_variables ty vars_to_bind st in
                    Expr.show_payload_type ty
                | _ -> Err.unimpl "handling more than 1 payload"
              in
              let new_entry =
                Printf.sprintf "%s: (st: %s) -> (%s) -> ML unit"
                  (FstarNames.recv_state_callback_name st label)
                  (FstarNames.state_record_name st)
                  recv_payload
              in
              new_entry :: acc
          | _ ->
              Err.violation
                "A receiving state should have only RecvA outgoing \
                 transitions"
        in
        let acc = G.fold_succ_e collect_recv_transition g st acc in
        acc
    | `Mixed -> Err.violation "Mixed states should not occur in the EFSM"
    | `Terminal -> acc
  in
  let transitions = G.fold_vertex collect_transition g [] in
  generate_record ~noeq:true FstarNames.callback_record_name
    (List.rev transitions)

let generate_comms payload_types =
  let payload_types = Set.to_list payload_types in
  let content =
    List.concat_map
      ~f:(fun ty ->
        let ty = PayloadTypeName.user ty in
        let send_ty =
          Printf.sprintf "%s: %s -> ML unit"
            (FstarNames.send_payload_fn_name ty)
            ty
        in
        let recv_ty =
          Printf.sprintf "%s: unit -> ML %s"
            (FstarNames.recv_payload_fn_name ty)
            ty
        in
        [send_ty; recv_ty])
      payload_types
  in
  generate_record ~noeq:true FstarNames.communication_record_name content

let generate_run_fns start g _var_map rec_var_info =
  let preamble =
    Printf.sprintf "let run (comms: %s -> %s) (callbacks: %s) : ML unit =\n"
      FstarNames.role_variant_name FstarNames.communication_record_name
      FstarNames.callback_record_name
  in
  let run_state_fns =
    let run_state_fn st acc =
      let preamble =
        Printf.sprintf "%s (st: %s): ML unit =\n"
          (FstarNames.run_state_fn_name st)
          (FstarNames.state_record_name st)
      in
      let body =
        match state_action_type g st with
        | `Terminal -> "()"
        | `Mixed -> Err.violation "An EFSM should not have a mixed state"
        | `Send r ->
            let connect_selection =
              Printf.sprintf "let conn = comms %s in"
                (FstarNames.role_variant_ctor r)
            in
            let match_head =
              Printf.sprintf "match callbacks.%s st with"
                (FstarNames.send_state_callback_name st)
            in
            let collect_match_hand (_, action, _next) acc =
              let entry =
                match action with
                | SendA (_, m, _) ->
                    let payload, base_ty =
                      match List.length m.Gtype.payload with
                      | 0 -> ("_unit", "unit")
                      | 1 -> (
                        match List.hd_exn m.Gtype.payload with
                        | Gtype.PValue (v, ty) ->
                            let payload =
                              Option.value ~default:"payload"
                                (Option.map ~f:VariableName.user v)
                            in
                            let ty =
                              Expr.payload_typename_of_payload_type ty
                              |> PayloadTypeName.user
                            in
                            (payload, ty)
                        | Gtype.PDelegate _ ->
                            Err.unimpl "delegation in code generation" )
                      | _ -> Err.unimpl "sending multiple payload items"
                    in
                    let match_pattern =
                      Printf.sprintf "| %s %s ->"
                        (FstarNames.choice_ctor_name st
                           (LabelName.user m.Gtype.label))
                        payload
                    in
                    let send_label =
                      Printf.sprintf "let () = conn.%s \"%s\" in"
                        (FstarNames.send_payload_fn_name "string")
                        (LabelName.user m.Gtype.label)
                    in
                    let send_payload =
                      Printf.sprintf "let () = conn.%s %s in"
                        (FstarNames.send_payload_fn_name base_ty)
                        payload
                    in
                    [match_pattern; send_label; send_payload; "assert false (* TODO *)"]
                | _ ->
                    Err.violation
                      "Sending state should only have outgoing send actions"
              in
              entry :: acc
            in
            let match_hands = G.fold_succ_e collect_match_hand g st [] in
            String.concat ~sep:"\n"
              ( connect_selection :: match_head
              :: List.concat (List.rev match_hands) )
        | `Recv r ->
            let connect_selection =
              Printf.sprintf "let conn = comms %s in"
                (FstarNames.role_variant_ctor r)
            in
            let match_head =
              Printf.sprintf "match conn.%s () with"
                (FstarNames.recv_payload_fn_name "string")
            in
            let collect_match_hand (_, action, _next) acc =
              let entry =
                match action with
                | RecvA (_, m, _) ->
                    Printf.sprintf "| \"%s\" -> assert false (* TODO *)"
                      (LabelName.user m.Gtype.label)
                | _ ->
                    Err.violation
                      "Receiving state should only have outgoing receive \
                       actions"
              in
              entry :: acc
            in
            let match_hands = G.fold_succ_e collect_match_hand g st [] in
            let catch_all = "| _ -> unexpected \"Unexpected label\"" in
            String.concat ~sep:"\n"
              ( connect_selection :: match_head
              :: List.rev (catch_all :: match_hands) )
      in
      (preamble ^ body) :: acc
    in
    List.rev (G.fold_vertex run_state_fn g [])
  in
  let run_fns =
    "let rec " ^ String.concat ~sep:"\nand " run_state_fns ^ "\nin\n"
  in
  let init_state_content =
    match Map.find rec_var_info start with
    | None -> "()"
    | Some rvs ->
        if List.is_empty rvs then "()"
        else
          let inner =
            String.concat ~sep:";\n"
              (List.map
                 ~f:
                   (fun ( is_silent
                        , {Gtype.rv_name; Gtype.rv_ty; Gtype.rv_init_expr; _}
                        ) ->
                   let rv_name = VariableName.user rv_name in
                   let value =
                     if is_silent then
                       Printf.sprintf "(assume false; hide %s)"
                         (Expr.show (Expr.default_value rv_ty))
                     else Expr.show rv_init_expr
                   in
                   Printf.sprintf "%s = %s" rv_name value)
                 rvs)
          in
          "{\n" ^ inner ^ "\n}\n"
  in
  let init_state =
    Printf.sprintf "let initState: %s =\n%s\nin"
      (FstarNames.state_record_name start)
      init_state_content
  in
  let run_init_state = FstarNames.run_state_fn_name start ^ " initState" in
  print_endline
    (String.concat ~sep:"\n" [preamble; run_fns; init_state; run_init_state])

let gen_code (start, g, rec_var_info) =
  let var_map = compute_var_map start g rec_var_info in
  let () = generate_state_defs var_map in
  let () = generate_send_choices g var_map in
  let () = generate_roles (find_all_roles g) in
  let () = generate_transition_typedefs g var_map in
  let () = generate_comms (find_all_payloads g) in
  let () = generate_run_fns start g var_map rec_var_info in
  assert false
