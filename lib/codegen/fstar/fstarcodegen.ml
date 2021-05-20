open! Base
open! Stdio
open Mpst
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
      | _ -> None )
  in
  let concrete_vars = find_named_variables m.Gtype.payload in
  concrete_vars

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
              (rv_name, rv_ty, is_silent) )
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
  if Pragma.verbose () then
    Map.iteri
      ~f:(fun ~key:st ~data ->
        print_endline
          (Printf.sprintf "State %d has variables: %s" st (show_vars data))
        )
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

  let record_getter st var =
    Printf.sprintf "(Mk%s?.%s st)" (state_record_name st)
      (VariableName.user var)

  let dum_state_field_name st = Printf.sprintf "_dumState%d" st
end

(** If any variable in [vars_to_bind] appears in [e], bind them with a getter
    from state [st] *)
let bind_variables_e e vars_to_bind st =
  let erased_vars_to_bind, concrete_vars_to_bind =
    List.partition_tf ~f:(fun (_, _, is_silent) -> is_silent) vars_to_bind
  in
  let bind_concrete_var var =
    Expr.Var (VariableName.of_string (FstarNames.record_getter st var))
  in
  let bind_erased_var var =
    Expr.Var
      (VariableName.of_string
         (Printf.sprintf "(reveal %s)" (FstarNames.record_getter st var)) )
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
        ~f:(fun e var -> Expr.substitute ~from:var ~replace:(binder var) e)
        vars_needed_binding
    in
    e
  in
  let e = bind_var e erased_vars_to_bind bind_erased_var in
  let e = bind_var e concrete_vars_to_bind bind_concrete_var in
  e

(** If any variable in [vars_to_bind] appears in [ty], bind them with a
    getter from state [st] *)
let bind_variables_ty ty vars_to_bind st =
  match ty with
  | Expr.PTRefined (v, ty, e) ->
      Expr.PTRefined (v, ty, bind_variables_e e vars_to_bind st)
  | ty -> ty

let generate_record_type buffer ~noeq name content =
  let noeq = if noeq then "noeq " else "" in
  let preamble = Printf.sprintf "%stype %s =\n" noeq name in
  let def =
    if List.is_empty content then "unit"
    else "{\n" ^ String.concat ~sep:";\n" content ^ "\n}\n"
  in
  Buffer.add_string buffer (preamble ^ def ^ "\n")

let generate_record_value contents =
  let record =
    if List.is_empty contents then "()"
    else "{\n" ^ String.concat ~sep:";\n" contents ^ "\n}\n"
  in
  record

let generate_state_defs buffer var_maps =
  Map.iteri
    ~f:(fun ~key:st ~data ->
      let name = FstarNames.state_record_name st in
      let reveal_silent_vars silent_vars = function
        | Expr.PTRefined (v, ty, e) ->
            let free_vars = Expr.free_var e in
            let silent_vars =
              Set.of_list (module VariableName) silent_vars
            in
            let vars_to_reveal = Set.inter free_vars silent_vars in
            let e =
              Set.fold ~init:e
                ~f:(fun e v ->
                  Expr.substitute ~from:v
                    ~replace:
                      (Expr.Var
                         (VariableName.of_string
                            (Printf.sprintf "(reveal %s)"
                               (VariableName.user v) ) ) )
                    e )
                vars_to_reveal
            in
            Expr.PTRefined (v, ty, e)
        | ty -> ty
      in
      let content, _ =
        List.fold
          ~init:
            ( [Printf.sprintf "%s: unit" (FstarNames.dum_state_field_name st)]
            , [] )
          ~f:(fun (acc, silent_vars) (name, ty, is_silent) ->
            let ty = reveal_silent_vars silent_vars ty in
            let entry =
              Printf.sprintf "%s: (%s%s)" (VariableName.user name)
                (if is_silent then "erased " else "")
                (Expr.show_payload_type ty)
            in
            ( entry :: acc
            , if is_silent then name :: silent_vars else silent_vars ) )
          data
      in
      generate_record_type buffer ~noeq:true name (List.rev content) )
    var_maps

let generate_send_choices buffer g var_map =
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
                 let ty = bind_variables_ty ty vars_to_bind st in
                 Printf.sprintf "| %s of %s"
                   (FstarNames.choice_ctor_name st (LabelName.user label))
                   (Expr.show_payload_type ty) )
               acc )
        in
        Buffer.add_string buffer (preamble ^ def ^ "\n")
    | `Recv _ | `Terminal | `Mixed -> ()
  in
  G.iter_vertex generate_send_choice g

let generate_roles buffer roles =
  let preamble = Printf.sprintf "type %s =\n" FstarNames.role_variant_name in
  let roles = Set.to_list roles in
  let def =
    String.concat ~sep:"\n"
      (List.map ~f:(fun r -> "| " ^ FstarNames.role_variant_ctor r) roles)
  in
  Buffer.add_string buffer (preamble ^ def ^ "\n")

let generate_transition_typedefs buffer g var_map =
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
                    let ty = bind_variables_ty ty vars_to_bind st in
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
  generate_record_type buffer ~noeq:true FstarNames.callback_record_name
    (List.rev transitions)

let generate_comms buffer payload_types =
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
        [send_ty; recv_ty] )
      payload_types
  in
  generate_record_type buffer ~noeq:true FstarNames.communication_record_name
    content

let construct_next_state var_map ~curr ~next action rec_var_info =
  let curr_vars = Map.find_exn var_map curr in
  let next_vars = Map.find_exn var_map next in
  let next_rv_info = Option.value ~default:[] (Map.find rec_var_info next) in
  let new_silent_vars, rec_var_updates =
    match action with
    | SendA (_, _, rannot) | RecvA (_, _, rannot) ->
        let silent_vars = rannot.silent_vars in
        let rec_expr_updates = rannot.rec_expr_updates in
        let _, rec_var_updates =
          List.fold ~init:(rec_expr_updates, [])
            ~f:(fun (updates, acc) (is_silent, {Gtype.rv_name; _}) ->
              if is_silent then (updates, acc)
              else
                match updates with
                | [] -> ([], acc)
                | e :: rest -> (rest, (rv_name, e) :: acc) )
            next_rv_info
        in
        (silent_vars, List.rev rec_var_updates)
    | Epsilon ->
        Err.violation
          "Epsilon transition should not appear after EFSM generation"
  in
  let content =
    List.fold
      ~init:[Printf.sprintf "%s= ()" (FstarNames.dum_state_field_name next)]
      ~f:(fun acc (v, _, _) ->
        let entry =
          let header = VariableName.user v ^ "= " in
          let value =
            match
              List.find
                ~f:(fun (v_, _) -> VariableName.equal v v_)
                new_silent_vars
            with
            | Some (_, ty) ->
                (* We have a new silent variable *)
                Printf.sprintf "(assume false; hide %s)"
                  (Expr.default_value ty |> Expr.show)
            | None -> (
              match
                List.find
                  ~f:(fun (v_, _) -> VariableName.equal v v_)
                  rec_var_updates
              with
              | Some (_, e) ->
                  (* We have a recursion variable to be updated *)
                  let e = bind_variables_e e curr_vars curr in
                  Expr.show e
              | None -> (
                match
                  List.find
                    ~f:(fun (_, {Gtype.rv_name; _}) ->
                      VariableName.equal v rv_name )
                    next_rv_info
                with
                | Some (is_silent, {Gtype.rv_init_expr; Gtype.rv_ty; _}) ->
                    (* We have a recursion variable to be initialised *)
                    if is_silent then
                      Printf.sprintf "(assume false; hide %s)"
                        (Expr.default_value rv_ty |> Expr.show)
                    else
                      let e = bind_variables_e rv_init_expr curr_vars curr in
                      Expr.show e
                | None ->
                    (* We have a variable that is either new, or can be
                       retrieved from the existing state *)
                    let e = bind_variables_e (Expr.Var v) curr_vars curr in
                    Expr.show e ) )
          in
          header ^ value
        in
        entry :: acc )
      next_vars
  in
  generate_record_value (List.rev content)

let generate_run_fns buffer start g var_map rec_var_info =
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
      let find_payload m =
        match List.length m.Gtype.payload with
        | 0 -> ("_unit", "unit", None)
        | 1 -> (
          match List.hd_exn m.Gtype.payload with
          | Gtype.PValue (v, ty) ->
              let payload =
                Option.value ~default:"payload"
                  (Option.map ~f:VariableName.user v)
              in
              let curr_vars = Map.find_exn var_map st in
              let e =
                match ty with
                | Expr.PTRefined (_, _, e) ->
                    let e = bind_variables_e e curr_vars st in
                    Some e
                | _ -> None
              in
              let ty =
                Expr.payload_typename_of_payload_type ty
                |> PayloadTypeName.user
              in
              (payload, ty, e)
          | Gtype.PDelegate _ -> Err.unimpl "delegation in code generation" )
        | _ -> Err.unimpl "sending multiple payload items"
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
            let collect_match_hand (_, action, next) acc =
              let entry =
                match action with
                | SendA (_, m, _) ->
                    let payload, base_ty, _ = find_payload m in
                    let match_pattern =
                      Printf.sprintf "| %s %s ->"
                        (FstarNames.choice_ctor_name st
                           (LabelName.user m.Gtype.label) )
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
                    let next_state = "let nextState =" in
                    let run_next_state =
                      Printf.sprintf "%s nextState"
                        (FstarNames.run_state_fn_name next)
                    in
                    [ match_pattern
                    ; send_label
                    ; send_payload
                    ; next_state
                    ; construct_next_state var_map ~curr:st ~next action
                        rec_var_info
                    ; "in"
                    ; run_next_state ]
                | _ ->
                    Err.violation
                      "Sending state should only have outgoing send actions"
              in
              entry :: acc
            in
            let match_hands = G.fold_succ_e collect_match_hand g st [] in
            String.concat ~sep:"\n"
              (connect_selection
               :: match_head :: List.concat (List.rev match_hands) )
        | `Recv r ->
            let connect_selection =
              Printf.sprintf "let conn = comms %s in"
                (FstarNames.role_variant_ctor r)
            in
            let match_head =
              Printf.sprintf "match conn.%s () with"
                (FstarNames.recv_payload_fn_name "string")
            in
            let collect_match_hand (_, action, next) acc =
              let entry =
                match action with
                | RecvA (_, m, _) ->
                    let payload, base_ty, e = find_payload m in
                    let match_pattern =
                      Printf.sprintf "| \"%s\" ->"
                        (LabelName.user m.Gtype.label)
                    in
                    let recv_payload =
                      Printf.sprintf "let %s = conn.%s () in" payload
                        (FstarNames.recv_payload_fn_name base_ty)
                    in
                    let recv_payload =
                      match e with
                      | Some e ->
                          Printf.sprintf "%s\nassume (%s);" recv_payload
                            (Expr.show e)
                      | None -> recv_payload
                    in
                    let recv_callback =
                      Printf.sprintf "let () = callbacks.%s st %s in"
                        (FstarNames.recv_state_callback_name st m.Gtype.label)
                        payload
                    in
                    let next_state = "let nextState =" in
                    let run_next_state =
                      Printf.sprintf "%s nextState"
                        (FstarNames.run_state_fn_name next)
                    in
                    [ match_pattern
                    ; recv_payload
                    ; recv_callback
                    ; next_state
                    ; construct_next_state var_map ~curr:st ~next action
                        rec_var_info
                    ; "in"
                    ; run_next_state ]
                | _ ->
                    Err.violation
                      "Receiving state should only have outgoing receive \
                       actions"
              in
              entry :: acc
            in
            let match_hands = G.fold_succ_e collect_match_hand g st [] in
            let catch_all = ["| _ -> unexpected \"Unexpected label\""] in
            String.concat ~sep:"\n"
              (connect_selection
               ::
               match_head
               :: List.concat (List.rev (catch_all :: match_hands)) )
      in
      (preamble ^ body) :: acc
    in
    List.rev (G.fold_vertex run_state_fn g [])
  in
  let run_fns =
    "let rec " ^ String.concat ~sep:"\nand " run_state_fns ^ "\nin\n"
  in
  let init_state_content =
    let content =
      let dum_state =
        Printf.sprintf "%s= ()" (FstarNames.dum_state_field_name start)
      in
      let vars =
        match Map.find rec_var_info start with
        | None -> []
        | Some rvs ->
            if List.is_empty rvs then []
            else
              List.map
                ~f:(fun ( is_silent
                        , {Gtype.rv_name; Gtype.rv_ty; Gtype.rv_init_expr; _}
                        ) ->
                  let rv_name = VariableName.user rv_name in
                  let value =
                    if is_silent then
                      Printf.sprintf "(assume false; hide %s)"
                        (Expr.show (Expr.default_value rv_ty))
                    else Expr.show rv_init_expr
                  in
                  Printf.sprintf "%s= %s" rv_name value )
                rvs
      in
      dum_state :: vars
    in
    generate_record_value content
  in
  let init_state =
    Printf.sprintf "let initState: %s =\n%s\nin"
      (FstarNames.state_record_name start)
      init_state_content
  in
  let run_init_state = FstarNames.run_state_fn_name start ^ " initState" in
  Buffer.add_string buffer
    (String.concat ~sep:"\n" [preamble; run_fns; init_state; run_init_state])

let generate_preamble buffer =
  let open_modules = ["FStar.All"; "FStar.Ghost"; "FStar.Error"] in
  Buffer.add_string buffer "module Generated\n"
  (* TODO: Change it to ProtocolName + RoleName *) ;
  List.iter
    ~f:(fun m -> Buffer.add_string buffer (Printf.sprintf "open %s\n" m))
    open_modules

let gen_code (start, g, rec_var_info) =
  let buffer = Buffer.create 4096 in
  let var_map = compute_var_map start g rec_var_info in
  let () = generate_preamble buffer in
  let () = generate_state_defs buffer var_map in
  let () = generate_send_choices buffer g var_map in
  let () = generate_roles buffer (find_all_roles g) in
  let () = generate_transition_typedefs buffer g var_map in
  let () = generate_comms buffer (find_all_payloads g) in
  let () = generate_run_fns buffer start g var_map rec_var_info in
  Buffer.contents buffer
