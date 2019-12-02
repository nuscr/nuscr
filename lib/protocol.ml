open! Base
open Syntax
open Err

let rec swap_role swap_role_f {Loc.value; Loc.loc} =
  let value =
    match value with
    | MessageTransfer {message; from_role; to_roles; ann} ->
        MessageTransfer
          { message
          ; from_role= swap_role_f from_role
          ; to_roles= List.map ~f:swap_role_f to_roles
          ; ann }
    | Recursion (rec_name, g) ->
        Recursion (rec_name, List.map ~f:(swap_role swap_role_f) g)
    | Continue rec_name -> Continue rec_name
    | Choice (role, gs) ->
        Choice
          ( swap_role_f role
          , List.map ~f:(List.map ~f:(swap_role swap_role_f)) gs )
    | Do (proto, msgs, roles, ann) ->
        Do (proto, msgs, List.map ~f:swap_role_f roles, ann)
    | Connect (msg, r1, r2, ann) ->
        Connect (msg, swap_role_f r1, swap_role_f r2, ann)
    | Disconnect (r1, r2) -> Disconnect (swap_role_f r1, swap_role_f r2)
  in
  {value; loc}

let instantiate (protocol : raw_global_protocol)
    (replacement_roles : name list) =
  let original_roles = protocol.roles in
  let replacement_map = List.zip original_roles replacement_roles in
  let replacement_map =
    match replacement_map with
    | List.Or_unequal_lengths.Unequal_lengths -> unimpl "Mismatched arity"
    | List.Or_unequal_lengths.Ok replacement_map -> replacement_map
  in
  let replacement_map =
    List.fold
      ~f:(fun acc (ori, rep) ->
        Map.add_exn acc ~key:(Name.user ori) ~data:rep)
      ~init:(Map.empty (module String))
      replacement_map
  in
  let swap_f r = Map.find_exn replacement_map (Name.user r) in
  List.map ~f:(swap_role swap_f) protocol.interactions

let rec_var_of_protocol_roles (name, roles) =
  let names = List.map ~f:Name.user (name :: roles) in
  Name.of_string @@ Printf.sprintf "__%s" (String.concat ~sep:"_" names)

let mk_protocol_map scr_module =
  let f acc {Loc.value= p; Loc.loc} =
    match
      Map.add acc ~key:(Name.user p.name) ~data:(p, loc, List.length p.roles)
    with
    | `Ok acc -> acc
    | `Duplicate ->
        let _, old_loc, _ = Map.find_exn acc (Name.user p.name) in
        uerr (RedefinedProtocol (p.name, loc, old_loc))
  in
  let protocols =
    List.fold ~f ~init:(Map.empty (module String)) scr_module.protocols
  in
  protocols

(** Unroll `do` in protocol *)
let expand_global_protocol (scr_module : scr_module)
    (protocol : global_protocol) =
  let protocols = mk_protocol_map scr_module in
  let known_roles = protocol.value.roles in
  let check_role r =
    if List.mem known_roles ~equal:Name.equal r then ()
    else unimpl "Error message for unbound role"
  in
  let maybe_add_recursion ~loc ~known (name, roles) interactions =
    let has_recursion = Map.find_exn known (name, roles) in
    let interactions =
      if has_recursion then
        [ { Loc.loc
          ; Loc.value=
              Recursion
                (rec_var_of_protocol_roles (name, roles), interactions) } ]
      else interactions
    in
    interactions
  in
  let rec expand_aux known interactions =
    let expand_single known ({Loc.value; Loc.loc} as i) =
      (* known is a map to (protocol, name) -> bool
       * true indicates that the protocol has been called, meaning it is recursive;
       * false otherwise *)
      match value with
      | Do (name, [], roles, None) when Map.Poly.mem known (name, roles) ->
          let known = Map.update known (name, roles) ~f:(fun _ -> true) in
          ( known
          , [ { Loc.value= Continue (rec_var_of_protocol_roles (name, roles))
              ; loc } ] )
      | Do (name, [], roles, None) ->
          let protocol_to_expand = Map.find protocols (Name.user name) in
          let protocol_to_expand, _, arity =
            match protocol_to_expand with
            | Some p -> p
            | None -> unimpl "Error msg for unbound protocol"
          in
          if List.length roles <> arity then
            unimpl "Error msg for arity mismatch" ;
          List.iter ~f:check_role roles ;
          let known = Map.add_exn known ~key:(name, roles) ~data:false in
          let known, interactions =
            expand_aux known (instantiate protocol_to_expand roles)
          in
          let interactions =
            maybe_add_recursion ~loc ~known (name, roles) interactions
          in
          let known = Map.remove known (name, roles) in
          (known, interactions)
      | Do _ -> unimpl "Do with other features"
      | Recursion (r, is) ->
          let known, is = expand_aux known is in
          (known, [{i with value= Recursion (r, is)}])
      | Choice (r, iss) ->
          let known, iss = List.fold_map ~f:expand_aux ~init:known iss in
          (known, [{i with value= Choice (r, iss)}])
      | _ -> (known, [i])
    in
    let known, interactions =
      List.fold_map ~f:expand_single ~init:known interactions
    in
    (known, List.concat interactions)
  in
  let top_level = (protocol.value.name, protocol.value.roles) in
  let known, interactions =
    expand_aux
      (Map.Poly.singleton top_level false)
      protocol.value.interactions
  in
  let interactions =
    maybe_add_recursion ~loc:protocol.loc ~known top_level interactions
  in
  {protocol with Loc.value= {protocol.Loc.value with interactions}}
