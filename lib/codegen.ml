open! Core_kernel

(* open Efsm *)

open! Ppxlib_ast
open Parsetree
open! Ast_helper
open Syntax
open Efsm

let state_action_type (g : G.t) (st : int) =
  let merge_state_action_type aty1 aty2 =
    match (aty1, aty2) with
    | `Terminal, aty2 -> aty2
    | aty1, `Terminal -> aty1
    | `Send, `Send -> `Send
    | `Recv, `Recv -> `Recv
    | `Send, `Recv -> `Mixed
    | `Recv, `Send -> `Mixed
    | aty1, `Mixed -> aty1
    | `Mixed, aty2 -> aty2
  in
  let f (_, a, _) acc =
    let aty =
      match a with
      | SendA _ -> `Send
      | RecvA _ -> `Recv
      | Epsilon -> failwith "Impossible"
    in
    merge_state_action_type aty acc
  in
  G.fold_succ_e f g st `Terminal

let mk_constr id = Typ.constr (Location.mknoloc (Longident.parse id)) []

let loc = Location.none

let unit = [%type: unit]

let gen_callback_typedef (g : G.t) : structure_item =
  let f st acc =
    match state_action_type g st with
    | `Mixed -> failwith "Impossible"
    | `Terminal -> acc
    | `Send ->
        let gen_send (_, a, _) acc =
          match a with
          | SendA (_, msg) ->
              let label = message_label msg in
              let payload_type = message_payload_ty msg in
              let payload_type =
                match payload_type with
                | [] -> unit
                | [x] -> mk_constr x
                | _ -> Typ.tuple (List.map ~f:mk_constr payload_type)
              in
              (label, payload_type) :: acc
          | _ -> failwith "Impossible"
        in
        let return_ty = G.fold_succ_e gen_send g st [] in
        let return_ty =
          match return_ty with
          | [] -> unit
          | _ ->
              let f (label, payload_ty) =
                Rtag (Location.mknoloc label, [], true, [payload_ty])
              in
              let rows = List.map ~f return_ty in
              Typ.variant rows Asttypes.Closed None
        in
        let field_name = sprintf "state%dSend" st in
        let field_ty =
          Typ.arrow Asttypes.Nolabel (mk_constr "unit") return_ty
        in
        let field = Type.field (Location.mknoloc field_name) field_ty in
        field :: acc
    | `Recv ->
        let gen_recv (_, a, _) callbacks =
          match a with
          | RecvA (_, msg) ->
              let label = message_label msg in
              let payload_type = message_payload_ty msg in
              let payload_type =
                match payload_type with
                | [] -> unit
                | [x] -> mk_constr x
                | _ -> Typ.tuple (List.map ~f:mk_constr payload_type)
              in
              let field_ty = Typ.arrow Asttypes.Nolabel payload_type unit in
              let field_name = sprintf "state%dReceive%s" st label in
              let field =
                Type.field (Location.mknoloc field_name) field_ty
              in
              field :: callbacks
          | _ -> failwith "Impossible"
        in
        G.fold_succ_e gen_recv g st acc
  in
  let callbacks = G.fold_vertex f g [] in
  let callbacks =
    Type.mk ~kind:(Ptype_record callbacks) (Location.mknoloc "callbacks")
  in
  Str.type_ Asttypes.Nonrecursive [callbacks]

let find_all_payloads g =
  let module S = String.Set in
  let f (_, a, _) acc =
    match a with
    | SendA (_, msg) | RecvA (_, msg) -> (
        let payloads = message_payload_ty msg in
        match payloads with
        | [] -> S.add acc "unit"
        | _ -> List.fold ~f:S.add ~init:acc payloads )
    | _ -> failwith "Impossible"
  in
  G.fold_edges_e f g (S.singleton "string") |> S.to_list

let find_all_roles g =
  let module S = String.Set in
  let f (_, a, _) acc =
    match a with
    | SendA (r, _) | RecvA (r, _) -> S.add acc r
    | _ -> failwith "Impossible"
  in
  G.fold_edges_e f g S.empty |> S.to_list

let gen_comms_typedef payload_types =
  let mk_recv payload_ty_str =
    let payload_ty = mk_constr payload_ty_str in
    let field_ty = Typ.arrow Asttypes.Nolabel unit payload_ty in
    let field_name = "recv_" ^ payload_ty_str in
    Type.field (Location.mknoloc field_name) field_ty
  in
  let mk_send payload_ty_str =
    let payload_ty = mk_constr payload_ty_str in
    let field_ty = Typ.arrow Asttypes.Nolabel payload_ty unit in
    let field_name = "send_" ^ payload_ty_str in
    Type.field (Location.mknoloc field_name) field_ty
  in
  let send_functions = List.map ~f:mk_send payload_types in
  let recv_functions = List.map ~f:mk_recv payload_types in
  let comms =
    Type.mk
      ~kind:(Ptype_record (send_functions @ recv_functions))
      (Location.mknoloc "comms")
  in
  Str.type_ Asttypes.Nonrecursive [comms]

let gen_role_ty roles =
  let f role = Rtag (Location.mknoloc role, [], true, []) in
  let role_ty = Typ.variant (List.map ~f roles) Asttypes.Closed None in
  role_ty

let gen_ast (_proto, _role) (_start, g) : structure =
  let loc = Location.none in
  let callback_typedef = gen_callback_typedef g in
  let payload_types = find_all_payloads g in
  let comms_typedef = gen_comms_typedef payload_types in
  let roles = find_all_roles g in
  let role_ty = gen_role_ty roles in
  let run_expr = [%expr ()] in
  let run =
    [%stri
      let run (router : [%t role_ty] -> comms) (callbacks : callbacks) =
        [%e run_expr]]
  in
  [callback_typedef; comms_typedef; run]

let gen_code (proto, role) (start, g) =
  let buffer = Buffer.create 4196 in
  let formatter = Format.formatter_of_buffer buffer in
  let ast = gen_ast (proto, role) (start, g) in
  Pprintast.structure formatter ast ;
  Format.pp_print_flush formatter () ;
  Buffer.contents buffer
