open! Base
open Printf
open! Ppxlib_ast
open Parsetree
open Asttypes
open Longident
open! Ast_helper
module S = Set
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

let mk_lid id = Location.mknoloc (parse id)

let mk_constr id = Typ.constr (mk_lid id) []

let loc = Location.none

let unit = [%type: unit]

let gen_callback_typedef (g : G.t) : structure_item =
  let env = [%type: 'env] in
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
              Typ.variant rows Closed None
        in
        let return_ty = [%type: [%t env] * [%t return_ty]] in
        let field_name = sprintf "state%dSend" st in
        let field_ty = [%type: [%t env] -> [%t return_ty]] in
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
              let field_ty =
                [%type: [%t env] -> [%t payload_type] -> [%t env]]
              in
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
  let callbacks = List.rev callbacks in
  let callbacks =
    Type.mk ~kind:(Ptype_record callbacks)
      ~params:[(Typ.var "env", Invariant)]
      (Location.mknoloc "callbacks")
  in
  Str.type_ Nonrecursive [callbacks]

let find_all_payloads g =
  let f (_, a, _) acc =
    match a with
    | SendA (_, msg) | RecvA (_, msg) -> (
        let payloads = message_payload_ty msg in
        match payloads with
        | [] -> S.add acc "unit"
        | _ -> List.fold ~f:S.add ~init:acc payloads )
    | _ -> failwith "Impossible"
  in
  G.fold_edges_e f g (S.singleton (module String) "string") |> S.to_list

let find_all_roles g =
  let f (_, a, _) acc =
    match a with
    | SendA (r, _) | RecvA (r, _) -> S.add acc r
    | _ -> failwith "Impossible"
  in
  G.fold_edges_e f g (S.empty (module String)) |> S.to_list

let gen_comms_typedef payload_types =
  let mk_recv payload_ty_str =
    let payload_ty = mk_constr payload_ty_str in
    let field_ty = Typ.arrow Nolabel unit payload_ty in
    let field_name = "recv_" ^ payload_ty_str in
    Type.field (Location.mknoloc field_name) field_ty
  in
  let mk_send payload_ty_str =
    let payload_ty = mk_constr payload_ty_str in
    let field_ty = Typ.arrow Nolabel payload_ty unit in
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
  (* let s = [%stri type 'a t = {x: 'a}] in let s = [s] in let migrate =
     Migrate_parsetree.Versions.migrate Migrate_parsetree.Versions.ocaml_407
     Migrate_parsetree.Versions.ocaml_current in let s =
     migrate.copy_structure s in Printast.structure 0 (Format.std_formatter)
     s; *)
  Str.type_ Nonrecursive [comms]

let gen_role_ty roles =
  let f role = Rtag (Location.mknoloc role, [], true, []) in
  let role_ty = Typ.variant (List.map ~f roles) Closed None in
  role_ty

let gen_run_expr start g =
  let get_transitions st =
    let f (_, a, next) acc =
      match a with
      | SendA (r, msg) | RecvA (r, msg) ->
          (r, message_label msg, message_payload_ty msg, next) :: acc
      | _ -> failwith "Impossible"
    in
    G.fold_succ_e f g st []
  in
  let mk_run_state_name st = sprintf "run_state_%d" st in
  let mk_run_state_ident st = Exp.ident (mk_lid (mk_run_state_name st)) in
  let f st acc =
    let run_state_expr =
      match state_action_type g st with
      | `Send ->
          let send_fn_name = sprintf "state%dSend" st in
          let send_callback =
            Exp.field (Exp.ident (mk_lid "callbacks")) (mk_lid send_fn_name)
          in
          let transitions = get_transitions st in
          let role, _, _, _ = List.hd_exn transitions in
          let role = Exp.variant role None in
          let comms = [%expr router [%e role]] in
          let mk_match_case (_, label, payload_ty, next) =
            let label_e = Exp.constant (Const.string label) in
            let send_label = [%expr comms.send_string [%e label_e]] in
            let send_payload =
              match payload_ty with
              | [] -> [%expr comms.send_unit ()]
              | [payload_ty] ->
                  let send_payload_func =
                    Exp.field
                      (Exp.ident (mk_lid "comms"))
                      (mk_lid (sprintf "send_%s" payload_ty))
                  in
                  [%expr [%e send_payload_func] payload]
              | _ -> failwith "TODO"
            in
            let next_state = mk_run_state_ident next in
            let e =
              [%expr
                [%e send_label] ;
                [%e send_payload] ;
                [%e next_state] env]
            in
            { pc_lhs=
                Pat.tuple
                  [ Pat.var (Location.mknoloc "env")
                  ; Pat.variant label
                      (Some (Pat.var (Location.mknoloc "payload"))) ]
            ; pc_guard= None
            ; pc_rhs= e }
          in
          let match_cases = List.map ~f:mk_match_case transitions in
          let e = Exp.match_ [%expr [%e send_callback] env] match_cases in
          [%expr
            let comms = [%e comms] in
            [%e e]]
      | `Recv ->
          let transitions = get_transitions st in
          let role, _, _, _ = List.hd_exn transitions in
          let role = Exp.variant role None in
          let comms = [%expr router [%e role]] in
          let mk_match_case (_, label, payload_ty, next) =
            let recv_payload =
              match payload_ty with
              | [] -> [%expr comms.recv_unit ()]
              | [payload_ty] ->
                  let recv_payload_func =
                    Exp.field
                      (Exp.ident (mk_lid "comms"))
                      (mk_lid (sprintf "recv_%s" payload_ty))
                  in
                  [%expr [%e recv_payload_func] ()]
              | _ -> failwith "TODO"
            in
            let recv_callback_name = sprintf "state%dReceive%s" st label in
            let recv_callback =
              Exp.field
                (Exp.ident (mk_lid "callbacks"))
                (mk_lid recv_callback_name)
            in
            let next_state = mk_run_state_ident next in
            let e =
              [%expr
                let payload = [%e recv_payload] in
                let env = [%e recv_callback] env payload in
                [%e next_state] env]
            in
            { pc_lhs= Pat.constant (Const.string label)
            ; pc_guard= None
            ; pc_rhs= e }
          in
          let impossible_case =
            {pc_lhs= Pat.any (); pc_guard= None; pc_rhs= [%expr assert false]}
          in
          let match_cases = List.map ~f:mk_match_case transitions in
          let e =
            Exp.match_ [%expr comms.recv_string ()]
              (match_cases @ [impossible_case])
          in
          [%expr
            let comms = [%e comms] in
            [%e e]]
      | `Terminal -> [%expr env]
      | `Mixed -> failwith "Impossible"
    in
    let run_state_expr = [%expr fun env -> [%e run_state_expr]] in
    let pat = Pat.var (Location.mknoloc (mk_run_state_name st)) in
    Vb.mk pat run_state_expr :: acc
  in
  let bindings = G.fold_vertex f g [] in
  let bindings = List.rev bindings in
  let init_expr =
    Exp.apply (mk_run_state_ident start) [(Nolabel, [%expr env])]
  in
  Exp.let_ Recursive bindings init_expr

let gen_ast (_proto, _role) (start, g) : structure =
  let loc = Location.none in
  let callback_typedef = gen_callback_typedef g in
  let payload_types = find_all_payloads g in
  let comms_typedef = gen_comms_typedef payload_types in
  let roles = find_all_roles g in
  let role_ty = gen_role_ty roles in
  let run_expr = gen_run_expr start g in
  let run =
    [%stri
      let run (router : [%t role_ty] -> comms) (callbacks : 'env callbacks)
          (env : 'env) =
        [%e run_expr]]
  in
  [callback_typedef; comms_typedef; run]

let gen_code (proto, role) (start, g) =
  let buffer = Buffer.create 4196 in
  let formatter = Caml.Format.formatter_of_buffer buffer in
  let ast = gen_ast (proto, role) (start, g) in
  Pprintast.structure formatter ast ;
  Caml.Format.pp_print_flush formatter () ;
  Buffer.contents buffer
