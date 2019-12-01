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

let state_action_type (g : Efsm.t) (st : int) =
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

let mk_receive_callback st label = sprintf "state%dReceive%s" st label

let mk_send_callback st = sprintf "state%dSend" st

let process_msg msg =
  let label = message_label msg in
  let payload_type = message_payload_ty msg in
  let payload_type =
    match payload_type with
    | [] -> unit
    | [x] -> mk_constr x
    | _ -> Typ.tuple (List.map ~f:mk_constr payload_type)
  in
  (label, payload_type)

let gen_callback_module (g : G.t) : structure_item =
  let env = [%type: t] in
  let f st acc =
    match state_action_type g st with
    | `Mixed -> failwith "Impossible"
    | `Terminal -> acc
    | `Send ->
        let gen_send (_, a, _) acc =
          match a with
          | SendA (_, msg) ->
              let label, payload_type = process_msg msg in
              let row =
                Rtag (Location.mknoloc label, [], true, [payload_type])
              in
              row :: acc
          | _ -> failwith "Impossible"
        in
        let rows = G.fold_succ_e gen_send g st [] in
        let rows =
          match rows with [] -> unit | _ -> Typ.variant rows Closed None
        in
        let return_ty = [%type: [%t env] * [%t rows]] in
        let name = mk_send_callback st in
        let ty = [%type: [%t env] -> [%t return_ty]] in
        let val_ = Val.mk (Location.mknoloc name) ty in
        val_ :: acc
    | `Recv ->
        let gen_recv (_, a, _) callbacks =
          match a with
          | RecvA (_, msg) ->
              let label, payload_type = process_msg msg in
              let ty = [%type: [%t env] -> [%t payload_type] -> [%t env]] in
              let name = mk_receive_callback st label in
              let val_ = Val.mk (Location.mknoloc name) ty in
              val_ :: callbacks
          | _ -> failwith "Impossible"
        in
        G.fold_succ_e gen_recv g st acc
  in
  let callbacks = G.fold_vertex f g [] in
  let callbacks = List.rev_map ~f:(Sig.value ~loc) callbacks in
  let env_type = Sig.type_ Nonrecursive [Type.mk (Location.mknoloc "t")] in
  let callbacks = Mty.signature (env_type :: callbacks) in
  Str.modtype (Mtd.mk ~typ:callbacks (Location.mknoloc "Callbacks"))

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

let find_all_roles _g =
  let _f (_, a, _) acc =
    match a with
    | SendA (r, _) | RecvA (r, _) -> S.add acc r
    | _ -> failwith "Impossible"
  in
  (* G.fold_edges_e f g (S.empty (module String)) |> S.to_list *)
  assert false

let gen_comms_typedef ~monad payload_types =
  let mk_monadic ty = if monad then [%type: [%t ty] M.t] else ty in
  let mk_recv payload_ty_str =
    let payload_ty = mk_constr payload_ty_str in
    let field_ty = Typ.arrow Nolabel unit (mk_monadic payload_ty) in
    let field_name = "recv_" ^ payload_ty_str in
    Type.field (Location.mknoloc field_name) field_ty
  in
  let mk_send payload_ty_str =
    let payload_ty = mk_constr payload_ty_str in
    let field_ty = Typ.arrow Nolabel payload_ty (mk_monadic unit) in
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
  Str.type_ Nonrecursive [comms]

let gen_role_ty roles =
  let f role = Rtag (Location.mknoloc role, [], true, []) in
  let role_ty = Typ.variant (List.map ~f roles) Closed None in
  role_ty

let get_transitions g st =
  let f (_, a, next) acc =
    match a with
    | SendA (r, msg) | RecvA (r, msg) ->
        (r, message_label msg, message_payload_ty msg, next) :: acc
    | _ -> failwith "Impossible"
  in
  G.fold_succ_e f g st []

let gen_run_expr ~monad start g =
  ignore monad ;
  let mk_run_state_name st = sprintf "run_state_%d" st in
  let mk_run_state_ident st = Exp.ident (mk_lid (mk_run_state_name st)) in
  let f st acc =
    let run_state_expr_inner =
      match state_action_type g st with
      | `Terminal -> [%expr M.return env]
      | `Mixed -> failwith "Impossible"
      | (`Send | `Recv) as action ->
          let transitions = get_transitions g st in
          let role, _, _, _ = List.hd_exn transitions in
          let role = Exp.variant role.value None in
          let comms = [%expr router [%e role]] in
          let send_fn_name = sprintf "state%dSend" st in
          let send_callback = Exp.ident (mk_lid send_fn_name) in
          let comm_payload_func action payload_ty =
            let verb =
              match action with `Send -> "send" | `Recv -> "recv"
            in
            Exp.field
              (Exp.ident (mk_lid "comms"))
              (mk_lid (sprintf "%s_%s" verb payload_ty))
          in
          let comm_payload action = function
            | [] ->
                let comm_func = comm_payload_func action "unit" in
                [%expr [%e comm_func] ()]
            | [payload_ty] ->
                let comm_payload_func =
                  comm_payload_func action payload_ty
                in
                let arg =
                  match action with
                  | `Send -> [%expr payload]
                  | `Recv -> [%expr ()]
                in
                [%expr [%e comm_payload_func] [%e arg]]
            | _ -> failwith "TODO"
          in
          let mk_match_case (_, label, payload_ty, next) =
            let next_state = mk_run_state_ident next in
            match action with
            | `Send ->
                let label_e = Exp.constant (Const.string label) in
                let send_label = [%expr comms.send_string [%e label_e]] in
                let send_payload = comm_payload `Send payload_ty in
                { pc_lhs=
                    Pat.tuple
                      [[%pat? env]; Pat.variant label (Some [%pat? payload])]
                ; pc_guard= None
                ; pc_rhs=
                    [%expr
                      let ret = [%e send_label] in
                      M.bind ret (fun () ->
                          let ret = [%e send_payload] in
                          M.bind ret (fun () -> [%e next_state] env))] }
            | `Recv ->
                let recv_payload = comm_payload `Recv payload_ty in
                let recv_callback_name = mk_receive_callback st label in
                let recv_callback = Exp.ident (mk_lid recv_callback_name) in
                { pc_lhs= Pat.constant (Const.string label)
                ; pc_guard= None
                ; pc_rhs=
                    [%expr
                      let payload = [%e recv_payload] in
                      M.bind payload (fun payload ->
                          let env = [%e recv_callback] env payload in
                          [%e next_state] env)] }
          in
          let match_cases = List.map ~f:mk_match_case transitions in
          let impossible_case =
            {pc_lhs= Pat.any (); pc_guard= None; pc_rhs= [%expr assert false]}
          in
          let e =
            match action with
            | `Send -> Exp.match_ [%expr [%e send_callback] env] match_cases
            | `Recv ->
                let e =
                  Exp.match_ [%expr label] (match_cases @ [impossible_case])
                in
                [%expr
                  let label = comms.recv_string () in
                  M.bind label (fun label -> [%e e])]
          in
          [%expr
            let comms = [%e comms] in
            [%e e]]
    in
    let run_state_expr = [%expr fun env -> [%e run_state_expr_inner]] in
    let pat = Pat.var (Location.mknoloc (mk_run_state_name st)) in
    Vb.mk pat run_state_expr :: acc
  in
  let bindings = G.fold_vertex f g [] in
  let bindings = List.rev bindings in
  let init_expr =
    Exp.apply (mk_run_state_ident start) [(Nolabel, [%expr env])]
  in
  Exp.let_ Recursive bindings init_expr

let gen_impl_module ~monad (proto : name) (role : name) start g =
  let module_name = sprintf "Impl_%s_%s" proto.value role.value in
  let payload_types = find_all_payloads g in
  let comms_typedef = gen_comms_typedef ~monad payload_types in
  let roles = find_all_roles g in
  let role_ty = gen_role_ty roles in
  let run_expr = gen_run_expr ~monad start g in
  let run =
    [%stri
      let run (router : [%t role_ty] -> comms) (env : CB.t) =
        let open CB in
        [%e run_expr]]
  in
  let inner_structure = Mod.structure [comms_typedef; run] in
  let inner_structure =
    if monad then
      Mod.functor_ (Location.mknoloc "M")
        (Some (Mty.ident (mk_lid "Monad")))
        inner_structure
    else inner_structure
  in
  let module_ =
    Mod.functor_ (Location.mknoloc "CB")
      (Some (Mty.ident (mk_lid "Callbacks")))
      inner_structure
  in
  Str.module_ (Mb.mk (Location.mknoloc module_name) module_)

let monad_signature =
  let monad_type =
    Sig.type_ Nonrecursive
      [Type.mk ~params:[(Typ.var "a", Invariant)] (Location.mknoloc "t")]
  in
  let return =
    Sig.value (Val.mk (Location.mknoloc "return") [%type: 'a -> 'a t])
  in
  let bind =
    Sig.value
      (Val.mk (Location.mknoloc "bind")
         [%type: 'a t -> ('a -> 'b t) -> 'b t])
  in
  let callbacks = Mty.signature [monad_type; return; bind] in
  Str.modtype (Mtd.mk ~typ:callbacks (Location.mknoloc "Monad"))

let gen_ast ?(monad = false) ((proto, role) : name * name) (start, g) : structure =
  let callback_module_sig = gen_callback_module g in
  let impl_module = gen_impl_module ~monad proto role start g in
  let all = [callback_module_sig; impl_module] in
  if monad then monad_signature :: all else all

let gen_code ?(monad = false) (proto, role) (start, g) =
  let buffer = Buffer.create 4196 in
  let formatter = Caml.Format.formatter_of_buffer buffer in
  let ast = gen_ast ~monad (proto, role) (start, g) in
  Pprintast.structure formatter ast ;
  Caml.Format.pp_print_flush formatter () ;
  Buffer.contents buffer
