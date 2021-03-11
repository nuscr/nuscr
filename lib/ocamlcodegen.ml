open! Base
open Printf
open! Ppxlib_ast
open Parsetree
open Asttypes
open Longident
open! Ast_helper
open Names
open Efsm

let mk_lid id = Location.mknoloc (Option.value_exn (unflatten [id]))

let mk_constr id = Typ.constr (mk_lid id) []

let mk_variant desc =
  {prf_desc= desc; prf_loc= Location.none; prf_attributes= []}

let loc = Location.none

let unit = [%type: unit]

let mk_receive_callback st label = sprintf "state%dReceive%s" st label

let mk_send_callback st = sprintf "state%dSend" st

let payload_values payloads =
  List.map
    ~f:(fun p -> PayloadTypeName.user @@ Gtype.typename_of_payload p)
    payloads

let process_msg (msg : Gtype.message) =
  let {Gtype.label; Gtype.payload} = msg in
  let payload_values = payload_values payload in
  let payload_type =
    match payload_values with
    | [] -> unit
    | [x] -> mk_constr x
    | _ -> Typ.tuple (List.map ~f:mk_constr payload_values)
  in
  (label, payload_type)

let gen_callback_module (g : G.t) : structure_item =
  let env = [%type: t] in
  let f st acc =
    match state_action_type g st with
    | `Mixed -> Err.violation "Mixed states should not occur in an EFSM"
    | `Terminal -> acc
    | `Send _ ->
        let gen_send (_, a, _) acc =
          match a with
          | SendA (_, msg, _) ->
              let label, payload_type = process_msg msg in
              let row =
                mk_variant
                  (Rtag
                     ( Location.mknoloc (LabelName.user label)
                     , true
                     , [payload_type] ) )
              in
              row :: acc
          | _ -> Err.violation "Sending states should only have send actions"
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
    | `Recv _ ->
        let gen_recv (_, a, _) callbacks =
          match a with
          | RecvA (_, msg, _) ->
              let label, payload_type = process_msg msg in
              let ty = [%type: [%t env] -> [%t payload_type] -> [%t env]] in
              let name = mk_receive_callback st (LabelName.user label) in
              let val_ = Val.mk (Location.mknoloc name) ty in
              val_ :: callbacks
          | _ ->
              Err.violation
                "Receiving states should only have receive actions"
        in
        G.fold_succ_e gen_recv g st acc
  in
  let callbacks = G.fold_vertex f g [] in
  let callbacks = List.rev_map ~f:(Sig.value ~loc) callbacks in
  let env_type = Sig.type_ Nonrecursive [Type.mk (Location.mknoloc "t")] in
  let callbacks = Mty.signature (env_type :: callbacks) in
  Str.modtype (Mtd.mk ~typ:callbacks (Location.mknoloc "Callbacks"))

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
  let f role = mk_variant (Rtag (Location.mknoloc role, true, [])) in
  let role_ty = Typ.variant (List.map ~f roles) Closed None in
  role_ty

let get_transitions g st =
  let f (_, a, next) acc =
    match a with
    | SendA (r, msg, _) | RecvA (r, msg, _) ->
        let {Gtype.label; Gtype.payload} = msg in
        (r, LabelName.user label, payload_values payload, next) :: acc
    | _ ->
        Err.violation
          "Epsilon transtions should not appear after EFSM generation"
  in
  G.fold_succ_e f g st []

let gen_run_expr ~monad start g =
  let mk_run_state_name st = sprintf "run_state_%d" st in
  let mk_run_state_ident st = Exp.ident (mk_lid (mk_run_state_name st)) in
  let f st acc =
    let run_state_expr_inner =
      match state_action_type g st with
      | `Terminal -> if monad then [%expr M.return env] else [%expr env]
      | `Mixed -> failwith "Impossible"
      | (`Send role | `Recv role) as action ->
          let transitions = get_transitions g st in
          let role = Exp.variant (RoleName.user role) None in
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
            | `Send _ ->
                let label_e = Exp.constant (Const.string label) in
                let send_label = [%expr comms.send_string [%e label_e]] in
                let send_payload = comm_payload `Send payload_ty in
                { pc_lhs=
                    Pat.tuple
                      [ [%pat? env]
                      ; Pat.variant label
                          (Some
                             ( if List.is_empty payload_ty then [%pat? ()]
                             else [%pat? payload] ) ) ]
                ; pc_guard= None
                ; pc_rhs=
                    ( if monad then
                      [%expr
                        let* () = [%e send_label] in
                        let* () = [%e send_payload] in
                        [%e next_state] env]
                    else
                      [%expr
                        [%e send_label] ;
                        [%e send_payload] ;
                        [%e next_state] env] ) }
            | `Recv _ ->
                let recv_payload = comm_payload `Recv payload_ty in
                let recv_callback_name = mk_receive_callback st label in
                let recv_callback = Exp.ident (mk_lid recv_callback_name) in
                { pc_lhs= Pat.constant (Const.string label)
                ; pc_guard= None
                ; pc_rhs=
                    ( if monad then
                      [%expr
                        let* payload = [%e recv_payload] in
                        let env = [%e recv_callback] env payload in
                        [%e next_state] env]
                    else
                      [%expr
                        let payload = [%e recv_payload] in
                        let env = [%e recv_callback] env payload in
                        [%e next_state] env] ) }
          in
          let match_cases = List.map ~f:mk_match_case transitions in
          let impossible_case =
            {pc_lhs= Pat.any (); pc_guard= None; pc_rhs= [%expr assert false]}
          in
          let e =
            match action with
            | `Send _ ->
                Exp.match_ [%expr [%e send_callback] env] match_cases
            | `Recv _ ->
                let e =
                  Exp.match_ [%expr label] (match_cases @ [impossible_case])
                in
                if monad then
                  [%expr
                    let* label = comms.recv_string () in
                    [%e e]]
                else
                  [%expr
                    let label = comms.recv_string () in
                    [%e e]]
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

let gen_impl_module ~monad (proto : ProtocolName.t) (role : RoleName.t) start
    g =
  let module_name =
    sprintf "Impl_%s_%s" (ProtocolName.user proto) (RoleName.user role)
  in
  let payload_types = find_all_payloads g |> Set.to_list in
  let payload_types = List.map ~f:PayloadTypeName.user payload_types in
  let comms_typedef = gen_comms_typedef ~monad payload_types in
  let roles = find_all_roles g |> Set.to_list in
  let roles = List.map ~f:RoleName.user roles in
  let role_ty = gen_role_ty roles in
  let run_expr = gen_run_expr ~monad start g in
  let run =
    [%stri
      let run (router : [%t role_ty] -> comms) (env : CB.t) =
        let open CB in
        [%e run_expr]]
  in
  let let_syntax = [%stri let ( let* ) x f = M.bind x f] in
  let inner_structure =
    Mod.structure
      ((if monad then [let_syntax] else []) @ [comms_typedef; run])
  in
  let inner_structure =
    if monad then
      Mod.functor_
        (Named (Location.mknoloc (Some "M"), Mty.ident (mk_lid "Monad")))
        inner_structure
    else inner_structure
  in
  let module_ =
    Mod.functor_
      (Named (Location.mknoloc (Some "CB"), Mty.ident (mk_lid "Callbacks")))
      inner_structure
  in
  Str.module_ (Mb.mk (Location.mknoloc (Some module_name)) module_)

let monad_signature =
  let monad_type =
    Sig.type_ Nonrecursive
      [ Type.mk
          ~params:[(Typ.var "a", (NoVariance, NoInjectivity))]
          (Location.mknoloc "t") ]
  in
  let return =
    Sig.value (Val.mk (Location.mknoloc "return") [%type: 'a -> 'a t])
  in
  let bind =
    Sig.value
      (Val.mk (Location.mknoloc "bind") [%type: 'a t -> ('a -> 'b t) -> 'b t])
  in
  let callbacks = Mty.signature [monad_type; return; bind] in
  Str.modtype (Mtd.mk ~typ:callbacks (Location.mknoloc "Monad"))

let gen_ast ?(monad = false) ((proto, role) : ProtocolName.t * RoleName.t)
    (start, g) : structure =
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
