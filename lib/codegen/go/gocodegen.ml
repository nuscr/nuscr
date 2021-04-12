open! Base
open Ltype
open Goimpl
open Gtype
open Message
open Fsutil
open Names
open! Gonames

let create_pkg pkg_name = create_dir (PackageName.user pkg_name)

(** Validate protocols to ensure Go implementation can be generated

    For current code generation scheme:

    - protocol names must be unique when lowercased
    - role names must be unique when lowercased
    - msg labels must be unique when capitalised (can't have m1() and M1())
      and have the same payload whenever they are used within the same
      protocol
    - payload field names must be unique when capitalised *)
let ensure_unique_identifiers (global_t : Gtype.nested_t) =
  let add_unique_protocol_name protocols protocol_name =
    let name_str = PackageName.protocol_pkg_name protocol_name in
    if Set.mem protocols name_str then
      Err.violation ~here:[%here]
        "Protocol names must be unique when lowercased" ;
    Set.add protocols name_str
  in
  let add_unique_role_name roles role =
    let role_str = RoleName.to_lowercase_string role in
    if Set.mem roles role_str then
      Err.violation ~here:[%here] "Role names must be unique when lowercased" ;
    Set.add roles role_str
  in
  (* Ensure message labels are unique when capitalised *)
  let add_consistent_msg msgs {label; payload} =
    let add_unique_payload_field fields payload =
      match payload with
      | PValue (None, _) -> fields
      | PValue (Some field_name, _) ->
          let field_str = VariableName.to_capitalize_string field_name in
          if Set.mem fields field_str then
            Err.uerr
              (Err.DuplicatePayloadField
                 (label, VariableName.of_string field_str) ) ;
          Set.add fields field_str
      | PDelegate _ -> Err.violation ~here:[%here] "Delegation not supported"
    in
    let label_str = LabelName.to_capitalize_string label in
    match Map.find msgs label_str with
    | None ->
        let _ =
          List.fold
            ~init:(Set.empty (module String))
            ~f:add_unique_payload_field payload
        in
        Map.add_exn msgs ~key:label_str ~data:(label, payload)
    | Some (label', payload') ->
        if not (LabelName.equal label label') then
          Err.violation ~here:[%here]
            "Message labels must be unique when capitalized cased" ;
        if not (List.equal equal_pvalue_payload payload payload') then
          Err.violation ~here:[%here]
            "Within a protocol, messages with the same label should  have \
             the same payloads" ;
        msgs
  in
  let validate_protocol ~key ~data protocol_names =
    let rec validate_protocol_msgs messages gtype =
      match gtype with
      | EndG | TVarG _ -> messages
      | MuG (_, _, g) | CallG (_, _, _, g) ->
          validate_protocol_msgs messages g
      | ChoiceG (_, gtypes) ->
          List.fold ~init:messages ~f:validate_protocol_msgs gtypes
      | MessageG (msg, _, _, g) ->
          let messages = add_consistent_msg messages msg in
          validate_protocol_msgs messages g
    in
    let protocol_names = add_unique_protocol_name protocol_names key in
    let {static_roles; dynamic_roles; gtype; _} = data in
    let _ =
      List.fold
        ~init:(Set.empty (module String))
        ~f:add_unique_role_name
        (static_roles @ dynamic_roles)
    in
    let _ = validate_protocol_msgs (Map.empty (module String)) gtype in
    protocol_names
  in
  let _ =
    Map.fold
      ~init:(Set.empty (module PackageName))
      ~f:validate_protocol global_t
  in
  ()

module RolePair = struct
  module T = struct
    type t = ProtocolName.t * RoleName.t * RoleName.t
    [@@deriving sexp_of, ord]
  end

  include T
  include Comparable.Make (T)
end

module Namegen = Namegen.Make (VariableName)

module ProtoLblPair = struct
  module T = struct
    type t = ProtocolName.t * LabelName.t [@@deriving sexp_of, ord]
  end

  include T
  include Comparable.Make (T)
end

module GoGenM = struct
  type ctx =
    { channels: (VariableName.t * goType) Map.M(RolePair).t
    ; ctx_vars: VariableName.t Map.M(LocalProtocolId).t
    ; curr_fn: LocalProtocolId.t option }

  type state =
    { namegen: Namegen.t
    ; lp_fns: LocalProtocolName.t Map.M(LocalProtocolId).t
    ; msg_iface: goType Map.M(ProtocolName).t
    ; req_chans: (RoleName.t * RoleName.t) list Map.M(LocalProtocolId).t
    ; ctx_type: goType Map.M(LocalProtocolId).t
    ; proto_lbls: goTyDecl Map.M(ProtoLblPair).t
    ; callbacks: (FunctionName.t * goType) list Map.M(LocalProtocolId).t
    ; lp_ctx: ctx }
  (* type 'a t = state -> state * 'a *)

  let init_lp_ctx =
    { channels= Map.empty (module RolePair)
    ; ctx_vars= Map.empty (module LocalProtocolId)
    ; curr_fn= None }

  let init =
    { namegen= Namegen.create ()
    ; lp_fns= Map.empty (module LocalProtocolId)
    ; msg_iface= Map.empty (module ProtocolName)
    ; req_chans= Map.empty (module LocalProtocolId)
    ; ctx_type= Map.empty (module LocalProtocolId)
    ; proto_lbls= Map.empty (module ProtoLblPair)
    ; callbacks= Map.empty (module LocalProtocolId)
    ; lp_ctx= init_lp_ctx }

  let get (st : state) = (st, st)

  let put st _ = (st, ())

  let map f m st =
    let st', x = m st in
    (st', f x)

  let bind m k (st : state) =
    let st', x = m st in
    k x st'

  (* let product m1 m2 st = let (st', x) = m1 st in let (st'', y) = m2 st' in
     (st'', (x, y)) *)

  (* let run m st = m st *)
  let eval m (st : state) = snd (m st)

  (* TODO: clean up local state for current definition *)
  let cleanup st = (st, ())

  module Syntax = struct
    (* let ( let+ ) x f = map f x *)
    (* let ( and+ ) m1 m2 = product m1 m2 *)
    let ( let* ) x f = bind x f

    let pure x = function st -> (st, x)
  end
end

let fresh nm =
  let nm = VariableName.of_string nm in
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ng, nm' = Namegen.unique_name st.GoGenM.namegen nm in
  let* _ = GoGenM.put {st with GoGenM.namegen= ng} in
  pure nm'

let find_lp_name ~id =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  pure (Map.find st.GoGenM.lp_fns id)

let put_lp_name ~key ~data =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  GoGenM.put {st with GoGenM.lp_fns= Map.add_exn st.GoGenM.lp_fns ~key ~data}

let get_lp_name ~id =
  let open GoGenM.Syntax in
  let pn = LocalProtocolId.get_protocol id |> ProtocolName.user in
  let rn = LocalProtocolId.get_role id |> RoleName.user in
  let nm = Printf.sprintf "%s_%s" pn rn in
  let* lp = find_lp_name ~id in
  match lp with
  | None ->
      let* nm = fresh nm in
      let nm = LocalProtocolName.of_string (VariableName.user nm) in
      let* _ = put_lp_name ~key:id ~data:nm in
      pure nm
  | Some fn -> pure fn

let find_iface_type ~proto =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  match Map.find st.GoGenM.msg_iface proto with
  | None ->
      let* nm = fresh ("Msg" ^ ProtocolName.user proto) in
      let ifaces =
        Map.add_exn st.GoGenM.msg_iface ~key:proto ~data:(GoTyVar nm)
      in
      let* st = GoGenM.get in
      let* _ = GoGenM.put {st with GoGenM.msg_iface= ifaces} in
      pure (GoTyVar nm)
  | Some t -> pure t

let get_chan ~proto ~src ~dst =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ctx = st.GoGenM.lp_ctx in
  let chans = ctx.GoGenM.channels in
  match Map.find chans (proto, src, dst) with
  | None ->
      let* ch = fresh ("ch" ^ RoleName.user src ^ RoleName.user dst) in
      let* iface = find_iface_type ~proto in
      let ty = GoChan iface in
      let ctx' =
        { ctx with
          GoGenM.channels=
            Map.add_exn chans ~key:(proto, src, dst) ~data:(ch, ty) }
      in
      let* st = GoGenM.get in
      let* _ = GoGenM.put {st with GoGenM.lp_ctx= ctx'} in
      pure (ch, ty)
  | Some res -> pure res

(* Assumes context has been declared *)
let get_ctx_var ~proto ~role =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let lp_ctx = st.GoGenM.lp_ctx in
  pure
    (Map.find_exn lp_ctx.GoGenM.ctx_vars (LocalProtocolId.create proto role))

let mk_sr_callback ~proto ~role cb_pre lbl from =
  let open GoGenM.Syntax in
  let cb_nm =
    FunctionName.of_string
      (cb_pre ^ "_" ^ LabelName.user lbl ^ "_" ^ RoleName.user from)
  and cb_ty = GoTyVar (VariableName.of_string "<callback_type>") in
  let* ctx = get_ctx_var ~proto ~role in
  let key = LocalProtocolId.create proto role and data = (cb_nm, cb_ty) in
  let* st = GoGenM.get in
  let st =
    {st with GoGenM.callbacks= Map.add_multi st.GoGenM.callbacks ~key ~data}
  in
  let* _ = GoGenM.put st in
  pure (fun args -> GoMCall (ctx, cb_nm, args))

let mk_ch_callback cb_ty from =
  let open GoGenM.Syntax in
  pure (FunctionName.of_string (cb_ty ^ "_" ^ RoleName.user from ^ "_TODO"))

let mk_end_callback =
  let open GoGenM.Syntax in
  pure "End"

let get_label _ = "TODO_Label"

let get_protocol_call_label _ _ _ =
  let open GoGenM.Syntax in
  pure "TODO_Call_Label"

let get_acc_callback _ _ _ =
  let open GoGenM.Syntax in
  pure "TODO_accept_callback"

let get_post_acc_callback _ _ _ =
  let open GoGenM.Syntax in
  pure "TODO_post_accept_callback"

let get_proto_call r p =
  let open GoGenM.Syntax in
  let key = LocalProtocolId.create p r in
  let* lpn = get_lp_name ~id:key in
  pure (FunctionName.of_string @@ LocalProtocolName.to_capitalize_string lpn)

let mk_channels _ =
  let open GoGenM.Syntax in
  pure (VariableName.of_string "TODO_newchans", [])

(** Input: role, local type; Output: list of unique pairs of roles that
    require a channel in this local type *)
let rec required_channels ~role lty =
  let rec go = function
    | RecvL (_, src, cont) -> (role, src) :: go cont
    | SendL (_, dst, cont) -> (dst, role) :: go cont
    | ChoiceL (_, alts) ->
        List.fold ~init:[] ~f:List.append (List.map ~f:go alts)
    | TVarL _ -> []
    | MuL (_, _, k) -> go k
    | EndL -> []
    | InviteCreateL (dsts, _, _, k) ->
        List.map ~f:(fun dst -> (dst, role)) dsts @ go k
    | AcceptL (src, _, _, _, _, k) -> (role, src) :: go k
    | SilentL _ -> assert false
  in
  let cmp_pair (p1, q1) (p2, q2) =
    let cmp_fst = RoleName.compare p1 p2 in
    if cmp_fst = 0 then RoleName.compare q1 q2 else cmp_fst
  in
  List.dedup_and_sort ~compare:cmp_pair (go lty)

let fldname_of_payload ~i = function
  | PValue (None, _) -> VariableName.of_string (Printf.sprintf "p%d" i)
  | PValue (Some x, _) ->
      VariableName.of_string (String.uncapitalize (VariableName.user x))
  | PDelegate _ -> Err.violation ~here:[%here] "Delegation not supported"

let typename_of_payload = function
  | PValue (_, ty) ->
      GoTyVar
        (VariableName.of_string
           (PayloadTypeName.user (Expr.payload_typename_of_payload_type ty)) )
  | PDelegate _ -> Err.violation ~here:[%here] "Delegation not supported"

let mk_type_decl pl =
  match pl with
  | [] -> GoStruct []
  | [x] -> GoSyn (typename_of_payload x)
  | xs ->
      GoStruct
        (List.foldi xs ~init:[] ~f:(fun i acc x ->
             (fldname_of_payload ~i x, typename_of_payload x) :: acc ) )

let record_label_name ~proto lbl ty =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  match Map.find st.GoGenM.proto_lbls (proto, lbl) with
  | Some _ -> pure ()
  | None ->
      let ty_d = mk_type_decl ty in
      let proto_lbls =
        Map.add_exn st.GoGenM.proto_lbls ~key:(proto, lbl) ~data:ty_d
      in
      let st = {st with GoGenM.proto_lbls} in
      GoGenM.put st

let gen_local ~proto ~role lty =
  let open GoGenM.Syntax in
  let rec go pre go_impl lty =
    match lty with
    | RecvL ({label; payload= ty}, from, cont) ->
        let* var, k =
          match pre with
          | Some var -> pure (var, [])
          | None ->
              let* var = fresh "x" in
              let* chan, _ = get_chan ~proto ~src:role ~dst:from in
              let* _ = record_label_name ~proto label ty in
              let lbl =
                VariableName.of_string (LabelName.to_capitalize_string label)
              in
              let recv_stmt =
                GoAssign (var, GoAssert (GoRecv (GoVar chan), lbl))
              in
              pure (var, [recv_stmt])
        in
        let* cb_recv = mk_sr_callback ~proto ~role "Recv" label from in
        go None ((GoExpr (cb_recv [GoVar var]) :: k) @ go_impl) cont
    | SendL ({label; _}, dst, cont) ->
        let* var, k =
          match pre with
          | Some var -> pure (var, [])
          | None ->
              let* cb_nm = mk_sr_callback ~proto ~role "Send" label dst in
              let* var = fresh "x" in
              let send_callback = GoAssign (var, cb_nm []) in
              pure (var, [send_callback])
        in
        let* chan, _ = get_chan ~proto ~src:dst ~dst:role in
        let send_stmt = GoSend (GoVar chan, GoVar var) in
        go None ((send_stmt :: k) @ go_impl) cont
    | ChoiceL (who, alts) ->
        if RoleName.equal role who then
          let* cb_nm = mk_ch_callback "Choice" who in
          let* var_x = fresh "x" in
          let* var_v = fresh "v" in
          let* conts = go_alt var_v alts in
          let choice_stmt =
            GoSwitch (GoAssign (var_v, GoTypeOf (GoVar var_x)), conts)
          and callback = GoAssign (var_x, GoCall (cb_nm, [])) in
          pure ([choice_stmt; callback] @ go_impl)
        else
          let* chan, _ = get_chan ~proto ~src:role ~dst:who in
          let* var = fresh "x" in
          let recv_stmt = GoAssign (var, GoRecv (GoVar chan)) in
          let* var_v = fresh "v" in
          let* conts = go_alt var_v alts in
          let choice_stmt =
            GoSwitch (GoAssign (var_v, GoTypeOf (GoVar var)), conts)
          in
          pure ([choice_stmt; recv_stmt] @ go_impl)
    | TVarL (jump_to, _exprs) ->
        let rv = LabelName.of_string (TypeVariableName.user jump_to) in
        pure (GoContinue (Some rv) :: go_impl)
    | MuL (var, _rvars, cont) ->
        let* body = go pre [] cont in
        let rv = LabelName.of_string (TypeVariableName.user var) in
        pure (GoFor (GoSeq (List.rev body)) :: GoLabel rv :: go_impl)
    | EndL ->
        let* cb_nm = mk_end_callback in
        pure (GoReturn (GoCall (FunctionName.of_string cb_nm, [])) :: go_impl)
    | InviteCreateL (roles, new_roles, proto, cont) ->
        let* lbl = get_protocol_call_label roles new_roles proto in
        let* invitations = go_invite [] lbl proto roles in
        let* new_goroutines = go_create [] proto new_roles in
        go None (new_goroutines @ invitations @ go_impl) cont
    | AcceptL (from, proto, roles, new_roles, who, cont) ->
        (* What is the label for this protocol call? *)
        let* lbl = get_protocol_call_label roles new_roles proto in
        (* Get channel from sender and receive protocol channels *)
        let* chan, _ = get_chan ~proto ~src:role ~dst:from in
        let* var = fresh "x" in
        let recv_stmt =
          GoAssign
            (var, GoAssert (GoRecv (GoVar chan), VariableName.of_string lbl))
        in
        (* get accept callback (from this role's context to who's context *)
        let* cb_nm = get_acc_callback from who proto in
        let* ctx = fresh "ctx" in
        let callback =
          GoAssign (ctx, GoCall (FunctionName.of_string cb_nm, []))
        in
        (* get actual protocol call *)
        let* call_proto = get_proto_call who proto in
        let* ctx' = fresh "ctx" in
        let call_stmt =
          GoAssign (ctx', GoCall (call_proto, [GoVar ctx; GoVar var]))
        in
        let* cb_nm = get_post_acc_callback from who proto in
        let callback' =
          GoExpr (GoCall (FunctionName.of_string cb_nm, [GoVar ctx']))
        in
        go None
          (callback' :: call_stmt :: callback :: recv_stmt :: go_impl)
          cont
    | SilentL _ -> assert false
  and go_alt var = function
    | [] -> pure []
    | x :: xs ->
        let* cont = go (Some var) [] x in
        let* alts = go_alt var xs in
        pure
          ( ( GoVar (VariableName.of_string (get_label x))
            , GoSeq (List.rev cont) )
          :: alts )
  and go_invite acc lbl next_proto = function
    | [] -> pure acc
    | r :: rs ->
        let* ch_name, chans = mk_channels next_proto in
        let* chan, _ = get_chan ~proto ~src:role ~dst:r in
        let go_inv = GoSend (GoVar chan, GoVar ch_name) in
        go_invite ((go_inv :: chans) @ acc) lbl next_proto rs
  and go_create acc next_proto = function
    | [] -> pure acc
    | r :: rs ->
        let* ch_name, chans = mk_channels next_proto in
        let* cb_nm = get_acc_callback role r proto in
        let* ctx = fresh "ctx" in
        let callback =
          GoAssign (ctx, GoCall (FunctionName.of_string cb_nm, []))
        in
        (* get actual protocol call *)
        let* call_proto = get_proto_call r next_proto in
        let call_stmt =
          GoSpawn (GoCall (call_proto, [GoVar ctx; GoVar ch_name]))
        in
        go_create ((call_stmt :: callback :: chans) @ acc) next_proto rs
  in
  GoGenM.map (fun l -> GoSeq (List.rev l)) (go None [] lty)

let rec decl_channels ~proto lty =
  let open GoGenM.Syntax in
  let rec go lty =
    match lty with
    | [] -> pure []
    | (f, t) :: rs ->
        let* ch = get_chan ~proto ~src:f ~dst:t in
        let* chs = go rs in
        pure (ch :: chs)
  in
  let* chs = go lty in
  match chs with
  | [] -> pure []
  | (_, t) :: _ -> pure [(List.map ~f:fst chs, t)]

let enter_decl ~proto ~role =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ctx = st.GoGenM.lp_ctx in
  GoGenM.put
    { st with
      GoGenM.lp_ctx=
        {ctx with GoGenM.curr_fn= Some (LocalProtocolId.create proto role)}
    }

let get_ctx_type ~proto ~role =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  match Map.find st.GoGenM.ctx_type (LocalProtocolId.create proto role) with
  | Some ty -> pure ty
  | None ->
      let* ctx_ty =
        fresh ("Ctx_" ^ ProtocolName.user proto ^ "_" ^ RoleName.user role)
      in
      let ctx_ty = GoTyVar ctx_ty in
      let* st = GoGenM.get in
      let st =
        { st with
          GoGenM.ctx_type=
            Map.add_exn st.GoGenM.ctx_type
              ~key:(LocalProtocolId.create proto role)
              ~data:ctx_ty }
      in
      let* _ = GoGenM.put st in
      pure ctx_ty

let new_ctx ~proto ~role =
  let open GoGenM.Syntax in
  let* ctx_ty = get_ctx_type ~proto ~role in
  let* ctx_var = fresh "ctx" in
  let* st = GoGenM.get in
  let lp_ctx = st.GoGenM.lp_ctx in
  let lp_ctx =
    { lp_ctx with
      GoGenM.ctx_vars=
        Map.add_exn lp_ctx.GoGenM.ctx_vars
          ~key:(LocalProtocolId.create proto role)
          ~data:ctx_var }
  in
  let st = {st with GoGenM.lp_ctx} in
  let* _ = GoGenM.put st in
  pure (ctx_var, ctx_ty)

(* Requires to pre-record required protocol channels *)
let get_req_chans ~proto ~role =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  pure (Map.find_exn st.GoGenM.req_chans (LocalProtocolId.create proto role))

let gen_decl_body ~proto ~role lty =
  let open GoGenM.Syntax in
  let* _ = GoGenM.cleanup in
  let* _ = enter_decl ~proto ~role in
  let* chans = get_req_chans ~proto ~role in
  let* chs = decl_channels ~proto chans in
  let* ctx_var, ctx_ty = new_ctx ~proto ~role in
  let* clty = gen_local ~proto ~role lty in
  let* _ = GoGenM.cleanup in
  pure (([ctx_var], ctx_ty) :: chs, ctx_ty, clty)

let gen_local_func ~key ~data:(_roles, lty) cgen =
  let open GoGenM.Syntax in
  let* acc = cgen in
  let* args, r_ty, clty =
    gen_decl_body
      ~proto:(LocalProtocolId.get_protocol key)
      ~role:(LocalProtocolId.get_role key)
      lty
  in
  let* lpn = get_lp_name ~id:key in
  let fnm =
    FunctionName.of_string @@ LocalProtocolName.to_capitalize_string lpn
  in
  (* Generate return type for [key] in callbacks/key *)
  let fdecl = GoFunc (fnm, args, Some r_ty, clty) in
  (* let function_impl = function_decl impl_function params return_type impl
     in *)
  (* (env, function_impl) *)
  pure (fdecl :: acc)

let reg_req_chans ~key ~data:(_roles, lty) acc =
  let role = LocalProtocolId.get_role key in
  let chans = required_channels ~role lty in
  let open GoGenM.Syntax in
  let* _ = acc in
  let* st = GoGenM.get in
  GoGenM.put
    { st with
      GoGenM.req_chans= Map.add_exn st.GoGenM.req_chans ~key ~data:chans }

let gen_code_alt _root_dir _gen_protocol _global_t local_t =
  let open GoGenM.Syntax in
  let genf =
    let* _ = Map.fold local_t ~init:(pure ()) ~f:reg_req_chans in
    Map.fold local_t ~init:(pure []) ~f:gen_local_func
  in
  ppr_prog (GoGenM.eval genf GoGenM.init)

let generate_from_scr ast protocol root_dir =
  let global_t = nested_t_of_module ast in
  ensure_unique_identifiers global_t ;
  let local_t = Ltype.project_nested_t global_t in
  let local_t = Ltype.ensure_unique_tvars local_t in
  gen_code_alt root_dir protocol global_t local_t

let error_no_global_protocol () =
  Err.uerr
    (Err.InvalidCommandLineParam
       "Cannot find entrypoint global protocol in input file." )

let find_global_protocol ast =
  let open Syntax in
  match List.rev ast.protocols with
  | [] -> error_no_global_protocol ()
  | x :: _ -> x.Loc.value.name

let generate_go_code ast ~out_dir ~go_path:_ =
  let protocol = find_global_protocol ast in
  let protocol_pkg = PackageName.protocol_pkg_name protocol in
  let root_dir =
    RootDirName.of_string
    @@ Printf.sprintf "%s/%s" out_dir (PackageName.user protocol_pkg)
  in
  generate_from_scr ast protocol root_dir
(* if Option.is_some go_path then write_code_to_files result
   (Option.value_exn go_path) out_dir protocol; *)
(* show_codegen_result result protocol root_dir *)
