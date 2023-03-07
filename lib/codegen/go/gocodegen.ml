open! Base
open Ltype
open Goimpl
open Goenvs
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
      | CombineG (g1, g2) ->
          let m1 = validate_protocol_msgs messages g1 in
          validate_protocol_msgs m1 g2
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

let buffer_size = 1

let get_lp_name ~id =
  let open GoGenM.Syntax in
  let pn = LocalProtocolId.get_protocol id |> ProtocolName.user in
  let rn = LocalProtocolId.get_role id |> RoleName.user in
  let nm = Printf.sprintf "%s_%s" pn rn in
  let* lp = find_lp_name ~id in
  match lp with
  | None ->
      let* nm = fresh nm in
      let nm = FunctionName.of_string (VariableName.user nm) in
      let* _ = put_lp_name ~key:id ~data:nm in
      pure nm
  | Some fn -> pure fn

let find_iface_type ~proto =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  match Map.find st.GoGenM.msg_iface proto with
  | None ->
      let* nm = fresh ("Msg" ^ ProtocolName.user proto) in
      let ifaces = Map.add_exn st.GoGenM.msg_iface ~key:proto ~data:nm in
      let* st = GoGenM.get in
      let* _ = GoGenM.put {st with GoGenM.msg_iface= ifaces} in
      pure (GoTyVar nm)
  | Some t -> pure (GoTyVar t)

let get_chan ~proto ~src ~dst =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ctx = st.GoGenM.lp_ctx in
  let chans = ctx.GoGenM.channels in
  match Map.find chans (proto, src, dst) with
  | None ->
      let* ch =
        fresh ("ch_" ^ RoleName.user src ^ "_" ^ RoleName.user dst)
      in
      let* iface = find_iface_type ~proto in
      let ty = GoChan iface in
      let ctx' =
        { ctx with
          GoGenM.channels=
            Map.add_exn chans ~key:(proto, src, dst) ~data:(ch, ty) }
      in
      let* st = GoGenM.get in
      let* _ = GoGenM.put {st with GoGenM.lp_ctx= ctx'} in
      pure (true, (ch, ty))
  | Some (ch, ty) -> pure (false, (ch, ty))

let chan_fld_name f t =
  VariableName.of_string ("ch_" ^ RoleName.user f ^ "_" ^ RoleName.user t)

let get_channel ~proto ~src ~dst =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  match st.GoGenM.lp_ctx.GoGenM.in_call with
  | None ->
      let* _, (chn, _) = get_chan ~proto ~src ~dst in
      pure (GoVar chn)
  | Some (var, b) ->
      let fld = chan_fld_name src dst in
      if b then pure (GoStructProj (GoVar var, fld)) else pure (GoVar var)

let get_new_chan ~proto ~src ~dst =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ctx = st.GoGenM.lp_ctx in
  let chans = ctx.GoGenM.new_channels in
  match Map.find chans (proto, src, dst) with
  | None ->
      let* ch =
        fresh ("ch_" ^ RoleName.user src ^ "_" ^ RoleName.user dst)
      in
      let* iface = find_iface_type ~proto in
      let ty = GoChan iface in
      let ctx' =
        { ctx with
          GoGenM.new_channels=
            Map.add_exn chans ~key:(proto, src, dst) ~data:(ch, ty) }
      in
      let* st = GoGenM.get in
      let* _ = GoGenM.put {st with GoGenM.lp_ctx= ctx'} in
      pure (true, (ch, ty))
  | Some res -> pure (false, res)

let reset_new_chans =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ctx = st.GoGenM.lp_ctx in
  GoGenM.put
    { st with
      GoGenM.lp_ctx=
        {ctx with GoGenM.new_channels= Map.empty (module RolePair)} }

(* Assumes context has been declared *)
let get_ctx_var ~proto ~role =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let lp_ctx = st.GoGenM.lp_ctx in
  pure
    (Map.find_exn lp_ctx.GoGenM.ctx_vars (LocalProtocolId.create proto role))

let mk_callback_name ~proto cb_pre lbl from =
  let open GoGenM.Syntax in
  let* cb_nm =
    fresh
      ( cb_pre ^ "_" ^ RoleName.user from ^ "_" ^ ProtocolName.user proto
      ^ "_" ^ LabelName.user lbl )
  in
  pure (FunctionName.of_string (VariableName.user cb_nm))

let mk_sr_callback ~proto ~role ~arg_ty ~ret_ty ~cb_nm =
  let open GoGenM.Syntax in
  let cb_ty = GoFunTy (arg_ty, ret_ty) in
  let* ctx = get_ctx_var ~proto ~role in
  let key = LocalProtocolId.create proto role and data = (cb_nm, cb_ty) in
  let* st = GoGenM.get in
  let st =
    {st with GoGenM.callbacks= Map.add_multi st.GoGenM.callbacks ~key ~data}
  in
  let* _ = GoGenM.put st in
  pure (fun args -> GoMCall (ctx, cb_nm, args))

let mk_end_callback ~proto ~role =
  let open GoGenM.Syntax in
  let cb_nm = FunctionName.of_string "End" in
  let* ctx = get_ctx_var ~proto ~role in
  pure (GoMCall (ctx, cb_nm, []))

let get_protocol_call_label c = function
  | who -> (
      let open GoGenM.Syntax in
      let* st = GoGenM.get in
      match Map.find st.GoGenM.call_lbls (who, c) with
      | Some l -> pure l
      | None ->
          let lbl =
            LabelName.of_string
              ("Call_" ^ RoleName.user who ^ "_" ^ ProtocolName.user c)
          in
          let* st = GoGenM.get in
          let st =
            { st with
              GoGenM.call_lbls=
                Map.add_exn st.GoGenM.call_lbls ~key:(who, c) ~data:lbl }
          in
          let* _ = GoGenM.put st in
          pure lbl )

let get_invite_label c = function
  | who -> (
      let open GoGenM.Syntax in
      let* st = GoGenM.get in
      match Map.find st.GoGenM.invite_lbls (who, c) with
      | Some l -> pure l
      | None ->
          let lbl =
            LabelName.of_string
              ("Invite_" ^ RoleName.user who ^ "_" ^ ProtocolName.user c)
          in
          let* st = GoGenM.get in
          let st =
            { st with
              GoGenM.invite_lbls=
                Map.add_exn st.GoGenM.invite_lbls ~key:(who, c) ~data:lbl }
          in
          let* _ = GoGenM.put st in
          pure lbl )

let record_label_name ~proto lbl ty_d =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  match Map.find st.GoGenM.proto_lbls (proto, lbl) with
  | Some _ -> pure ()
  | None ->
      let proto_lbls =
        Map.add_exn st.GoGenM.proto_lbls ~key:(proto, lbl) ~data:ty_d
      in
      let st = {st with GoGenM.proto_lbls} in
      GoGenM.put st

let typename_of_payload = function
  | PValue (_, ty) ->
      GoTyVar
        (VariableName.of_string
           (PayloadTypeName.user (Expr.payload_typename_of_payload_type ty)) )
  | PDelegate _ -> Err.violation ~here:[%here] "Delegation not supported"

let fldname_of_payload ~i = function
  | PValue (None, _) -> VariableName.of_string (Printf.sprintf "P%d" i)
  | PValue (Some x, _) ->
      VariableName.of_string (String.capitalize (VariableName.user x))
  | PDelegate _ -> Err.violation ~here:[%here] "Delegation not supported"

let mk_type_decl pl =
  match pl with
  | [] -> GoStruct []
  | [x] -> GoTyDef (typename_of_payload x)
  | xs ->
      GoStruct
        (List.foldi xs ~init:[] ~f:(fun i acc x ->
             (fldname_of_payload ~i x, typename_of_payload x) :: acc ) )

let rec get_choice_label ~proto = function
  | RecvL ({label; payload= ty}, _, _) ->
      let open GoGenM.Syntax in
      let* _ = record_label_name ~proto label (mk_type_decl ty) in
      GoGenM.Syntax.pure label
  | SendL ({label; payload= ty}, _, _) ->
      let open GoGenM.Syntax in
      let* _ = record_label_name ~proto label (mk_type_decl ty) in
      GoGenM.Syntax.pure label
  | MuL (_, _, k) -> get_choice_label ~proto k
  | InviteCreateL (_, _, proto, _) -> (
      let open GoGenM.Syntax in
      let* st = GoGenM.get in
      let roles, new_roles = Map.find_exn st.GoGenM.role_args proto in
      match (roles, new_roles) with
      | role :: _, _ -> get_invite_label proto role
      | [], role :: _ -> get_invite_label proto role
      | _, _ ->
          Err.violation ~here:[%here] "Panic! Empty protocol definition" )
  | AcceptL (who, proto, _, _, _, _) -> get_protocol_call_label proto who
  | ChoiceL (_, _) ->
      Err.violation ~here:[%here] "Panic! Nested choices are not supported."
  | _ ->
      Err.violation ~here:[%here]
        "Panic! The continuation of  choice alternatives must be \
         interactions."

let get_proto_call r p =
  let open GoGenM.Syntax in
  let key = LocalProtocolId.create p r in
  let* lpn = get_lp_name ~id:key in
  pure (FunctionName.capitalize lpn)

let get_protocol_args proto =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  pure (Map.find_exn st.GoGenM.role_args proto)

let mk_channels proto role =
  let open GoGenM.Syntax in
  let* _, nrs = get_protocol_args proto in
  let rec mk_chs flds acc is = function
    | [] -> pure (List.rev flds, List.rev acc, List.rev is)
    | (f, t) :: cs ->
        let fld_name = chan_fld_name f t in
        if
          List.mem ~equal:RoleName.equal nrs f
          || List.mem ~equal:RoleName.equal nrs t
        then
          let* is_new, (c, ty) = get_new_chan ~proto ~src:f ~dst:t in
          if is_new then
            mk_chs ((fld_name, ty) :: flds) (c :: acc)
              (GoAssignNew (c, GoMake (ty, buffer_size)) :: is)
              cs
          else mk_chs ((fld_name, ty) :: flds) (c :: acc) is cs
        else
          let* is_new, (c, ty) = get_chan ~proto ~src:f ~dst:t in
          if is_new then
            mk_chs ((fld_name, ty) :: flds) (c :: acc)
              (GoAssignNew (c, GoMake (ty, buffer_size)) :: is)
              cs
          else mk_chs ((fld_name, ty) :: flds) (c :: acc) is cs
  in
  let* st = GoGenM.get in
  let cs =
    Map.find_exn st.GoGenM.req_chans (LocalProtocolId.create proto role)
  in
  mk_chs [] [] [] cs

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
    | AcceptL (_, _, _, _, src, k) -> (role, src) :: go k
    | CallL (_, _, _, _, k) -> go k
    | ICallL (_, _, _, _, _) ->
        []
        (* Inlined calls only happen after "accept", so their channels are
           not required*)
    | CombineL _ ->
        Err.unimpl ~here:[%here]
          "Cannot generate code for local types with\n  CombineL"
    | SilentL _ -> assert false
  in
  let cmp_pair (p1, q1) (p2, q2) =
    let cmp_fst = RoleName.compare p1 p2 in
    if cmp_fst = 0 then RoleName.compare q1 q2 else cmp_fst
  in
  List.dedup_and_sort ~compare:cmp_pair (go lty)

let chan_struct pl =
  match pl with [x] -> GoTyDef (snd x) | xs -> GoStruct xs

let lbl_to_var x = VariableName.of_string (LabelName.user x)

let init_chan_struct lbl = function
  | [x] -> GoCast (GoTyVar (lbl_to_var lbl), GoVar x)
  | xs -> GoStructLit (lbl_to_var lbl, List.map ~f:(fun x -> GoVar x) xs)

let get_ctx_type ~proto ~role =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  match Map.find st.GoGenM.ctx_type (LocalProtocolId.create proto role) with
  | Some ty -> pure (GoTyVar ty)
  | None ->
      let* ctx_ty =
        fresh ("Ctx_" ^ ProtocolName.user proto ^ "_" ^ RoleName.user role)
      in
      let* st = GoGenM.get in
      let st =
        { st with
          GoGenM.ctx_type=
            Map.add_exn st.GoGenM.ctx_type
              ~key:(LocalProtocolId.create proto role)
              ~data:ctx_ty }
      in
      let* _ = GoGenM.put st in
      pure (GoTyVar ctx_ty)

(* Requires to pre-record required protocol channels *)
let get_req_chans ~proto ~role =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  pure (Map.find_exn st.GoGenM.req_chans (LocalProtocolId.create proto role))

let proj_chans var cs =
  let rec go = function
    | [] -> []
    | (f, t) :: xs ->
        let fld = chan_fld_name f t in
        GoStructProj (GoVar var, fld) :: go xs
  in
  match cs with [_] -> [GoVar var] | _ -> go cs

let reassign_channels proto var req_chans =
  let open GoGenM.Syntax in
  let rec go = function
    | [] -> pure []
    | (f, t) :: xs ->
        let fld = chan_fld_name f t in
        let* stmts = go xs in
        let* _, (ch, _) = get_chan ~proto ~src:f ~dst:t in
        pure (GoAssign (ch, GoStructProj (GoVar var, fld)) :: stmts)
  in
  match req_chans with
  | [(f, t)] ->
      let* _, (ch, _) = get_chan ~proto ~src:f ~dst:t in
      pure [GoAssign (ch, GoVar var)]
  | _ -> go req_chans
(* let rec go = function | [] -> pure [] | (f, t) :: xs -> let fld =
   chan_fld_name f t in let* _ (ch, _) = get_chan ~proto ~role f t in (* pure
   (GoAssignNew (ch, GoStructProj (GoVar var, fld)) :: go xs) *) pure [] in
   go req_chans *)

let store_proto_chan key v =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ctx = st.GoGenM.lp_ctx in
  GoGenM.put
    { st with
      GoGenM.lp_ctx=
        { ctx with
          GoGenM.call_chans=
            Map.update ctx.GoGenM.call_chans key ~f:(fun _ -> v) } }

let enter_iproto key =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ctx = st.GoGenM.lp_ctx in
  let vv = Map.find_exn ctx.GoGenM.call_chans key in
  GoGenM.put {st with GoGenM.lp_ctx= {ctx with GoGenM.in_call= Some vv}}

let leave_iproto =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ctx = st.GoGenM.lp_ctx in
  GoGenM.put {st with GoGenM.lp_ctx= {ctx with GoGenM.in_call= None}}

let gen_local ~wg ~proto ~role lty =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let _, nrs = Map.find_exn st.GoGenM.role_args proto in
  let defer =
    if List.mem ~equal:RoleName.equal nrs role then
      [GoDefer (GoMCall (wg, FunctionName.of_string "Done", []))]
    else []
  in
  let rec go pre go_impl lty =
    match lty with
    | RecvL ({label; payload= ty}, from, cont) ->
        let lbl =
          VariableName.of_string (LabelName.to_capitalize_string label)
        in
        let* var, k =
          match pre with
          | Some var -> pure (var, [])
          | None ->
              let* var = fresh "x" in
              let* chan = get_channel ~proto ~src:role ~dst:from in
              let* _ = record_label_name ~proto label (mk_type_decl ty) in
              let recv_stmt =
                GoAssignNew (var, GoAssert (GoRecv chan, GoTyVar lbl))
              in
              pure (var, [recv_stmt])
        in
        let* cb_nm = mk_callback_name ~proto "Recv" label from in
        let* cb_recv =
          mk_sr_callback ~proto ~role ~cb_nm
            ~arg_ty:[([var], GoTyVar lbl)]
            ~ret_ty:None
        in
        go None ((GoExpr (cb_recv [GoVar var]) :: k) @ go_impl) cont
    | SendL ({label; payload= ty}, dst, cont) ->
        let lbl =
          VariableName.of_string (LabelName.to_capitalize_string label)
        in
        let* var, k =
          match pre with
          | Some var -> pure (var, [])
          | None ->
              let* cb_nm = mk_callback_name ~proto "Send" label dst in
              let* cb =
                mk_sr_callback ~proto ~role ~arg_ty:[]
                  ~ret_ty:(Some (GoTyVar lbl)) ~cb_nm
              in
              let* var = fresh "x" in
              let send_callback = GoAssignNew (var, cb []) in
              pure (var, [send_callback])
        in
        let* chan = get_channel ~proto ~src:dst ~dst:role in
        let* _ = record_label_name ~proto label (mk_type_decl ty) in
        let send_stmt = GoSend (chan, GoVar var) in
        go None ((send_stmt :: k) @ go_impl) cont
    | ChoiceL (who, alts) ->
        if RoleName.equal role who then
          let* ctx_ty = fresh ("Select_" ^ RoleName.user who) in
          let* cb_nm =
            mk_callback_name ~proto "Choice" (LabelName.of_string "") who
          in
          let* cb =
            mk_sr_callback ~proto ~role ~arg_ty:[]
              ~ret_ty:(Some (GoTyVar ctx_ty)) ~cb_nm
          in
          let* var_x = fresh "x" in
          let* var_v = fresh "v" in
          let* conts = go_select ctx_ty var_v alts in
          let choice_stmt =
            GoSwitch (GoAssignNew (var_v, GoTypeOf (GoVar var_x)), conts)
          and callback = GoAssignNew (var_x, cb []) in
          pure ([choice_stmt; callback] @ go_impl)
        else
          let* chan = get_channel ~proto ~src:role ~dst:who in
          let* var = fresh "x" in
          let recv_stmt = GoAssignNew (var, GoRecv chan) in
          let* var_v = fresh "v" in
          let* conts = go_branch var_v alts in
          let choice_stmt =
            GoSwitch (GoAssignNew (var_v, GoTypeOf (GoVar var)), conts)
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
        let* cb = mk_end_callback ~proto ~role in
        pure (GoReturn None :: GoExpr cb :: go_impl)
    | InviteCreateL (roles, new_roles, proto, cont) ->
        let fn_lbl = get_protocol_call_label proto in
        let* rns, _ = get_protocol_args proto in
        let* invitations = go_invite [] fn_lbl proto rns roles in
        let* new_goroutines = go_create [] proto new_roles in
        let* _ = reset_new_chans in
        go None (new_goroutines @ invitations @ go_impl) cont
    | AcceptL (who, new_proto, _, _, from, cont) ->
        (* What is the label for this protocol call? *)
        let* var, stmt =
          match pre with
          | None ->
              let* lbl = get_protocol_call_label new_proto who in
              (* Get channel from sender and receive protocol channels *)
              let* chan = get_channel ~proto ~src:role ~dst:from in
              let* var = fresh "x" in
              let recv_stmt =
                GoAssignNew
                  ( var
                  , GoAssert
                      ( GoRecv chan
                      , GoTyVar (VariableName.of_string (LabelName.user lbl))
                      ) )
              in
              pure (var, [recv_stmt])
          | Some var -> pure (var, [])
        in
        let* req_chans = get_req_chans ~proto:new_proto ~role:who in
        let is_struct = match req_chans with [_] -> false | _ -> true in
        let key = LocalProtocolId.create new_proto who in
        let* _ = store_proto_chan key (var, is_struct) in
        go (Some var) (stmt @ go_impl) cont
    | CallL (who, new_proto, _, _, cont) ->
        (* get accept callback (from this role's context to who's context *)
        (* If it's a recursive call, generate it in tail position TODO:
           optimize tail recursion to loops *)
        let var =
          match pre with
          | Some v -> v
          | None ->
              Err.uerr
                (Err.Uncategorised
                   "Impossible: protocol call did not receive session!" )
        in
        if RoleName.equal role who && ProtocolName.equal proto new_proto then
          let* ctx = get_ctx_var ~proto ~role in
          let* req_chans = get_req_chans ~proto:new_proto ~role:who in
          let* call_proto = get_proto_call who new_proto in
          match cont with
          | EndL ->
              let call_stmt =
                GoGoto (LabelName.of_string (FunctionName.user call_proto))
              in
              let* assign = reassign_channels proto var req_chans in
              let* _ = put_tail_rec in
              pure ((call_stmt :: assign) @ go_impl)
          | _ ->
              let call_stmt =
                GoExpr
                  (GoCall
                     ( GoFnVar call_proto
                     , GoVar ctx :: GoVar wg :: proj_chans var req_chans ) )
              in
              go None (call_stmt :: go_impl) cont
        else
          let* ctx_ty = get_ctx_type ~role:who ~proto:new_proto in
          let* cb_nm =
            mk_callback_name ~proto:new_proto "Init"
              (LabelName.of_string "Ctx")
              who
          in
          let* cb =
            mk_sr_callback ~proto ~role ~arg_ty:[] ~ret_ty:(Some ctx_ty)
              ~cb_nm
          in
          let* ctx = fresh "ctx" in
          let callback = GoAssignNew (ctx, cb []) in
          (* get actual protocol call *)
          let* call_proto = get_proto_call who new_proto in
          let* req_chans = get_req_chans ~proto:new_proto ~role:who in
          let call_stmt =
            GoExpr
              (GoCall
                 ( GoFnVar call_proto
                 , GoVar ctx :: GoVar wg :: proj_chans var req_chans ) )
          in
          let* cb_nm =
            mk_callback_name ~proto:new_proto "End"
              (LabelName.of_string "Ctx")
              who
          in
          let* cb =
            mk_sr_callback ~proto ~role
              ~arg_ty:[([ctx], ctx_ty)]
              ~ret_ty:None ~cb_nm
          in
          let callback' = GoExpr (cb [GoVar ctx]) in
          go None (callback' :: call_stmt :: callback :: go_impl) cont
    | ICallL (who, new_proto, _, _, cont) ->
        let* _ = enter_iproto (LocalProtocolId.create new_proto who) in
        let* res = go pre go_impl cont in
        let* _ = leave_iproto in
        pure res
    | CombineL _ ->
        Err.unimpl ~here:[%here]
          "Cannot generate code from local types containing\n\
          \        combination operators"
    | SilentL _ -> assert false
  and go_select ifc var = function
    | [] -> pure []
    | x :: xs ->
        let* st = GoGenM.get in
        let lp_ctx = st.GoGenM.lp_ctx in
        let* cont = go (Some var) [] x in
        let* st = GoGenM.get in
        let* _ = GoGenM.put {st with GoGenM.lp_ctx} in
        let* alts = go_select ifc var xs in
        let* lbl = get_choice_label ~proto x in
        let* st = GoGenM.get in
        let* _ =
          GoGenM.put
            { st with
              GoGenM.choice_iface=
                Map.add_multi st.GoGenM.choice_iface ~key:ifc ~data:lbl }
        in
        let* _ = record_label_name ~proto lbl (GoStruct []) in
        let lbl =
          VariableName.of_string (LabelName.to_capitalize_string lbl)
        in
        pure ((GoVar lbl, GoSeq (List.rev cont)) :: alts)
  and go_branch var = function
    | [] -> pure []
    | x :: xs ->
        let* st = GoGenM.get in
        let lp_ctx = st.GoGenM.lp_ctx in
        let* cont = go (Some var) [] x in
        let* st = GoGenM.get in
        let* _ = GoGenM.put {st with GoGenM.lp_ctx} in
        let* alts = go_branch var xs in
        let* lbl = get_choice_label ~proto x in
        let lbl =
          VariableName.of_string (LabelName.to_capitalize_string lbl)
        in
        pure ((GoVar lbl, GoSeq (List.rev cont)) :: alts)
  and go_invite acc fn_lbl next_proto rns roles =
    match (rns, roles) with
    | [], [] -> pure acc
    | ra :: ras, r :: rs ->
        let* flds, chs, chans = mk_channels next_proto ra in
        let* lbl = fn_lbl ra in
        let* _ = record_label_name ~proto lbl (chan_struct flds) in
        let* chan = get_channel ~proto ~src:r ~dst:role in
        let go_inv = GoSend (chan, init_chan_struct lbl chs) in
        go_invite ((go_inv :: chans) @ acc) fn_lbl next_proto ras rs
    | _, _ ->
        Err.violation ~here:[%here]
          ("Partial call to protocol '" ^ ProtocolName.user next_proto ^ "'")
  and go_create acc next_proto = function
    | [] -> pure acc
    | r :: rs ->
        let* _, ch_name, chans = mk_channels next_proto r in
        let* ctx_ty = get_ctx_type ~role:r ~proto:next_proto in
        let* cb_nm =
          mk_callback_name ~proto:next_proto "Init"
            (LabelName.of_string "Ctx")
            r
        in
        let* cb =
          mk_sr_callback ~proto ~role ~arg_ty:[] ~ret_ty:(Some ctx_ty) ~cb_nm
        in
        let* ctx = fresh "ctx" in
        let callback = GoAssignNew (ctx, cb []) in
        (* get actual protocol call *)
        let* call_proto = get_proto_call r next_proto in
        let call_stmt =
          GoSpawn
            (GoCall
               ( GoFnVar call_proto
               , GoVar ctx :: GoVar wg
                 :: List.map ~f:(fun x -> GoVar x) ch_name ) )
        in
        let wait =
          GoExpr (GoMCall (wg, FunctionName.of_string "Add", [GoIntLit 1]))
        in
        go_create
          ((call_stmt :: wait :: callback :: chans) @ acc)
          next_proto rs
  in
  GoGenM.map (fun l -> GoSeq (defer @ List.rev l)) (go None [] lty)

let rec decl_channels ~proto lty =
  let open GoGenM.Syntax in
  let rec go lty =
    match lty with
    | [] -> pure []
    | (f, t) :: rs ->
        let* _, ch = get_chan ~proto ~src:f ~dst:t in
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

let gen_decl_body ~proto ~role ~wg lty =
  let open GoGenM.Syntax in
  let* _ = GoGenM.cleanup in
  let* _ = enter_decl ~proto ~role in
  let* _ = find_iface_type ~proto in
  let* chans = get_req_chans ~proto ~role in
  let* chs = decl_channels ~proto chans in
  let* ctx_var, ctx_ty = new_ctx ~proto ~role in
  let* clty = gen_local ~wg ~proto ~role lty in
  let* st = GoGenM.get in
  let* _ = GoGenM.cleanup in
  pure
    ( st.GoGenM.tail_rec
    , ([ctx_var], ctx_ty) :: ([wg], GoPtr GoWaitGroup) :: chs
    , clty )

let gen_local_func ~wg ~key ~data:(_roles, lty) cgen =
  let open GoGenM.Syntax in
  let* acc = cgen in
  let* st = GoGenM.get in
  let st =
    {st with GoGenM.callbacks= Map.add_exn st.GoGenM.callbacks ~key ~data:[]}
  in
  let* _ = GoGenM.put st in
  let* is_tail_rec, args, clty =
    gen_decl_body ~wg
      ~proto:(LocalProtocolId.get_protocol key)
      ~role:(LocalProtocolId.get_role key)
      lty
  in
  let* lpn = get_lp_name ~id:key in
  let fnm = FunctionName.capitalize lpn in
  let fnm_l = LabelName.of_string (FunctionName.user fnm) in
  let lbl = if is_tail_rec then [GoLabel fnm_l] else [] in
  let fdecl = GoFunc (None, fnm, args, None, GoSeq (lbl @ [clty])) in
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

let reg_role_args ~key ~data:{static_roles= rs; dynamic_roles= n_rs; _} acc =
  let open GoGenM.Syntax in
  let* _ = acc in
  let* st = GoGenM.get in
  GoGenM.put
    { st with
      GoGenM.role_args= Map.add_exn st.GoGenM.role_args ~key ~data:(rs, n_rs)
    }

let mk_proto_ifaces ~key ~data:_ acc =
  let open GoGenM.Syntax in
  let* acc = acc in
  let* st = GoGenM.get in
  let ty = Map.find_exn st.GoGenM.msg_iface key in
  let fld_nm = FunctionName.of_string ("isMsg_" ^ ProtocolName.user key) in
  let fld = GoFunTy ([], None) in
  pure (GoIfaceDecl (ty, [(fld_nm, fld)]) :: acc)

let mk_choice_ifaces ~key ~data acc =
  let fld_nm = FunctionName.of_string ("is" ^ VariableName.user key) in
  let fld = GoFunTy ([], None) in
  let decls =
    List.map data ~f:(fun lbl ->
        GoFunc
          ( Some
              ( VariableName.of_string "lbl"
              , GoTyVar
                  (VariableName.of_string
                     (LabelName.to_capitalize_string lbl) ) )
          , fld_nm
          , []
          , None
          , GoSeq [] ) )
  in
  (GoIfaceDecl (key, [(fld_nm, fld)]) :: decls) @ acc

(** Assumptions: 1. all pairs (label, payloadtype) are unique. 2. protocol
    call labels (P(roles)) are identical for all protocols *)
let mk_lbl_decl ~key:(proto, lbl) ~data (curr_lbls, decls) =
  let lbl_s = LabelName.to_capitalize_string lbl in
  let lbl_ty = VariableName.of_string lbl_s in
  let curr_lbls, decl =
    if Map.mem curr_lbls lbl then (curr_lbls, [])
    else (Map.add_exn curr_lbls ~key:lbl ~data:(), [GoTyDecl (lbl_s, data)])
  in
  let f_name = FunctionName.of_string ("isMsg_" ^ ProtocolName.user proto) in
  let m = Some (VariableName.of_string "lbl", GoTyVar lbl_ty) in
  (curr_lbls, (GoFunc (m, f_name, [], None, GoSeq []) :: decl) @ decls)

let mk_label_decls =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let lbls = st.GoGenM.proto_lbls in
  let _, decls =
    Map.fold lbls ~init:(Map.empty (module LabelName), []) ~f:mk_lbl_decl
  in
  pure (List.rev decls)

let decl_callback_iface ~key ~data acc =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  let ctx_type = Map.find_exn st.GoGenM.ctx_type key in
  let* decls = acc in
  let end_cb = (FunctionName.of_string "End", GoFunTy ([], None)) in
  pure (GoIfaceDecl (ctx_type, List.rev (end_cb :: data)) :: decls)

let mk_callback_decls =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  Map.fold st.GoGenM.callbacks ~init:(pure []) ~f:decl_callback_iface

let mk_choice_decls =
  let open GoGenM.Syntax in
  let* st = GoGenM.get in
  pure (Map.fold st.GoGenM.choice_iface ~init:[] ~f:mk_choice_ifaces)

let gen_entrypoint proto wg =
  (* get_ctx_type ~proto ~role *)
  let open GoGenM.Syntax in
  (* let* nm = fresh (ProtocolName.user proto ^ "_Start") in *)
  let* nm = fresh "Start" in
  let nm = FunctionName.of_string (VariableName.user nm) in
  let* st = GoGenM.get in
  let rs, nrs = Map.find_exn st.GoGenM.role_args proto in
  let rec go_gen is_dyn args acc roles =
    match roles with
    | [] -> pure (List.rev args, List.rev acc)
    | r :: rs ->
        let* ctx = fresh "ictx" in
        let* ctx_ty = get_ctx_type ~proto ~role:r in
        let* _, chs, chans = mk_channels proto r in
        let lp_fn =
          Map.find_exn st.GoGenM.lp_fns (LocalProtocolId.create proto r)
        in
        let r_call =
          GoCall
            ( GoFnVar lp_fn
            , GoVar ctx :: GoAddr (GoVar wg)
              :: List.map ~f:(fun x -> GoVar x) chs )
        in
        let nr =
          if is_dyn then GoSpawn r_call
          else
            GoSpawn
              (GoCall
                 ( GoAbs
                     ( []
                     , None
                     , GoSeq
                         [ GoDefer
                             (GoMCall (wg, FunctionName.of_string "Done", []))
                         ; GoExpr r_call ] )
                 , [] ) )
        in
        let wait =
          GoExpr (GoMCall (wg, FunctionName.of_string "Add", [GoIntLit 1]))
        in
        go_gen is_dyn (([ctx], ctx_ty) :: args)
          ((nr :: wait :: chans) @ acc)
          rs
  in
  let* args, stmts = go_gen false [] [] rs in
  let* nargs, nstmts = go_gen true [] [] nrs in
  let* _ = reset_new_chans in
  let wait = GoExpr (GoMCall (wg, FunctionName.of_string "Wait", [])) in
  pure
    (GoFunc
       ( None
       , nm
       , args @ nargs
       , None
       , GoSeq ((GoVarDecl (wg, GoWaitGroup) :: stmts) @ nstmts @ [wait]) )
    )

let gen_code_alt _root_dir gen_protocol global_t local_t =
  let open GoGenM.Syntax in
  let genf =
    let* _ = Map.fold global_t ~init:(pure ()) ~f:reg_role_args in
    let* _ = Map.fold local_t ~init:(pure ()) ~f:reg_req_chans in
    let* wg = fresh "wg" in
    let* roles = Map.fold local_t ~init:(pure []) ~f:(gen_local_func ~wg) in
    let* entry = gen_entrypoint gen_protocol wg in
    let* chans = Map.fold global_t ~init:(pure []) ~f:mk_proto_ifaces in
    let* choices = mk_choice_decls in
    let* labels = mk_label_decls in
    let* callbacks = mk_callback_decls in
    pure (chans @ labels @ choices @ callbacks @ roles @ [entry])
  in
  ppr_prog (GoGenM.eval genf GoGenM.init)

let generate_from_scr ast protocol root_dir =
  let global_t = nested_t_of_module ast in
  ensure_unique_identifiers global_t ;
  let local_t = Ltype.project_nested_t global_t in
  let local_t = Ltype.unfold_combine local_t in
  (* Stdio.print_endline (Ltype.show_local_t local_t); *)
  let local_t = Ltype.ensure_unique_tvars local_t in
  gen_code_alt root_dir protocol global_t local_t

let error_no_global_protocol () =
  Err.uerr
    (Err.InvalidCommandLineParam
       "Cannot find entrypoint global protocol in input file." )

let find_global_protocol ast =
  let open Syntax in
  match (List.rev ast.protocols, List.rev ast.nested_protocols) with
  | [], x :: _ -> x.Loc.value.name
  | x :: _, _ -> x.Loc.value.name
  | _, _ -> error_no_global_protocol ()

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
