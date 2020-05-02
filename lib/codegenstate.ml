open Names
open! Base
open Ltype
open Printf
open Gtype

module UniqueNameGen : sig
  type t

  val create : unit -> t

  val unique_name : t -> string -> t * string

  val curr_unique_name : t -> string -> string
end = struct
  type t = (string, int, String.comparator_witness) Map.t

  let create () = Map.empty (module String)

  let make_unique_name name uid = sprintf "%s_%d" name uid

  let rec unique_name uids name =
    match Map.find uids name with
    | None -> (Map.add_exn uids ~key:name ~data:1, name)
    | Some curr_id -> (
        let uid = curr_id + 1 in
        let new_name = make_unique_name name uid in
        match Map.find uids new_name with
        | None ->
            let uids = Map.add_exn uids ~key:new_name ~data:1 in
            (Map.update uids name ~f:(fun _ -> uid), new_name)
        | Some _ -> unique_name (Map.update uids name ~f:(fun _ -> uid)) name
        )

  let curr_unique_name uids name =
    let uid = Map.find_exn uids name in
    make_unique_name name uid
end

let msg_type_name msg = String.capitalize (LabelName.user msg)

(* let lowercase_role_name role = String.lowercase @@ RoleName.user role *)

let capitalize_role_name role = String.capitalize @@ RoleName.user role

let capitalize_local_protocol protocol =
  String.capitalize @@ LocalProtocolName.user protocol

let chan_struct_field_name role msg_type =
  sprintf "%s_%s" (capitalize_role_name role) (msg_type_name msg_type)

let chan_struct_name role_name =
  sprintf "%s_Chan" (capitalize_role_name role_name)

let send_role_chan_name role_name local_protocol =
  sprintf "Invite_%s_To_%s"
    (capitalize_role_name role_name)
    (capitalize_local_protocol local_protocol)

let recv_role_chan_name caller local_protocol =
  sprintf "%s_Invite_To_%s"
    (capitalize_role_name caller)
    (capitalize_local_protocol local_protocol)

let send_role_invite_chan_name role_name local_protocol =
  sprintf "%s_InviteChan" (send_role_chan_name role_name local_protocol)

let recv_role_invite_chan_name role_name local_protocol =
  sprintf "%s_InviteChan" (recv_role_chan_name role_name local_protocol)

let invite_struct_name local_protocol =
  sprintf "%s_InviteChan" (capitalize_local_protocol local_protocol)

let messages_import root pkg =
  sprintf "import \"%s/messages/%s\"" (ProtocolName.user root)
    (PackageName.user pkg)

let package_stmt pkg_name = sprintf "package %s" (PackageName.user pkg_name)

(* GOLANG PACKAGE NAMES *)
let pkg_channels = PackageName.of_string "channels"

let protocol_pkg_name protocol =
  String.lowercase @@ ProtocolName.user protocol

let struct_decl struct_name field_decls =
  let field_decls = List.sort field_decls ~compare:String.compare in
  let field_decls_str = String.concat ~sep:"\n" field_decls in
  sprintf "type %s struct {\n%s\n}" struct_name field_decls_str

module ImportsEnv : sig
  type t

  val import_messages : t -> ProtocolName.t -> t * string

  val import_channels : t -> ProtocolName.t -> t * string

  val import_result : t -> ProtocolName.t -> t * string

  (* These last 3 might be overkill *)
  val import_invitations : t -> t * string

  val import_callbacks : t -> t * string

  val import_roles : t -> t * string

  val generate_imports : t -> string
end = struct
  type aliases = (string, string, String.comparator_witness) Map.t

  type import_aliases =
    { messages: aliases
    ; channels: aliases
    ; result: aliases
    ; invitations: aliases
    ; callbacks: aliases
    ; roles: aliases }

  type t = UniqueNameGen.t * ProtocolName.t * import_aliases

  let import_messages env _ = (env, "")

  let import_channels env _ = (env, "")

  let import_invitations env = (env, "")

  let import_callbacks env = (env, "")

  let import_result env _ = (env, "")

  let import_roles env = (env, "")

  let generate_imports (_ : t) = ""
end

module ChannelEnv : sig
  type t

  val new_channel : t -> RoleName.t -> LabelName.t -> t * ChannelName.t

  val gen_channel_struct : t -> RoleName.t -> string

  val create : ProtocolName.t -> t
end = struct
  type channel_fields = (ChannelName.t * MessageStructName.t) list

  type t = UniqueNameGen.t * PackageName.t * channel_fields

  let new_channel (name_gen, pkg, channel_fields) role msg_label =
    let chan_name = chan_struct_field_name role msg_label in
    let name_gen, chan_name = UniqueNameGen.unique_name name_gen chan_name in
    let channel_name = ChannelName.of_string chan_name in
    let msg_struct_name =
      MessageStructName.of_string (msg_type_name msg_label)
    in
    let env =
      (name_gen, pkg, (channel_name, msg_struct_name) :: channel_fields)
    in
    (env, channel_name)

  let gen_channel_struct ((_, pkg, channel_fields) : t) role =
    let gen_chan_field_decl (chan_name, msg_struct) =
      sprintf "\t%s chan %s.%s"
        (ChannelName.user chan_name)
        (PackageName.user pkg)
        (MessageStructName.user msg_struct)
    in
    let chan_decls = List.map ~f:gen_chan_field_decl channel_fields in
    struct_decl (chan_struct_name role) chan_decls

  let create protocol =
    let pkg_name = PackageName.of_string (protocol_pkg_name protocol) in
    (UniqueNameGen.create (), pkg_name, [])
end

module InviteEnv : sig
  type t

  val new_send_role_channel :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * InviteChannelName.t

  val new_send_invite_channel :
    t -> RoleName.t -> LocalProtocolName.t -> t * InviteChannelName.t

  val send_self_role_channel :
    t -> RoleName.t -> LocalProtocolName.t -> InviteChannelName.t

  val send_self_invite_channel :
    t -> RoleName.t -> LocalProtocolName.t -> InviteChannelName.t

  val new_recv_role_channel :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * InviteChannelName.t

  val new_recv_invite_channel :
    t -> RoleName.t -> LocalProtocolName.t -> t * InviteChannelName.t

  val gen_invite_channel_struct : t -> LocalProtocolName.t -> string

  val create : unit -> t
end = struct
  type role_channel_fields =
    (InviteChannelName.t * PackageName.t * ChannelStructName.t) list

  type role_invite_channel_fields =
    (InviteChannelName.t * InviteChannelStructName.t) list

  type t = UniqueNameGen.t * role_channel_fields * role_invite_channel_fields

  let new_role_channel_field
      (name_gen, channel_fields, invite_channel_fields) channel_name new_role
      protocol =
    let name_gen, channel_name =
      UniqueNameGen.unique_name name_gen channel_name
    in
    let invite_chan = InviteChannelName.of_string channel_name in
    let pkg = PackageName.of_string @@ protocol_pkg_name protocol in
    let role_chan_name =
      ChannelStructName.of_string @@ chan_struct_name new_role
    in
    let env =
      ( name_gen
      , (invite_chan, pkg, role_chan_name) :: channel_fields
      , invite_channel_fields )
    in
    (env, invite_chan)

  let new_invite_channel (name_gen, channel_fields, invite_channel_fields)
      channel_name local_protocol =
    let name_gen, channel_name =
      UniqueNameGen.unique_name name_gen channel_name
    in
    let invite_chan = InviteChannelName.of_string channel_name in
    let invite_struct = invite_struct_name local_protocol in
    let chan_type = InviteChannelStructName.of_string invite_struct in
    let env =
      ( name_gen
      , channel_fields
      , (invite_chan, chan_type) :: invite_channel_fields )
    in
    (env, invite_chan)

  let new_send_role_channel env participant local_protocol new_role protocol
      =
    let channel_name = send_role_chan_name participant local_protocol in
    new_role_channel_field env channel_name new_role protocol

  let new_send_invite_channel env participant local_protocol =
    let channel_name =
      send_role_invite_chan_name participant local_protocol
    in
    new_invite_channel env channel_name local_protocol

  let send_self_role_channel (name_gen, _, _) role local_protocol =
    let channel_name = send_role_chan_name role local_protocol in
    let channel_name =
      UniqueNameGen.curr_unique_name name_gen channel_name
    in
    InviteChannelName.of_string channel_name

  let send_self_invite_channel (name_gen, _, _) role local_protocol =
    let channel_name = send_role_invite_chan_name role local_protocol in
    let channel_name =
      UniqueNameGen.curr_unique_name name_gen channel_name
    in
    InviteChannelName.of_string channel_name

  let new_recv_role_channel env caller local_protocol new_role protocol =
    let channel_name = recv_role_chan_name caller local_protocol in
    new_role_channel_field env channel_name new_role protocol

  let new_recv_invite_channel env caller local_protocol =
    let channel_name = recv_role_invite_chan_name caller local_protocol in
    new_invite_channel env channel_name local_protocol

  let gen_invite_channel_struct (_, channel_fields, invite_channel_fields)
      local_protocol =
    let role_chan_field_decl (chan_name, pkg, role_chan) =
      sprintf "\t%s chan %s.%s"
        (InviteChannelName.user chan_name)
        (PackageName.user pkg)
        (ChannelStructName.user role_chan)
    in
    let invite_chan_field_decl (chan_name, invite_chan_struct) =
      sprintf "\t%s chan %s"
        (InviteChannelName.user chan_name)
        (InviteChannelStructName.user invite_chan_struct)
    in
    let role_chan_decls = List.map ~f:role_chan_field_decl channel_fields in
    let invite_chan_decls =
      List.map ~f:invite_chan_field_decl invite_channel_fields
    in
    let struct_name = invite_struct_name local_protocol in
    struct_decl struct_name (role_chan_decls @ invite_chan_decls)

  let create () = (UniqueNameGen.create (), [], [])
end

module CallbacksEnv : sig
  type t

  val new_send_callback : t -> LabelName.t -> RoleName.t -> t * string

  val new_recv_callback : t -> LabelName.t -> RoleName.t -> t * string

  (* Global protocol name *)

  val new_protocol_setup_callback : t -> ProtocolName.t -> t * string

  (* Local protocol name *)

  val new_create_protocol_env_callback :
    t -> string -> ProtocolName.t -> t * string

  val new_protocol_result_callback :
    t -> string -> ProtocolName.t -> RoleName.t -> t * string
end = struct
  type t = int

  let new_send_callback env _ _ = (env, "")

  let new_recv_callback env _ _ = (env, "")

  let new_protocol_setup_callback env _ = (env, "")

  let new_create_protocol_env_callback env _ _ = (env, "")

  let new_protocol_result_callback env _ _ _ = (env, "")
end

(* LTYPE CODEGEN *)
module LTypeCodeGenEnv : sig
  type t

  val create : ProtocolName.t -> RoleName.t -> t

  val new_channel : t -> RoleName.t -> LabelName.t -> t * ChannelName.t

  val gen_channel_struct : t -> RoleName.t -> string

  val new_send_role_channel :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * InviteChannelName.t

  val new_send_invite_channel :
    t -> RoleName.t -> LocalProtocolName.t -> t * InviteChannelName.t

  val send_self_role_channel :
    t -> RoleName.t -> LocalProtocolName.t -> InviteChannelName.t

  val send_self_invite_channel :
    t -> RoleName.t -> LocalProtocolName.t -> InviteChannelName.t

  val new_recv_role_channel :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * InviteChannelName.t

  val new_recv_invite_channel :
    t -> RoleName.t -> LocalProtocolName.t -> t * InviteChannelName.t

  val gen_invite_channel_struct : t -> LocalProtocolName.t -> string
end = struct
  type t =
    { protocol: ProtocolName.t
    ; role: RoleName.t
    ; channel_env: ChannelEnv.t
    ; invite_env: InviteEnv.t }

  let create protocol role =
    let channel_env = ChannelEnv.create protocol in
    let invite_env = InviteEnv.create () in
    {protocol; role; channel_env; invite_env}

  let new_channel ({channel_env; _} as t) role msg_label =
    let channel_env, channel_name =
      ChannelEnv.new_channel channel_env role msg_label
    in
    ({t with channel_env}, channel_name)

  let gen_channel_struct {channel_env; _} role =
    ChannelEnv.gen_channel_struct channel_env role

  let new_send_role_channel ({invite_env; _} as t) participant local_protocol
      new_role protocol =
    let invite_env, channel_name =
      InviteEnv.new_send_role_channel invite_env participant local_protocol
        new_role protocol
    in
    ({t with invite_env}, channel_name)

  let new_recv_role_channel ({invite_env; _} as t) caller local_protocol
      new_role protocol =
    let invite_env, channel_name =
      InviteEnv.new_recv_role_channel invite_env caller local_protocol
        new_role protocol
    in
    ({t with invite_env}, channel_name)

  let new_send_invite_channel ({invite_env; _} as t) participant
      local_protocol =
    let invite_env, channel_name =
      InviteEnv.new_send_invite_channel invite_env participant local_protocol
    in
    ({t with invite_env}, channel_name)

  let send_self_role_channel {invite_env; _} role local_protocol =
    InviteEnv.send_self_role_channel invite_env role local_protocol

  let send_self_invite_channel {invite_env; _} role local_protocol =
    InviteEnv.send_self_invite_channel invite_env role local_protocol

  let new_recv_invite_channel ({invite_env; _} as t) caller local_protocol =
    let invite_env, channel_name =
      InviteEnv.new_recv_invite_channel invite_env caller local_protocol
    in
    ({t with invite_env}, channel_name)

  let gen_invite_channel_struct {invite_env; _} local_protocol =
    InviteEnv.gen_invite_channel_struct invite_env local_protocol
end

let gen_send_invite_chan_names protocol_lookup protocol env
    (participant, new_role) =
  let local_protocol =
    lookup_local_protocol protocol_lookup protocol new_role
  in
  let env, role_chan =
    LTypeCodeGenEnv.new_send_role_channel env participant local_protocol
      new_role protocol
  in
  let env, invite_chan =
    LTypeCodeGenEnv.new_send_invite_channel env participant local_protocol
  in
  (env, (role_chan, invite_chan))

let gen_accept_invite_chan_names env curr_role caller new_role protocol
    local_protocol =
  if RoleName.equal caller curr_role then
    let role_chan =
      LTypeCodeGenEnv.send_self_role_channel env curr_role local_protocol
    in
    let invite_chan =
      LTypeCodeGenEnv.send_self_invite_channel env curr_role local_protocol
    in
    (env, role_chan, invite_chan)
  else
    let env, role_chan =
      LTypeCodeGenEnv.new_recv_role_channel env caller local_protocol
        new_role protocol
    in
    let env, invite_chan =
      LTypeCodeGenEnv.new_recv_invite_channel env caller local_protocol
    in
    (env, role_chan, invite_chan)

(* gen_role_implementation protocol role local_protocol ltype -> env * string*)
(* gen_impl env ltype indent -> env string *)
(* TODO: Are recursion labels unique? *)
(* TODO code gen of ltype *)
(* TODO: make local_proto_name a LocalProtocolName.t *)
let gen_role_implementation global_t protocol_lookup protocol role
    (local_proto_name : LocalProtocolName.t) ltype =
  let incr_indent curr_indent = sprintf "\t%s" curr_indent in
  let indent_line line indent = sprintf "%s%s" indent line in
  let rec gen_implementation indent env ltype =
    match ltype with
    | EndL -> (env, indent_line "return env.Done()" indent)
    | TVarL var ->
        ( env
        , indent_line
            (sprintf "continue %s" (TypeVariableName.user var))
            indent )
    | MuL (_, ltype') ->
        let env, impl = gen_implementation (incr_indent indent) env ltype' in
        (env, impl)
    | InviteCreateL (invite_roles, _, protocol', ltype') ->
        (* TODO: Delete this*)
        let print_invite_channels (role_chan, invite_chan) =
          sprintf "%s, %s"
            (InviteChannelName.user role_chan)
            (InviteChannelName.user invite_chan)
        in
        let (new_proto_roles, _), _, _ = Map.find_exn global_t protocol' in
        let acting_roles = List.zip_exn invite_roles new_proto_roles in
        let env, invite_channels =
          List.fold_map ~init:env
            ~f:(gen_send_invite_chan_names protocol_lookup protocol')
            acting_roles
        in
        let env, impl = gen_implementation indent env ltype' in
        let invite_chan_strs =
          List.map ~f:print_invite_channels invite_channels
        in
        let str_invite_chans = String.concat ~sep:"\n" invite_chan_strs in
        (env, sprintf "%s\n\n%s" str_invite_chans impl)
    | AcceptL (new_role, protocol', _, _, caller, ltype') ->
        let local_protocol =
          lookup_local_protocol protocol_lookup protocol' new_role
        in
        let env, role_channel, invite_channel =
          gen_accept_invite_chan_names env role caller new_role protocol'
            local_protocol
        in
        let invite_channel_strs =
          sprintf "%s, %s"
            (InviteChannelName.user role_channel)
            (InviteChannelName.user invite_channel)
        in
        let env, impl = gen_implementation indent env ltype' in
        (env, sprintf "%s\n\n%s" invite_channel_strs impl)
    | ChoiceL (_, ltys) ->
        let env, impls =
          List.fold_map ~init:env
            ~f:(gen_implementation (incr_indent indent))
            ltys
        in
        (env, String.concat ~sep:"\n\n" impls)
    | RecvL ({label; _}, r, ltype') | SendL ({label; _}, r, ltype') ->
        (* TODO: split cases *)
        let env, channel_name = LTypeCodeGenEnv.new_channel env r label in
        let env, impl = gen_implementation indent env ltype' in
        ( env
        , sprintf "%s\n%s"
            (indent_line (ChannelName.user channel_name) indent)
            impl )
  in
  let env = LTypeCodeGenEnv.create protocol role in
  let env, impl = gen_implementation (incr_indent "") env ltype in
  let impl =
    sprintf "fun %s() {\n%s\n}"
      (LocalProtocolName.user local_proto_name)
      impl
  in
  (env, impl)

type codegen_result =
  { channels: (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; invite_channels:
      (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; impl:
      ( LocalProtocolName.t
      , string
      , LocalProtocolName.comparator_witness )
      Map.t }

let empty_result () : codegen_result =
  { channels= Map.empty (module ProtocolName)
  ; invite_channels= Map.empty (module ProtocolName)
  ; impl= Map.empty (module LocalProtocolName) }

let gen_code root (global_t : global_t) (local_t : local_t) =
  (* TODO: Move build_local_proto_name_lookup to codegen *)
  let protocol_lookup = build_local_proto_name_lookup local_t in
  let gen_protocol_role_implementation ~key ~data result =
    let gen_channels_file roles envs =
      let gen_channel_imports protocol = messages_import root protocol in
      let roles_and_envs = List.zip_exn roles envs in
      let channel_structs =
        List.map
          ~f:(fun (role, env) -> LTypeCodeGenEnv.gen_channel_struct env role)
          roles_and_envs
      in
      let chan_structs_str = String.concat ~sep:"\n\n" channel_structs in
      let protocol_pkg = PackageName.of_string @@ protocol_pkg_name key in
      let channel_imports = gen_channel_imports protocol_pkg in
      let pkg = package_stmt pkg_channels in
      sprintf "%s\n\n%s\n\n%s" pkg channel_imports chan_structs_str
    in
    let gen_invite_structs roles envs =
      let roles_and_envs = List.zip_exn roles envs in
      let channel_structs =
        List.map
          ~f:(fun (role, env) ->
            let local_protocol =
              lookup_local_protocol protocol_lookup key role
            in
            LTypeCodeGenEnv.gen_invite_channel_struct env local_protocol)
          roles_and_envs
      in
      channel_structs
    in
    let (roles, new_roles), _, _ = data in
    let protocol = key in
    let result, envs =
      List.fold_map ~init:result
        ~f:(fun ({impl; _} as res) role ->
          let local_protocol_id = LocalProtocolId.create protocol role in
          let _, ltype = Map.find_exn local_t local_protocol_id in
          let local_protocol =
            lookup_local_protocol protocol_lookup protocol role
          in
          let env, protocol_impl =
            gen_role_implementation global_t protocol_lookup protocol role
              local_protocol ltype
          in
          ( { res with
              impl= Map.add_exn impl ~key:local_protocol ~data:protocol_impl
            }
          , env ))
        (roles @ new_roles)
    in
    let chan_file = gen_channels_file roles envs in
    let channels =
      Map.add_exn result.channels ~key:protocol ~data:chan_file
    in
    let invite_chans = gen_invite_structs roles envs in
    let invite_chans_str = String.concat ~sep:"\n\n" invite_chans in
    let invite_channels =
      Map.add_exn result.invite_channels ~key:protocol ~data:invite_chans_str
    in
    {result with channels; invite_channels}
  in
  Map.fold ~init:(empty_result ()) ~f:gen_protocol_role_implementation
    global_t
