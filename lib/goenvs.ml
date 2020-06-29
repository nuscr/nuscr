open! Base
open Names
open Goimpl
open Gonames
open Gtype
open Ltype

module ImportsEnv : sig
  type t

  val import_messages : t -> ProtocolName.t -> t * PackageName.t

  val import_channels : t -> ProtocolName.t -> t * PackageName.t

  val import_results : t -> ProtocolName.t -> t * PackageName.t

  (* These next 3 might be overkill *)
  val import_invitations : t -> t * PackageName.t

  val import_callbacks : t -> t * PackageName.t

  val import_roles : t -> t * PackageName.t

  val import_sync : t -> t * PackageName.t

  val generate_imports : t -> string

  val create : RootDirName.t -> t
end = struct
  type aliases =
    (PackageName.t, PackageName.t, PackageName.comparator_witness) Map.t

  type import_aliases =
    { messages: aliases
    ; channels: aliases
    ; results: aliases
    ; invitations: aliases
    ; callbacks: aliases
    ; roles: aliases
    ; sync: aliases }

  type t = Namegen.t * RootDirName.t * import_aliases

  let get_or_add_import_alias name_gen aliases pkg_name =
    let pkg_key = PackageName.of_string pkg_name in
    match Map.find aliases pkg_key with
    | Some alias -> (name_gen, aliases, alias)
    | None ->
        let name_gen, alias = Namegen.unique_name name_gen pkg_name in
        let pkg_alias = PackageName.of_string alias in
        let aliases = Map.add_exn aliases ~key:pkg_key ~data:pkg_alias in
        (name_gen, aliases, pkg_alias)

  let import_messages (name_gen, root_dir, imports) protocol =
    let pkg = protocol_pkg_name protocol in
    let name_gen, messages, pkg_alias =
      get_or_add_import_alias name_gen imports.messages pkg
    in
    ((name_gen, root_dir, {imports with messages}), pkg_alias)

  let import_channels (name_gen, root_dir, imports) protocol =
    let pkg = protocol_pkg_name protocol in
    let name_gen, channels, pkg_alias =
      get_or_add_import_alias name_gen imports.channels pkg
    in
    ((name_gen, root_dir, {imports with channels}), pkg_alias)

  let import_invitations (name_gen, root_dir, imports) =
    let pkg = PackageName.user pkg_invitations in
    let name_gen, invitations, pkg_alias =
      get_or_add_import_alias name_gen imports.invitations pkg
    in
    ((name_gen, root_dir, {imports with invitations}), pkg_alias)

  let import_callbacks (name_gen, root_dir, imports) =
    let pkg = PackageName.user pkg_callbacks in
    let name_gen, callbacks, pkg_alias =
      get_or_add_import_alias name_gen imports.callbacks pkg
    in
    ((name_gen, root_dir, {imports with callbacks}), pkg_alias)

  let import_results (name_gen, root_dir, imports) protocol =
    let pkg = protocol_pkg_name protocol in
    let name_gen, results, pkg_alias =
      get_or_add_import_alias name_gen imports.results pkg
    in
    ((name_gen, root_dir, {imports with results}), pkg_alias)

  let import_roles (name_gen, root_dir, imports) =
    let pkg = PackageName.user pkg_roles in
    let name_gen, roles, pkg_alias =
      get_or_add_import_alias name_gen imports.roles pkg
    in
    ((name_gen, root_dir, {imports with roles}), pkg_alias)

  let import_sync (name_gen, root_dir, imports) =
    let pkg = PackageName.user pkg_sync in
    let name_gen, sync, pkg_alias =
      get_or_add_import_alias name_gen imports.sync pkg
    in
    ((name_gen, root_dir, {imports with sync}), pkg_alias)

  (* TODO: Change format of imports to one single statement like const *)
  let generate_imports (_, root_dir, imports) =
    let acc_alias_import gen_import_path root_dir ~key ~data imports =
      let pkg_name, alias = (key, data) in
      let import_path = gen_import_path root_dir pkg_name in
      let import =
        if PackageName.equal pkg_name alias then gen_import import_path
        else gen_aliased_import import_path (PackageName.user alias)
      in
      import :: imports
    in
    let join_imports imports = String.concat ~sep:"\n" imports in
    let messages_imports =
      Map.fold imports.messages ~init:[]
        ~f:(acc_alias_import messages_import_path root_dir)
    in
    let channels_imports =
      Map.fold imports.channels ~init:[]
        ~f:(acc_alias_import channels_import_path root_dir)
    in
    let invitations_imports =
      Map.fold imports.invitations ~init:[]
        ~f:(acc_alias_import invitations_import_path root_dir)
    in
    let callbacks_imports =
      Map.fold imports.callbacks ~init:[]
        ~f:(acc_alias_import callbacks_import_path root_dir)
    in
    let results_imports =
      Map.fold imports.results ~init:[]
        ~f:(acc_alias_import results_import_path root_dir)
    in
    let roles_imports =
      Map.fold imports.roles ~init:[]
        ~f:(acc_alias_import roles_import_path root_dir)
    in
    let sync_imports =
      Map.fold imports.sync ~init:[]
        ~f:(acc_alias_import sync_import_path root_dir)
    in
    let joined_imports_list =
      List.map ~f:join_imports
        [ messages_imports
        ; channels_imports
        ; invitations_imports
        ; callbacks_imports
        ; results_imports
        ; roles_imports
        ; sync_imports ]
    in
    let non_empty_imports =
      List.filter
        ~f:(fun import -> not (String.equal import ""))
        joined_imports_list
    in
    join_imports non_empty_imports

  let create root_dir =
    let empty_aliases () = Map.empty (module PackageName) in
    let name_gen = Namegen.create () in
    let imports =
      { messages= empty_aliases ()
      ; channels= empty_aliases ()
      ; results= empty_aliases ()
      ; invitations= empty_aliases ()
      ; callbacks= empty_aliases ()
      ; roles= empty_aliases ()
      ; sync= empty_aliases () }
    in
    (name_gen, root_dir, imports)
end

(* Assumes codegen validation checks on messages have already been passed *)
module MessagesEnv : sig
  type t

  val create : unit -> t

  val add_message_struct :
    t -> LabelName.t -> payload list -> t * MessageStructName.t

  val generate_messages_file : t -> PackageName.t -> string
end = struct
  type payload_field = VariableName.t * PayloadTypeName.t

  type message_info = MessageStructName.t * payload_field list

  type t =
    Namegen.t
    * (LabelName.t, message_info, LabelName.comparator_witness) Map.t

  let gen_unnamed_payload_field_names ((name_gen, named_fields) as acc)
      payload =
    match payload with
    | PValue (None, payload_type) ->
        let field_name = msg_field_name_from_type payload_type in
        let name_gen, field_name = Namegen.unique_name name_gen field_name in
        let payload_field_info =
          (VariableName.of_string field_name, payload_type)
        in
        (name_gen, payload_field_info :: named_fields)
    | _ -> acc

  let gen_named_payload_field_names ((name_gen, named_fields) as acc) payload
      =
    match payload with
    | PValue (Some name, payload_type) ->
        let field_name = msg_field_name name in
        let name_gen, field_name = Namegen.unique_name name_gen field_name in
        let payload_field_info =
          (VariableName.of_string field_name, payload_type)
        in
        (name_gen, payload_field_info :: named_fields)
    | _ -> acc

  let add_message_struct ((name_gen, msgs) as env) label payload =
    match Map.find msgs label with
    | None ->
        let struct_name = msg_type_name label in
        let name_gen, struct_name =
          Namegen.unique_name name_gen struct_name
        in
        let msg_struct_name = MessageStructName.of_string struct_name in
        let payload_name_gen = Namegen.create () in
        let payload_name_gen, payload_fields =
          List.fold ~init:(payload_name_gen, [])
            ~f:gen_named_payload_field_names payload
        in
        let _, payload_fields =
          List.fold
            ~init:(payload_name_gen, payload_fields)
            ~f:gen_unnamed_payload_field_names payload
        in
        let msg_struct_info = (msg_struct_name, payload_fields) in
        let msgs = Map.add_exn msgs ~key:label ~data:msg_struct_info in
        let env = (name_gen, msgs) in
        (env, msg_struct_name)
    | Some (msg_struct_name, _) -> (env, msg_struct_name)

  let generate_messages_file (_, msgs) pkg =
    let pkg_stmt = package_stmt pkg in
    let msg_structs = List.map ~f:gen_msg_struct (Map.data msgs) in
    join_non_empty_lines ~sep:"\n\n" (pkg_stmt :: msg_structs)

  let create () = (Namegen.create (), Map.empty (module LabelName))
end

module EnumNamesEnv : sig
  type t

  val create : unit -> t

  val new_enum :
       t
    -> LocalProtocolName.t
    -> LabelName.t list
    -> t * EnumTypeName.t * EnumName.t list
end = struct
  type t = Namegen.t * Namegen.t

  let create () =
    let enum_type_name_gen = Namegen.create () in
    let enum_name_gen = Namegen.create () in
    (enum_type_name_gen, enum_name_gen)

  let new_enum (enum_type_name_gen, enum_name_gen) local_protocol labels =
    let enum_name_of_label name_gen label =
      let enum_name = choice_enum_value local_protocol label in
      let name_gen, enum_name = Namegen.unique_name name_gen enum_name in
      (name_gen, EnumName.of_string enum_name)
    in
    let enum_type = choice_enum_type local_protocol in
    let enum_type_name_gen, enum_type =
      Namegen.unique_name enum_type_name_gen enum_type
    in
    let enum_type_name = EnumTypeName.of_string enum_type in
    let enum_name_gen, enum_values =
      List.fold_map ~init:enum_name_gen ~f:enum_name_of_label labels
    in
    let env = (enum_type_name_gen, enum_name_gen) in
    (env, enum_type_name, enum_values)
end

module ChannelEnv : sig
  type t

  val new_channel :
    t -> RoleName.t -> LabelName.t -> t * ChannelName.t * MessageStructName.t

  val gen_channel_struct : t -> string

  val get_channel_imports : t -> ImportsEnv.t

  val struct_name : t -> ChannelStructName.t

  val create : RoleName.t -> ProtocolName.t -> ImportsEnv.t -> t
end = struct
  type channel_fields = (ChannelName.t * MessageStructName.t) list

  type t =
    Namegen.t
    * ChannelStructName.t
    * ProtocolName.t
    * channel_fields
    * ImportsEnv.t

  let new_channel (name_gen, struct_name, protocol, channel_fields, imports)
      role msg_label =
    let chan_name = chan_struct_field_name role msg_label in
    let name_gen, chan_name = Namegen.unique_name name_gen chan_name in
    let channel_name = ChannelName.of_string chan_name in
    let msg_struct_name =
      MessageStructName.of_string (msg_type_name msg_label)
    in
    let channel_fields = (channel_name, msg_struct_name) :: channel_fields in
    (* Add messages/protocol import if the role receives any message *)
    let imports, _ = ImportsEnv.import_messages imports protocol in
    let env = (name_gen, struct_name, protocol, channel_fields, imports) in
    (env, channel_name, msg_struct_name)

  let gen_channel_struct
      ((_, struct_name, protocol, channel_fields, imports) : t) =
    let gen_chan_field_decl (chan_name, msg_struct) =
      (* Get pkg name *)
      let _, pkg = ImportsEnv.import_messages imports protocol in
      let msg_struct_type = protocol_msg_access pkg msg_struct in
      let chan_type = chan_type msg_struct_type in
      struct_field_decl (ChannelName.user chan_name) chan_type
    in
    let chan_decls = List.map ~f:gen_chan_field_decl channel_fields in
    struct_decl (ChannelStructName.user struct_name) chan_decls

  let struct_name (_, struct_name, _, _, _) = struct_name

  let get_channel_imports (_, _, _, _, imports) = imports

  let create role protocol imports =
    let struct_name = ChannelStructName.of_string @@ chan_struct_name role in
    (Namegen.create (), struct_name, protocol, [], imports)
end

module InviteEnv : sig
  type t

  val new_send_role_channel :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * InviteChannelName.t * ChannelStructName.t

  val new_send_invite_channel :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> t * InviteChannelName.t * InviteChannelStructName.t

  val send_self_role_channel :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> InviteChannelName.t * ChannelStructName.t

  val send_self_invite_channel :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> InviteChannelName.t * InviteChannelStructName.t

  val new_recv_role_channel :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * InviteChannelName.t * ChannelStructName.t

  val new_recv_invite_channel :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> t * InviteChannelName.t * InviteChannelStructName.t

  val gen_invite_channel_struct : t -> string

  val get_invite_imports : t -> ImportsEnv.t

  val struct_name : t -> InviteChannelStructName.t

  val create : LocalProtocolName.t -> ImportsEnv.t -> t
end = struct
  type role_channel_fields =
    (InviteChannelName.t * (PackageName.t * ChannelStructName.t)) list

  type role_invite_channel_fields =
    (InviteChannelName.t * InviteChannelStructName.t) list

  type t =
    Namegen.t
    * InviteChannelStructName.t
    * role_channel_fields
    * role_invite_channel_fields
    * ImportsEnv.t

  let new_role_channel_field
      (name_gen, struct_name, channel_fields, invite_channel_fields, imports)
      channel_name new_role protocol =
    let name_gen, channel_name = Namegen.unique_name name_gen channel_name in
    let invite_chan = InviteChannelName.of_string channel_name in
    let imports, pkg = ImportsEnv.import_channels imports protocol in
    let role_chan_name =
      ChannelStructName.of_string @@ chan_struct_name new_role
    in
    let env =
      ( name_gen
      , struct_name
      , (invite_chan, (pkg, role_chan_name)) :: channel_fields
      , invite_channel_fields
      , imports )
    in
    (env, invite_chan, role_chan_name)

  let new_invite_channel
      (name_gen, struct_name, channel_fields, invite_channel_fields, imports)
      channel_name local_protocol =
    let name_gen, channel_name = Namegen.unique_name name_gen channel_name in
    let invite_chan = InviteChannelName.of_string channel_name in
    let invite_struct = invite_struct_name local_protocol in
    let chan_type = InviteChannelStructName.of_string invite_struct in
    let invite_channel_fields =
      (invite_chan, chan_type) :: invite_channel_fields
    in
    let env =
      (name_gen, struct_name, channel_fields, invite_channel_fields, imports)
    in
    (env, invite_chan, chan_type)

  let new_send_role_channel env participant local_protocol new_role protocol
      =
    let channel_name = send_role_chan_name participant local_protocol in
    new_role_channel_field env channel_name new_role protocol

  let new_send_invite_channel env participant local_protocol =
    let channel_name =
      send_role_invite_chan_name participant local_protocol
    in
    new_invite_channel env channel_name local_protocol

  let send_self_role_channel (name_gen, _, channel_fields, _, _) role
      local_protocol =
    let channel_name = send_role_chan_name role local_protocol in
    let channel_name = Namegen.curr_unique_name name_gen channel_name in
    let chan_name =
      InviteChannelName.of_string (Option.value_exn channel_name)
    in
    let _, (_, channel_type) =
      List.find_exn channel_fields ~f:(fun (name, (_, _)) ->
          InviteChannelName.equal chan_name name)
    in
    (chan_name, channel_type)

  let send_self_invite_channel (name_gen, _, _, invite_channel_fields, _)
      role local_protocol =
    let channel_name = send_role_invite_chan_name role local_protocol in
    let channel_name = Namegen.curr_unique_name name_gen channel_name in
    let chan_name =
      InviteChannelName.of_string (Option.value_exn channel_name)
    in
    let _, channel_type =
      List.find_exn invite_channel_fields ~f:(fun (name, _) ->
          InviteChannelName.equal chan_name name)
    in
    (chan_name, channel_type)

  let new_recv_role_channel env caller local_protocol new_role protocol =
    let channel_name = recv_role_chan_name caller local_protocol in
    new_role_channel_field env channel_name new_role protocol

  let new_recv_invite_channel env caller local_protocol =
    let channel_name = recv_role_invite_chan_name caller local_protocol in
    new_invite_channel env channel_name local_protocol

  let gen_invite_channel_struct
      (_, struct_name, channel_fields, invite_channel_fields, _) =
    let role_chan_decls = List.map ~f:role_chan_field_decl channel_fields in
    let invite_chan_decls =
      List.map ~f:invite_chan_field_decl invite_channel_fields
    in
    struct_decl
      (InviteChannelStructName.user struct_name)
      (role_chan_decls @ invite_chan_decls)

  let struct_name (_, struct_name, _, _, _) = struct_name

  let get_invite_imports (_, _, _, _, imports) = imports

  let create local_protocol imports =
    let struct_name =
      InviteChannelStructName.of_string @@ invite_struct_name local_protocol
    in
    (Namegen.create (), struct_name, [], [], imports)
end

module ProtocolSetupEnv : sig
  type t

  val get_setup_channel_struct :
       t
    -> ProtocolName.t
    -> InviteChannelStructName.t * InviteChannelName.t list

  val get_setup_invite_struct :
       t
    -> ProtocolName.t
    -> InviteChannelStructName.t * InviteChannelName.t list

  val get_setup_function_name : t -> ProtocolName.t -> FunctionName.t

  val gen_setup_channel_struct :
    t -> PackageName.t -> ProtocolName.t -> string

  val gen_setup_invite_struct : t -> ProtocolName.t -> string

  val create : local_proto_name_lookup -> local_t -> global_t -> t
end = struct
  type role_chan_field =
    InviteChannelStructName.t
    * (InviteChannelName.t * ChannelStructName.t) list

  type invite_chan_field =
    InviteChannelStructName.t
    * (InviteChannelName.t * InviteChannelStructName.t) list

  type setup_functions_env =
    (ProtocolName.t, FunctionName.t, ProtocolName.comparator_witness) Map.t

  type t =
    { role_channels:
        ( ProtocolName.t
        , role_chan_field
        , ProtocolName.comparator_witness )
        Map.t
    ; invite_channels:
        ( ProtocolName.t
        , invite_chan_field
        , ProtocolName.comparator_witness )
        Map.t
    ; setup_functions: Namegen.t * setup_functions_env }

  let new_protocol_setup_channel_struct env protocol roles =
    let gen_field_decl role =
      let chan_name =
        InviteChannelName.of_string @@ setup_role_chan_name role
      in
      let chan_type = ChannelStructName.of_string @@ chan_struct_name role in
      (chan_name, chan_type)
    in
    let struct_name =
      InviteChannelStructName.of_string @@ setup_chan_struct_name protocol
    in
    let chan_fields = List.map ~f:gen_field_decl roles in
    let role_channels =
      Map.add_exn env.role_channels ~key:protocol
        ~data:(struct_name, chan_fields)
    in
    {env with role_channels}

  let new_protocol_setup_invite_struct env protocol_lookup protocol roles =
    let gen_field_decl role =
      let chan_name =
        InviteChannelName.of_string @@ setup_invite_chan_name role
      in
      let local_protocol =
        lookup_local_protocol protocol_lookup protocol role
      in
      let chan_type =
        InviteChannelStructName.of_string
        @@ invite_struct_name local_protocol
      in
      (chan_name, chan_type)
    in
    let struct_name =
      InviteChannelStructName.of_string @@ setup_invite_struct_name protocol
    in
    let chan_fields = List.map ~f:gen_field_decl roles in
    let invite_channels =
      Map.add_exn env.invite_channels ~key:protocol
        ~data:(struct_name, chan_fields)
    in
    {env with invite_channels}

  let get_setup_channel_struct {role_channels; _} protocol =
    let struct_name, chan_fields = Map.find_exn role_channels protocol in
    let chan_names, _ = List.unzip chan_fields in
    (struct_name, chan_names)

  let get_setup_invite_struct {invite_channels; _} protocol =
    let struct_name, chan_fields = Map.find_exn invite_channels protocol in
    let chan_names, _ = List.unzip chan_fields in
    (struct_name, chan_names)

  let gen_setup_channel_struct {role_channels; _} channels_pkg protocol =
    let struct_name, chan_fields = Map.find_exn role_channels protocol in
    let chan_decls =
      List.map chan_fields ~f:(fun (chan_field, chan_struct) ->
          role_chan_field_decl (chan_field, (channels_pkg, chan_struct)))
    in
    struct_decl (InviteChannelStructName.user struct_name) chan_decls

  let gen_setup_invite_struct {invite_channels; _} protocol =
    let struct_name, chan_fields = Map.find_exn invite_channels protocol in
    let chan_decls = List.map chan_fields ~f:invite_chan_field_decl in
    struct_decl (InviteChannelStructName.user struct_name) chan_decls

  let new_protocol_setup_function env protocol =
    let func_name_gen, setup_function_names = env.setup_functions in
    let setup_function = protocol_setup_function_name protocol in
    let func_name_gen, setup_function =
      Namegen.unique_name func_name_gen setup_function
    in
    let setup_function_names =
      Map.add_exn setup_function_names ~key:protocol
        ~data:(FunctionName.of_string setup_function)
    in
    let setup_functions = (func_name_gen, setup_function_names) in
    {env with setup_functions}

  let get_setup_function_name env protocol =
    let _, setup_function_names = env.setup_functions in
    Map.find_exn setup_function_names protocol

  let uid_gen_of_local_type protocol_lookup local_t =
    let add_local_proto_function ~key:local_protocol_id ~data:_ name_gen =
      let local_protocol =
        lookup_protocol_id protocol_lookup local_protocol_id
      in
      let impl_function_name = local_protocol_function_name local_protocol in
      let name_gen, _ = Namegen.unique_name name_gen impl_function_name in
      name_gen
    in
    Map.fold ~init:(Namegen.create ()) ~f:add_local_proto_function local_t

  let create_empty protocol_lookup local_t =
    let function_name_gen = uid_gen_of_local_type protocol_lookup local_t in
    let setup_env = (function_name_gen, Map.empty (module ProtocolName)) in
    { role_channels= Map.empty (module ProtocolName)
    ; invite_channels= Map.empty (module ProtocolName)
    ; setup_functions= setup_env }

  let create protocol_lookup local_t global_t =
    let add_protocol_setup ~key:protocol ~data:(all_roles, _, _) setup_env =
      let roles, _ = all_roles in
      let setup_env =
        new_protocol_setup_channel_struct setup_env protocol roles
      in
      let setup_env =
        new_protocol_setup_invite_struct setup_env protocol_lookup protocol
          roles
      in
      new_protocol_setup_function setup_env protocol
    in
    let empty_setup_env = create_empty protocol_lookup local_t in
    Map.fold global_t ~init:empty_setup_env ~f:add_protocol_setup
end

module ProtocolSetupGen : sig
  type t

  val create :
       RootDirName.t
    -> RoleName.t list
    -> ProtocolName.t
    -> local_proto_name_lookup
    -> t

  val generate_setup_channels :
       global_t
    -> local_proto_name_lookup
    -> t
    -> ImportsEnv.t
       * (RoleName.t, VariableName.t, RoleName.comparator_witness) Map.t
       * (RoleName.t, VariableName.t, RoleName.comparator_witness) Map.t
       * string
end = struct
  type setup_channels =
    ( RoleName.t
    , (ChannelName.t, VariableName.t, ChannelName.comparator_witness) Map.t
    , RoleName.comparator_witness )
    Map.t

  type setup_invite_channels =
    ( RoleName.t
    , ( InviteChannelName.t
      , VariableName.t
      , InviteChannelName.comparator_witness )
      Map.t
    , RoleName.comparator_witness )
    Map.t

  type channel_envs =
    (RoleName.t, ChannelEnv.t, RoleName.comparator_witness) Map.t

  type invite_envs =
    (RoleName.t, InviteEnv.t, RoleName.comparator_witness) Map.t

  type setup_env =
    { channel_envs: channel_envs
    ; invite_envs: invite_envs
    ; chan_vars: (VariableName.t * string) list
    ; setup_channels: setup_channels
    ; setup_invite_channels: setup_invite_channels }

  type t = ProtocolName.t * ImportsEnv.t * Namegen.t * setup_env

  let init_channel_envs ({channel_envs; _} as env) roles protocol imports =
    let channel_envs =
      List.fold roles ~init:channel_envs ~f:(fun acc role ->
          let chan_env = ChannelEnv.create role protocol imports in
          Map.add_exn acc ~key:role ~data:chan_env)
    in
    {env with channel_envs}

  let init_invite_envs ({invite_envs; _} as env) roles protocol
      protocol_lookup imports =
    let invite_envs =
      List.fold roles ~init:invite_envs ~f:(fun acc role ->
          let local_protocol =
            lookup_local_protocol protocol_lookup protocol role
          in
          let chan_env = InviteEnv.create local_protocol imports in
          Map.add_exn acc ~key:role ~data:chan_env)
    in
    {env with invite_envs}

  let init_setup_channels ({setup_channels; _} as env) roles =
    let setup_channels =
      List.fold roles ~init:setup_channels ~f:(fun acc role ->
          Map.add_exn acc ~key:role ~data:(Map.empty (module ChannelName)))
    in
    {env with setup_channels}

  let init_setup_invite_channels ({setup_invite_channels; _} as env) roles =
    let setup_invite_channels =
      List.fold roles ~init:setup_invite_channels ~f:(fun acc role ->
          Map.add_exn acc ~key:role
            ~data:(Map.empty (module InviteChannelName)))
    in
    {env with setup_invite_channels}

  let create_empty_env root_dir protocol : t =
    let var_name_gen = Namegen.create () in
    let setup_channels = Map.empty (module RoleName) in
    let setup_invite_channels = Map.empty (module RoleName) in
    let invite_envs = Map.empty (module RoleName) in
    let channel_envs = Map.empty (module RoleName) in
    let chan_vars = [] in
    let setup_env =
      { channel_envs
      ; invite_envs
      ; chan_vars
      ; setup_channels
      ; setup_invite_channels }
    in
    let imports = ImportsEnv.create root_dir in
    (protocol, imports, var_name_gen, setup_env)

  let create root_dir roles protocol protocol_lookup =
    let protocol, imports, var_name_gen, setup_env =
      create_empty_env root_dir protocol
    in
    let dummy_imports = ImportsEnv.create root_dir in
    let setup_env =
      init_channel_envs setup_env roles protocol dummy_imports
    in
    let setup_env =
      init_invite_envs setup_env roles protocol protocol_lookup dummy_imports
    in
    let setup_env = init_setup_channels setup_env roles in
    let setup_env = init_setup_invite_channels setup_env roles in
    (protocol, imports, var_name_gen, setup_env)

  let new_channel_vars
      (protocol, imports, var_name_gen, ({chan_vars; _} as setup_env)) sender
      recv label =
    let update_channel_envs imports
        ({channel_envs; setup_channels; _} as setup_env) role1 role2 chan_var
        =
      let imports, msgs_pkg = ImportsEnv.import_messages imports protocol in
      let channel_env = Map.find_exn channel_envs role1 in
      let channel_env, channel_name, msg_struct =
        ChannelEnv.new_channel channel_env role2 label
      in
      let channel_envs =
        Map.update channel_envs role1 ~f:(fun _ -> channel_env)
      in
      let role_setup_channels = Map.find_exn setup_channels role1 in
      let role_setup_channels =
        Map.update role_setup_channels channel_name ~f:(fun _ -> chan_var)
      in
      let setup_channels =
        Map.update setup_channels role1 ~f:(fun _ -> role_setup_channels)
      in
      let channel_type_name =
        chan_type (protocol_msg_access msgs_pkg msg_struct)
      in
      ( imports
      , {setup_env with channel_envs; setup_channels}
      , channel_type_name )
    in
    let msg_chan_var = new_msg_chan_var sender recv label in
    let var_name_gen, msg_chan_var =
      Namegen.unique_name var_name_gen msg_chan_var
    in
    let msg_chan_var_name = VariableName.of_string msg_chan_var in
    let imports, setup_env, var_type =
      update_channel_envs imports setup_env sender recv msg_chan_var_name
    in
    let imports, setup_env, _ =
      update_channel_envs imports setup_env recv sender msg_chan_var_name
    in
    let chan_vars = (msg_chan_var_name, var_type) :: chan_vars in
    let setup_env = {setup_env with chan_vars} in
    (protocol, imports, var_name_gen, setup_env)

  let gen_send_invite_channels protocol_lookup protocol env
      (participant, new_role) =
    let local_protocol =
      lookup_local_protocol protocol_lookup protocol new_role
    in
    let env, role_chan, role_chan_struct =
      InviteEnv.new_send_role_channel env participant local_protocol new_role
        protocol
    in
    let env, invite_chan, invite_chan_struct =
      InviteEnv.new_send_invite_channel env participant local_protocol
    in
    (env, (role_chan, role_chan_struct), (invite_chan, invite_chan_struct))

  let gen_accept_invite_channels env curr_role caller new_role protocol
      local_protocol =
    if RoleName.equal caller curr_role then
      let role_chan, role_chan_struct =
        InviteEnv.send_self_role_channel env curr_role local_protocol
      in
      let invite_chan, invite_chan_struct =
        InviteEnv.send_self_invite_channel env curr_role local_protocol
      in
      (env, (role_chan, role_chan_struct), (invite_chan, invite_chan_struct))
    else
      let env, role_chan, role_chan_struct =
        InviteEnv.new_recv_role_channel env caller local_protocol new_role
          protocol
      in
      let env, invite_chan, invite_chan_struct =
        InviteEnv.new_recv_invite_channel env caller local_protocol
      in
      (env, (role_chan, role_chan_struct), (invite_chan, invite_chan_struct))

  let update_setup_invite_channels setup_channels role
      (role_chan_var, role_chan) (invite_chan_var, invite_chan) =
    let role_setup_channels = Map.find_exn setup_channels role in
    let role_setup_channels =
      Map.add_exn role_setup_channels ~key:role_chan ~data:role_chan_var
    in
    let role_setup_channels =
      Map.add_exn role_setup_channels ~key:invite_chan ~data:invite_chan_var
    in
    Map.update setup_channels role ~f:(fun _ -> role_setup_channels)

  let add_send_invite_channels
      ({invite_envs; setup_invite_channels; _} as setup_env) role_chan_var
      invite_chan_var caller participant new_role call_protocol
      protocol_lookup =
    let invite_env = Map.find_exn invite_envs caller in
    let ( invite_env
        , (role_chan, role_chan_type)
        , (invite_chan, invite_chan_type) ) =
      gen_send_invite_channels protocol_lookup call_protocol invite_env
        (participant, new_role)
    in
    let invite_envs =
      Map.update invite_envs caller ~f:(fun _ -> invite_env)
    in
    let setup_invite_channels =
      update_setup_invite_channels setup_invite_channels caller
        (role_chan_var, role_chan)
        (invite_chan_var, invite_chan)
    in
    let setup_env = {setup_env with invite_envs; setup_invite_channels} in
    (setup_env, role_chan_type, invite_chan_type)

  let add_accept_invite_channels
      ({invite_envs; setup_invite_channels; _} as setup_env) role_chan_var
      invite_chan_var caller participant new_role call_protocol
      protocol_lookup =
    let invite_env = Map.find_exn invite_envs participant in
    let local_protocol =
      lookup_local_protocol protocol_lookup call_protocol new_role
    in
    let ( invite_env
        , (role_chan, role_chan_type)
        , (invite_chan, invite_chan_type) ) =
      gen_accept_invite_channels invite_env participant caller new_role
        call_protocol local_protocol
    in
    let setup_invite_channels =
      if RoleName.equal caller participant then setup_invite_channels
      else
        update_setup_invite_channels setup_invite_channels participant
          (role_chan_var, role_chan)
          (invite_chan_var, invite_chan)
    in
    let invite_envs =
      Map.update invite_envs participant ~f:(fun _ -> invite_env)
    in
    let setup_env = {setup_env with invite_envs; setup_invite_channels} in
    (setup_env, role_chan_type, invite_chan_type)

  let generate_invite_channel_vars
      (protocol, imports, var_name_gen, ({chan_vars; _} as setup_env)) caller
      participant new_role call_protocol protocol_lookup =
    let role_chan_var = new_setup_role_chan_var caller participant in
    let var_name_gen, role_chan_var =
      Namegen.unique_name var_name_gen role_chan_var
    in
    let role_chan_var_name = VariableName.of_string role_chan_var in
    let invite_chan_var = new_setup_invite_chan_var caller participant in
    let var_name_gen, invite_chan_var =
      Namegen.unique_name var_name_gen invite_chan_var
    in
    let invite_chan_var_name = VariableName.of_string invite_chan_var in
    let setup_env, role_chan_type, invite_chan_type =
      add_send_invite_channels setup_env role_chan_var_name
        invite_chan_var_name caller participant new_role call_protocol
        protocol_lookup
    in
    let setup_env, _, _ =
      add_accept_invite_channels setup_env role_chan_var_name
        invite_chan_var_name caller participant new_role call_protocol
        protocol_lookup
    in
    let imports, call_protocol_pkg =
      ImportsEnv.import_channels imports call_protocol
    in
    let role_chan_type_str =
      protocol_channel_access call_protocol_pkg role_chan_type
    in
    let invite_chan_type_str =
      invitation_pkg_access pkg_invitations invite_chan_type
    in
    let chan_vars =
      (role_chan_var_name, chan_type role_chan_type_str)
      :: (invite_chan_var_name, chan_type invite_chan_type_str)
      :: chan_vars
    in
    (protocol, imports, var_name_gen, {setup_env with chan_vars})

  let generate_invitation_vars env caller roles new_roles call_protocol
      protocol_lookup =
    let zipped_roles = List.zip_exn roles new_roles in
    List.fold ~init:env
      ~f:(fun env (role, new_role) ->
        generate_invite_channel_vars env caller role new_role call_protocol
          protocol_lookup)
      zipped_roles

  let rec generate_channel_vars global_t protocol_lookup (env : t) = function
    | EndG | TVarG _ -> env
    | MuG (_, gtype) ->
        generate_channel_vars global_t protocol_lookup env gtype
    | MessageG ({label; _}, sender, recv, gtype) ->
        let env = new_channel_vars env sender recv label in
        generate_channel_vars global_t protocol_lookup env gtype
    | ChoiceG (_, gtypes) ->
        List.fold gtypes ~init:env
          ~f:(generate_channel_vars global_t protocol_lookup)
    | CallG (caller, protocol, roles, gtype) ->
        let (new_roles, _), _, _ = Map.find_exn global_t protocol in
        let env =
          generate_invitation_vars env caller roles new_roles protocol
            protocol_lookup
        in
        generate_channel_vars global_t protocol_lookup env gtype

  let gen_role_channel_struct_assign channel_envs pkg indent ~key:role
      ~data:chan_fields (var_name_gen, role_struct_vars, struct_assignments)
      =
    let channel_env = Map.find_exn channel_envs role in
    let struct_fields, struct_values =
      Map.fold chan_fields ~init:([], [])
        ~f:(fun ~key:field ~data:value (fields, values) ->
          ( ChannelName.user field :: fields
          , VariableName.user value :: values ))
    in
    let role_chan_var = new_role_chan_setup_var role in
    let var_name_gen, role_chan_var =
      Namegen.unique_name var_name_gen role_chan_var
    in
    let role_chan_var_name = VariableName.of_string role_chan_var in
    let struct_name =
      protocol_channel_access pkg (ChannelEnv.struct_name channel_env)
    in
    let role_chan_struct =
      struct_literal struct_name struct_fields struct_values indent
    in
    let struct_assign =
      new_var_assignment role_chan_var_name role_chan_struct
    in
    let role_struct_vars =
      Map.add_exn role_struct_vars ~key:role ~data:role_chan_var_name
    in
    (var_name_gen, role_struct_vars, struct_assign :: struct_assignments)

  let gen_invite_channel_struct_assign invite_envs indent invitations_pkg
      ~key:role ~data:chan_fields
      (var_name_gen, invite_struct_vars, struct_assignments) =
    let invite_env = Map.find_exn invite_envs role in
    let struct_fields, struct_values =
      Map.fold chan_fields ~init:([], [])
        ~f:(fun ~key:field ~data:value (fields, values) ->
          ( InviteChannelName.user field :: fields
          , VariableName.user value :: values ))
    in
    let invite_chan_var = new_invite_chan_setup_var role in
    let var_name_gen, invite_chan_var =
      Namegen.unique_name var_name_gen invite_chan_var
    in
    let invite_chan_var_name = VariableName.of_string invite_chan_var in
    let struct_name =
      protocol_invite_channel_access invitations_pkg
        (InviteEnv.struct_name invite_env)
    in
    let invite_chan_struct =
      struct_literal struct_name struct_fields struct_values indent
    in
    let struct_assign =
      new_var_assignment invite_chan_var_name invite_chan_struct
    in
    let invite_struct_vars =
      Map.add_exn invite_struct_vars ~key:role ~data:invite_chan_var_name
    in
    (var_name_gen, invite_struct_vars, struct_assign :: struct_assignments)

  let generate_setup_channels global_t protocol_lookup env =
    let protocol, _, _, _ = env in
    let _, _, gtype = Map.find_exn global_t protocol in
    let ( protocol
        , imports
        , var_name_gen
        , { channel_envs
          ; invite_envs
          ; chan_vars
          ; setup_channels
          ; setup_invite_channels } ) =
      generate_channel_vars global_t protocol_lookup env gtype
    in
    let indent = incr_indent "" in
    let chan_var_assignments =
      List.map chan_vars ~f:new_chan_var_assignment
    in
    let chan_var_assignments =
      List.map chan_var_assignments ~f:(indent_line indent)
    in
    let chan_var_assignments_str =
      join_non_empty_lines chan_var_assignments
    in
    let imports, protocol_pkg =
      ImportsEnv.import_channels imports protocol
    in
    let imports, invitations_pkg = ImportsEnv.import_invitations imports in
    let var_name_gen, role_chan_vars, role_chan_assignments =
      Map.fold
        ~init:(var_name_gen, Map.empty (module RoleName), [])
        ~f:(gen_role_channel_struct_assign channel_envs protocol_pkg indent)
        setup_channels
    in
    let role_chan_assignments =
      List.map role_chan_assignments ~f:(indent_line indent)
    in
    let role_chan_assignments_str =
      join_non_empty_lines role_chan_assignments
    in
    let _, invite_chan_vars, invite_chan_assignmets =
      Map.fold
        ~init:(var_name_gen, Map.empty (module RoleName), [])
        ~f:
          (gen_invite_channel_struct_assign invite_envs indent
             invitations_pkg)
        setup_invite_channels
    in
    let invite_chan_assignmets =
      List.map invite_chan_assignmets ~f:(indent_line indent)
    in
    let invite_chan_assignmets_str =
      join_non_empty_lines invite_chan_assignmets
    in
    ( imports
    , role_chan_vars
    , invite_chan_vars
    , join_non_empty_lines ~sep:"\n\n"
        [ chan_var_assignments_str
        ; role_chan_assignments_str
        ; invite_chan_assignmets_str ] )
end

module CallbacksEnv : sig
  type t

  val new_send_callback :
    t -> LabelName.t -> RoleName.t -> ProtocolName.t -> t * CallbackName.t

  val new_recv_callback :
    t -> LabelName.t -> RoleName.t -> ProtocolName.t -> t * CallbackName.t

  val new_protocol_setup_callback : t -> ProtocolName.t -> t * CallbackName.t

  val new_convert_env_callback :
    t -> LocalProtocolName.t -> t * CallbackName.t

  val new_choice_callback :
       t
    -> RoleName.t
    -> LabelName.t list
    -> LocalProtocolName.t
    -> t * CallbackName.t * EnumName.t list

  val new_protocol_result_callback :
       t
    -> LocalProtocolName.t
    -> ProtocolName.t
    -> RoleName.t
    -> t * CallbackName.t

  val add_done_callback :
    t -> ProtocolName.t -> RoleName.t -> bool -> t * CallbackName.t

  val get_enum_names_env : t -> EnumNamesEnv.t

  val gen_callbacks_file : t -> LocalProtocolName.t -> bool -> string

  val create : RootDirName.t -> EnumNamesEnv.t -> t
end = struct
  type param =
    ParameterName.t
    * [ `Result of PackageName.t * ResultName.t
      | `Msg of PackageName.t * MessageStructName.t ]

  type return_val =
    [ `Env of CallbacksEnvName.t
    | `Result of PackageName.t * ResultName.t
    | `Msg of PackageName.t * MessageStructName.t
    | `Enum of EnumTypeName.t ]

  type callbacks = (CallbackName.t * param option * return_val option) list

  type enum = EnumTypeName.t * EnumName.t list

  type choice_enums = EnumNamesEnv.t * enum list

  type t = Namegen.t * choice_enums * callbacks * ImportsEnv.t

  let new_send_callback (name_gen, choice_enums, callbacks, imports)
      msg_label recv_role protocol =
    (* <msg_struct>_To_<role>[_<uid>]() <pkg>.<msg_struct> *)
    let callback_name = send_callback_name msg_label recv_role in
    let name_gen, callback_name =
      Namegen.unique_name name_gen callback_name
    in
    let callback = CallbackName.of_string callback_name in
    let msg_struct_name =
      MessageStructName.of_string (msg_type_name msg_label)
    in
    let imports, pkg = ImportsEnv.import_messages imports protocol in
    let return_val = Some (`Msg (pkg, msg_struct_name)) in
    let callbacks = (callback, None, return_val) :: callbacks in
    let env = (name_gen, choice_enums, callbacks, imports) in
    (env, callback)

  let new_recv_callback (name_gen, choice_enums, callbacks, imports)
      msg_label sender_role protocol =
    (* <msg_struct>_From_<role>[_uid](<msg_struct> <pkg>.<msg_struct>) *)
    let callback_name = recv_callback_name msg_label sender_role in
    let name_gen, callback_name =
      Namegen.unique_name name_gen callback_name
    in
    let callback = CallbackName.of_string callback_name in
    let msg_struct_name =
      MessageStructName.of_string (msg_type_name msg_label)
    in
    let imports, pkg = ImportsEnv.import_messages imports protocol in
    let param_name = ParameterName.of_string @@ new_msg_var msg_label in
    let param_decl = Some (param_name, `Msg (pkg, msg_struct_name)) in
    let callbacks = (callback, param_decl, None) :: callbacks in
    let env = (name_gen, choice_enums, callbacks, imports) in
    (env, callback)

  let new_protocol_setup_callback
      (name_gen, choice_enums, callbacks, imports) protocol =
    (* <protocol>_Setup[_uid]() *)
    let callback_name = protocol_setup_callback_name protocol in
    let name_gen, callback_name =
      Namegen.unique_name name_gen callback_name
    in
    let callback = CallbackName.of_string callback_name in
    let callbacks = (callback, None, None) :: callbacks in
    let env = (name_gen, choice_enums, callbacks, imports) in
    (env, callback)

  let new_convert_env_callback (name_gen, choice_enums, callbacks, imports)
      local_protocol =
    (* To_<local_protocol>_Env[_uid]() <local_protocol>_Env *)
    let callback_name = convert_env_callback_name local_protocol in
    let name_gen, callback_name =
      Namegen.unique_name name_gen callback_name
    in
    let callback = CallbackName.of_string callback_name in
    let new_env_type =
      CallbacksEnvName.of_string @@ callbacks_env_name local_protocol
    in
    let callbacks =
      (callback, None, Some (`Env new_env_type)) :: callbacks
    in
    let env = (name_gen, choice_enums, callbacks, imports) in
    (env, callback)

  let new_protocol_result_callback
      (name_gen, choice_enums, callbacks, imports) local_protocol protocol
      role =
    (* ResultFrom_<local_protocol>[_<uid>](result <protocol>.<role>_Result) *)
    let callback_name = result_callback_name local_protocol in
    let name_gen, callback_name =
      Namegen.unique_name name_gen callback_name
    in
    let callback = CallbackName.of_string callback_name in
    let imports, pkg = ImportsEnv.import_results imports protocol in
    let result_name = ResultName.of_string @@ result_struct_name role in
    let result_param =
      ParameterName.of_string (VariableName.user result_var)
    in
    let callbacks =
      (callback, Some (result_param, `Result (pkg, result_name)), None)
      :: callbacks
    in
    let env = (name_gen, choice_enums, callbacks, imports) in
    (env, callback)

  let new_choice_callback
      (name_gen, (enum_names_env, enums), callbacks, imports) choice_role
      labels local_protocol =
    let callback_name = choice_callback_name choice_role in
    let name_gen, callback_name =
      Namegen.unique_name name_gen callback_name
    in
    let callback = CallbackName.of_string callback_name in
    let enum_names_env, enum_type_name, enum_values =
      EnumNamesEnv.new_enum enum_names_env local_protocol labels
    in
    let enums = (enum_type_name, enum_values) :: enums in
    let callbacks =
      (callback, None, Some (`Enum enum_type_name)) :: callbacks
    in
    let env = (name_gen, (enum_names_env, enums), callbacks, imports) in
    (env, callback, enum_values)

  let add_done_callback
      ((name_gen, (enum_name_gen, enums), callbacks, imports) as env)
      protocol role is_dynamic_role =
    let callback_name = CallbackName.user done_callback in
    match Namegen.curr_unique_name name_gen callback_name with
    | None ->
        let name_gen, callback_name =
          Namegen.unique_name name_gen callback_name
        in
        let callback = CallbackName.of_string callback_name in
        let imports, return_type =
          if is_dynamic_role then (imports, None)
          else
            let result_type =
              ResultName.of_string @@ result_struct_name role
            in
            let imports, pkg = ImportsEnv.import_results imports protocol in
            (imports, Some (`Result (pkg, result_type)))
        in
        let callbacks = (callback, None, return_type) :: callbacks in
        let env = (name_gen, (enum_name_gen, enums), callbacks, imports) in
        (env, callback)
    | Some callback_name -> (env, CallbackName.of_string callback_name)

  let get_enum_names_env (_, (enum_names_env, _), _, _) = enum_names_env

  let gen_create_env_function local_protocol callbacks_env_name =
    let indent = incr_indent "" in
    let function_body = indent_line indent (panic_with_msg todo_panic_msg) in
    let create_func_name =
      FunctionName.of_string @@ new_create_env_function_name local_protocol
    in
    let return_type = Some (CallbacksEnvName.user callbacks_env_name) in
    function_decl create_func_name [] return_type function_body

  let gen_callbacks_file (_, (_, enums), callbacks, imports) local_protocol
      is_dynamic_role =
    let gen_enum (enum_type, enum_values) =
      let type_decl = enum_type_decl enum_type in
      let value_decls = enum_decl enum_type enum_values in
      join_non_empty_lines [type_decl; value_decls]
    in
    let pkg = package_stmt pkg_callbacks in
    let enum_decls = List.map ~f:gen_enum enums in
    let enum_decls_str = String.concat ~sep:"\n\n" enum_decls in
    let imports_str = ImportsEnv.generate_imports imports in
    let env_name =
      CallbacksEnvName.of_string @@ callbacks_env_name local_protocol
    in
    let callbacks_interface = callbacks_env_interface env_name callbacks in
    let create_env_func =
      if is_dynamic_role then gen_create_env_function local_protocol env_name
      else ""
    in
    join_non_empty_lines ~sep:"\n\n"
      [pkg; imports_str; enum_decls_str; callbacks_interface; create_env_func]

  let create root_dir enum_names_env =
    let name_gen = Namegen.create () in
    let imports = ImportsEnv.create root_dir in
    let enums = (enum_names_env, []) in
    (name_gen, enums, [], imports)
end

(* Information aggregated throughout the code generation of all the
   projections of a global protocol *)
type protocol_env =
  { channel_imports: ImportsEnv.t
  ; invite_imports: ImportsEnv.t
  ; callback_enum_names: EnumNamesEnv.t }

(* LTYPE CODEGEN *)
module LTypeCodeGenEnv : sig
  type t

  val create :
       ProtocolName.t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RootDirName.t
    -> protocol_env
    -> t

  val new_channel : t -> RoleName.t -> LabelName.t -> t * ChannelName.t

  val gen_channel_struct : t -> string

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

  val gen_invite_channel_struct : t -> string

  val new_send_callback :
    t -> LabelName.t -> RoleName.t -> ProtocolName.t -> t * CallbackName.t

  val new_recv_callback :
    t -> LabelName.t -> RoleName.t -> ProtocolName.t -> t * CallbackName.t

  val new_protocol_setup_callback : t -> ProtocolName.t -> t * CallbackName.t

  val new_convert_env_callback :
    t -> LocalProtocolName.t -> t * CallbackName.t

  val new_choice_callback :
       t
    -> RoleName.t
    -> LabelName.t list
    -> LocalProtocolName.t
    -> t * CallbackName.t * EnumName.t list

  val new_protocol_result_callback :
       t
    -> LocalProtocolName.t
    -> ProtocolName.t
    -> RoleName.t
    -> t * CallbackName.t

  val add_done_callback :
    t -> ProtocolName.t -> RoleName.t -> bool -> t * CallbackName.t

  val gen_result_struct : RoleName.t -> string

  val import_channels : t -> ProtocolName.t -> t * PackageName.t

  val import_results : t -> ProtocolName.t -> t * PackageName.t

  val import_invitations : t -> t * PackageName.t

  val import_callbacks : t -> t * PackageName.t

  val import_sync : t -> t * PackageName.t

  val generate_role_imports : t -> string

  val get_protocol_env : t -> protocol_env

  val gen_callbacks_file : t -> LocalProtocolName.t -> bool -> string
end = struct
  type t =
    { protocol: ProtocolName.t
    ; role: RoleName.t
    ; channel_env: ChannelEnv.t
    ; invite_env: InviteEnv.t
    ; callbacks_env: CallbacksEnv.t
    ; role_imports: ImportsEnv.t }

  let create protocol role local_protocol root_dir
      {callback_enum_names; channel_imports; invite_imports} =
    let channel_env = ChannelEnv.create role protocol channel_imports in
    let invite_env = InviteEnv.create local_protocol invite_imports in
    let callbacks_env = CallbacksEnv.create root_dir callback_enum_names in
    let role_imports = ImportsEnv.create root_dir in
    {protocol; role; channel_env; invite_env; callbacks_env; role_imports}

  let new_channel ({channel_env; _} as t) role msg_label =
    let channel_env, channel_name, _ =
      ChannelEnv.new_channel channel_env role msg_label
    in
    ({t with channel_env}, channel_name)

  let gen_channel_struct {channel_env; _} =
    ChannelEnv.gen_channel_struct channel_env

  let new_send_role_channel ({invite_env; _} as t) participant local_protocol
      new_role protocol =
    let invite_env, channel_name, _ =
      InviteEnv.new_send_role_channel invite_env participant local_protocol
        new_role protocol
    in
    ({t with invite_env}, channel_name)

  let new_recv_role_channel ({invite_env; _} as t) caller local_protocol
      new_role protocol =
    let invite_env, channel_name, _ =
      InviteEnv.new_recv_role_channel invite_env caller local_protocol
        new_role protocol
    in
    ({t with invite_env}, channel_name)

  let new_send_invite_channel ({invite_env; _} as t) participant
      local_protocol =
    let invite_env, channel_name, _ =
      InviteEnv.new_send_invite_channel invite_env participant local_protocol
    in
    ({t with invite_env}, channel_name)

  let send_self_role_channel {invite_env; _} role local_protocol =
    let role_chan, _ =
      InviteEnv.send_self_role_channel invite_env role local_protocol
    in
    role_chan

  let send_self_invite_channel {invite_env; _} role local_protocol =
    let invite_chan, _ =
      InviteEnv.send_self_invite_channel invite_env role local_protocol
    in
    invite_chan

  let new_recv_invite_channel ({invite_env; _} as t) caller local_protocol =
    let invite_env, channel_name, _ =
      InviteEnv.new_recv_invite_channel invite_env caller local_protocol
    in
    ({t with invite_env}, channel_name)

  let gen_invite_channel_struct {invite_env; _} =
    InviteEnv.gen_invite_channel_struct invite_env

  let new_send_callback ({callbacks_env; _} as env) msg_label recv_role
      protocol =
    let callbacks_env, callback =
      CallbacksEnv.new_send_callback callbacks_env msg_label recv_role
        protocol
    in
    ({env with callbacks_env}, callback)

  let new_recv_callback ({callbacks_env; _} as env) msg_label sender_role
      protocol =
    let callbacks_env, callback =
      CallbacksEnv.new_recv_callback callbacks_env msg_label sender_role
        protocol
    in
    ({env with callbacks_env}, callback)

  let new_protocol_setup_callback ({callbacks_env; _} as env) protocol =
    let callbacks_env, callback =
      CallbacksEnv.new_protocol_setup_callback callbacks_env protocol
    in
    ({env with callbacks_env}, callback)

  let new_convert_env_callback ({callbacks_env; _} as env) local_protocol =
    let callbacks_env, callback =
      CallbacksEnv.new_convert_env_callback callbacks_env local_protocol
    in
    ({env with callbacks_env}, callback)

  let new_protocol_result_callback ({callbacks_env; _} as env) local_protocol
      protocol role =
    let callbacks_env, callback =
      CallbacksEnv.new_protocol_result_callback callbacks_env local_protocol
        protocol role
    in
    ({env with callbacks_env}, callback)

  let add_done_callback ({callbacks_env; _} as env) protocol role
      is_dynamic_role =
    let callbacks_env, callback =
      CallbacksEnv.add_done_callback callbacks_env protocol role
        is_dynamic_role
    in
    ({env with callbacks_env}, callback)

  let new_choice_callback ({callbacks_env; _} as env) choice_role labels
      local_protocol =
    let callbacks_env, enum_values, callback =
      CallbacksEnv.new_choice_callback callbacks_env choice_role labels
        local_protocol
    in
    ({env with callbacks_env}, enum_values, callback)

  let gen_result_struct role =
    let struct_name = result_struct_name role in
    struct_decl struct_name []

  let import_channels ({role_imports; _} as env) protocol =
    let role_imports, pkg_alias =
      ImportsEnv.import_channels role_imports protocol
    in
    ({env with role_imports}, pkg_alias)

  let import_results ({role_imports; _} as env) protocol =
    let role_imports, pkg_alias =
      ImportsEnv.import_results role_imports protocol
    in
    ({env with role_imports}, pkg_alias)

  let import_invitations ({role_imports; _} as env) =
    let role_imports, pkg_alias =
      ImportsEnv.import_invitations role_imports
    in
    ({env with role_imports}, pkg_alias)

  let import_callbacks ({role_imports; _} as env) =
    let role_imports, pkg_alias = ImportsEnv.import_callbacks role_imports in
    ({env with role_imports}, pkg_alias)

  let import_sync ({role_imports; _} as env) =
    let role_imports, pkg_alias = ImportsEnv.import_sync role_imports in
    ({env with role_imports}, pkg_alias)

  let generate_role_imports {role_imports; _} =
    ImportsEnv.generate_imports role_imports

  let gen_callbacks_file {callbacks_env; _} local_protocol is_dynamic_role =
    CallbacksEnv.gen_callbacks_file callbacks_env local_protocol
      is_dynamic_role

  let get_protocol_env {callbacks_env; channel_env; invite_env; _} =
    { channel_imports= ChannelEnv.get_channel_imports channel_env
    ; invite_imports= InviteEnv.get_invite_imports invite_env
    ; callback_enum_names= CallbacksEnv.get_enum_names_env callbacks_env }
end
