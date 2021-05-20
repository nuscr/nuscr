open! Base
open Names
open Goimpl
open Gonames
open Mpst
open Gtype
open Ltype

module ImportsEnv : sig
  type t

  val import_messages : t -> t * PackageName.t

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
  type aliases = PackageName.t Map.M(PackageName).t

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

  let import_messages (name_gen, root_dir, imports) =
    let pkg = PackageName.user pkg_messages in
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

  val create : ProtocolName.t -> t

  (* val add_message_struct : t -> LabelName.t -> payload list -> t *
     MessageStructName.t *)

  val add_message_enum : t -> LabelName.t -> t

  val add_invitation_enum : t -> ProtocolName.t -> RoleName.t list -> t

  val get_message_enum : t -> LabelName.t -> EnumName.t

  val get_enum_type_name : t -> EnumTypeName.t

  val get_invitation_enum :
    t -> ProtocolName.t -> RoleName.t list -> EnumName.t

  (* val generate_messages_file : t -> PackageName.t -> string *)

  val generate_messages_file : t -> string
end = struct
  (* type payload_field = VariableName.t * PayloadTypeName.t *)

  (* type message_info = MessageStructName.t * payload_field list *)

  type t = EnumTypeName.t * Namegen.t * EnumName.t Map.M(LabelName).t

  (* let gen_unnamed_payload_field_names ((name_gen, named_fields) as acc)
     payload = match payload with | PValue (None, payload_type) -> let
     field_name = msg_field_name_from_type payload_type in let name_gen,
     field_name = Namegen.unique_name name_gen field_name in let
     payload_field_info = (VariableName.of_string field_name, payload_type)
     in (name_gen, payload_field_info :: named_fields) | _ -> acc *)

  (* let gen_named_payload_field_names ((name_gen, named_fields) as acc)
     payload = match payload with | PValue (Some name, payload_type) -> let
     field_name = msg_field_name name in let name_gen, field_name =
     Namegen.unique_name name_gen field_name in let payload_field_info =
     (VariableName.of_string field_name, payload_type) in (name_gen,
     payload_field_info :: named_fields) | _ -> acc *)

  (* let add_message_struct ((name_gen, msgs) as env) label payload = match
     Map.find msgs label with | None -> let struct_name = msg_type_name label
     in let name_gen, struct_name = Namegen.unique_name name_gen struct_name
     in let msg_struct_name = MessageStructName.of_string struct_name in let
     payload_name_gen = Namegen.create () in let payload_name_gen,
     payload_fields = List.fold ~init:(payload_name_gen, [])
     ~f:gen_named_payload_field_names payload in let _, payload_fields =
     List.fold ~init:(payload_name_gen, payload_fields)
     ~f:gen_unnamed_payload_field_names payload in let msg_struct_info =
     (msg_struct_name, payload_fields) in let msgs = Map.add_exn msgs
     ~key:label ~data:msg_struct_info in let env = (name_gen, msgs) in (env,
     msg_struct_name) | Some (msg_struct_name, _) -> (env, msg_struct_name) *)

  let add_message_enum ((enum_type, name_gen, enums) as env) label =
    if Map.mem enums label then env
    else
      let enum_name = msg_enum_name label in
      let name_gen, enum_name = Namegen.unique_name name_gen enum_name in
      ( enum_type
      , name_gen
      , Map.add_exn enums ~key:label ~data:(EnumName.of_string enum_name) )

  let add_invitation_enum ((enum_type, name_gen, enums) as env) protocol
      roles =
    let call_label = gen_call_label protocol roles in
    if Map.mem enums call_label then env
    else
      let enum_name = invitation_enum_name protocol roles in
      let name_gen, enum_name = Namegen.unique_name name_gen enum_name in
      ( enum_type
      , name_gen
      , Map.add_exn enums ~key:call_label
          ~data:(EnumName.of_string enum_name) )

  let get_invitation_enum (_, _, enums) protocol roles =
    let call_label = gen_call_label protocol roles in
    Map.find_exn enums call_label

  let get_message_enum (_, _, enums) label = Map.find_exn enums label

  let get_enum_type_name (enum_type, _, _) = enum_type

  let generate_messages_file (enum_name, _, enums) =
    let pkg_stmt = package_stmt pkg_messages in
    let label_enum_decl = gen_enum (enum_name, Map.data enums) in
    join_non_empty_lines ~sep:"\n\n" [pkg_stmt; label_enum_decl]

  (* let pkg_stmt = package_stmt pkg in let msg_structs = List.map
     ~f:gen_msg_struct (Map.data msgs) in join_non_empty_lines ~sep:"\n\n"
     (pkg_stmt :: msg_structs) *)

  let create protocol_name =
    let namegen = Namegen.create () in
    let enum_type = message_label_enum_name protocol_name in
    let namegen, _ =
      Namegen.unique_name namegen (EnumTypeName.user enum_type)
    in
    (enum_type, namegen, Map.empty (module LabelName))
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

  val get_or_add_channel :
       t
    -> RoleName.t * PayloadTypeName.t option * bool
    -> t * (ChannelName.t * string)

  val gen_channel_struct : t -> string

  val get_channel_imports : t -> ImportsEnv.t

  val struct_name : t -> ChannelStructName.t

  val create :
    RoleName.t -> ProtocolName.t -> ProtocolName.t -> ImportsEnv.t -> t
end = struct
  module ChannelId = struct
    module T = struct
      type t = RoleName.t * PayloadTypeName.t option * bool
      [@@deriving sexp_of, ord]
    end

    include T
    include Comparable.Make (T)
  end

  type channel_fields = (ChannelName.t * string) Map.M(ChannelId).t

  type t =
    Namegen.t
    * ChannelStructName.t
    * ProtocolName.t
    * ProtocolName.t
    * channel_fields
    * ImportsEnv.t

  let get_or_add_channel
      (( namegen
       , struct_name
       , entrypoint_protocol
       , protocol
       , chan_fields
       , imports ) as env :
        t ) (role, payload, is_send) =
    let chan_id = (role, payload, is_send) in
    match Map.find chan_fields chan_id with
    | Some channel_field_and_type -> (env, channel_field_and_type)
    | None ->
        let chan_field_name, field_type, imports =
          match payload with
          | None ->
              let imports, pkg = ImportsEnv.import_messages imports in
              let label_enum_type =
                message_label_enum_name entrypoint_protocol
              in
              let label_enum_type_str =
                pkg_enum_type_access pkg label_enum_type
              in
              ( chan_field_name role "Label" is_send
              , label_enum_type_str
              , imports )
          | Some payload_type ->
              (* TODO: Assumes payload type is valid identifier: e.g. string,
                 int. etc.*)
              ( chan_field_name role
                  (capitalize_payload_type payload_type)
                  is_send
              , PayloadTypeName.user payload_type
              , imports )
        in
        let field_type = chan_type field_type in
        let namegen, chan_field =
          Namegen.unique_name namegen chan_field_name
        in
        let channel_field = ChannelName.of_string chan_field in
        let chan_fields =
          Map.add_exn chan_fields ~key:chan_id
            ~data:(channel_field, field_type)
        in
        let env =
          ( namegen
          , struct_name
          , entrypoint_protocol
          , protocol
          , chan_fields
          , imports )
        in
        (env, (channel_field, field_type))

  (* let update_channel_entry is_send = function | None -> if is_send then
     (true, false) else (false, true) | send, recv -> if is_send then (true,
     recv) else (send, true)

     let new_label_channel (struct_name, protocol, (label_channels,
     payload_channels), imports) role is_send = let imports, _ =
     ImportsEnv.import_messages imports protocol in let label_channels =
     Map.update label_channels role ~f:(update_channel_entry is_send) in
     (struct_name, protocol, (label_channels, payload_channels), imports)

     let new_payload_channel (struct_name, protocol, (label_channels,
     payload_channels), imports) role payload is_send = let
     update_payload_entry =function | None -> Map.singleton (module
     PayloadTypeName) payload (update_channel_entry is_send None) | Some
     payload_chans -> Map.update payload ~f:(update_channel_entry is_send) in
     let payload_channels = Map.update payload_channels role
     ~f:(update_payload_entry) in (struct_name, ) *)

  (* let chan_name = chan_struct_field_name role msg_label in let name_gen,
     chan_name = Namegen.unique_name name_gen chan_name in let channel_name =
     ChannelName.of_string chan_name in let msg_struct_name =
     MessageStructName.of_string (msg_type_name msg_label) in let
     channel_fields = (channel_name, msg_struct_name) :: channel_fields in (*
     Add messages/protocol import if the role receives any message *) let
     imports, _ = ImportsEnv.import_messages imports protocol in let env =
     (name_gen, struct_name, protocol, channel_fields, imports) in (env,
     channel_name, msg_struct_name) *)

  let gen_channel_struct ((_, struct_name, _, _, channel_fields, _) : t) =
    let gen_chan_field_decl ~key:_ ~data:(chan_name, chan_type) chan_fields =
      (* Get pkg name *)
      struct_field_decl (ChannelName.user chan_name) chan_type :: chan_fields
    in
    let chan_decls =
      Map.fold ~init:[] ~f:gen_chan_field_decl channel_fields
    in
    struct_decl (ChannelStructName.user struct_name) chan_decls

  let struct_name (_, struct_name, _, _, _, _) = struct_name

  let get_channel_imports (_, _, _, _, _, imports) = imports

  let create role entrypoint_protocol protocol imports : t =
    let struct_name = ChannelStructName.of_string @@ chan_struct_name role in
    ( Namegen.create ()
    , struct_name
    , entrypoint_protocol
    , protocol
    , Map.empty (module ChannelId)
    , imports )
end

module InviteEnv : sig
  type t

  type role_channel_field =
    InviteChannelName.t * (PackageName.t * ChannelStructName.t)

  type role_invite_channel_field =
    InviteChannelName.t * InviteChannelStructName.t

  (* val add_send_role_channel : t -> RoleName.t -> LocalProtocolName.t ->
     RoleName.t -> ProtocolName.t -> t * InviteChannelName.t *
     ChannelStructName.t

     val add_send_invite_channel : t -> RoleName.t -> LocalProtocolName.t ->
     t * InviteChannelName.t * InviteChannelStructName.t

     val send_self_role_channel : t -> RoleName.t -> LocalProtocolName.t ->
     InviteChannelName.t * ChannelStructName.t

     val send_self_invite_channel : t -> RoleName.t -> LocalProtocolName.t ->
     InviteChannelName.t * InviteChannelStructName.t

     val add_recv_role_channel : t -> RoleName.t -> LocalProtocolName.t ->
     RoleName.t -> ProtocolName.t -> t * InviteChannelName.t *
     ChannelStructName.t

     val add_recv_invite_channel : t -> RoleName.t -> LocalProtocolName.t ->
     t * InviteChannelName.t * InviteChannelStructName.t *)

  val get_or_add_send_invitation_channels :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * role_channel_field * role_invite_channel_field

  val get_or_add_recv_invitation_channels :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * role_channel_field * role_invite_channel_field

  val gen_invite_channel_struct : t -> string

  val get_invite_imports : t -> ImportsEnv.t

  val struct_name : t -> InviteChannelStructName.t

  val create : LocalProtocolName.t -> ImportsEnv.t -> t
end = struct
  module InvitationId = struct
    module T = struct
      type t = RoleName.t option * RoleName.t option * LocalProtocolName.t
      [@@deriving sexp_of, ord]
    end

    include T
    include Comparable.Make (T)
  end

  type role_channel_field =
    InviteChannelName.t * (PackageName.t * ChannelStructName.t)

  type role_invite_channel_field =
    InviteChannelName.t * InviteChannelStructName.t

  type invitation_channel_fields =
    (role_channel_field * role_invite_channel_field) Map.M(InvitationId).t

  type t =
    Namegen.t
    * InviteChannelStructName.t
    * invitation_channel_fields
    * ImportsEnv.t

  let new_role_channel_field namegen imports channel_name new_role protocol =
    let namegen, channel_name = Namegen.unique_name namegen channel_name in
    let invite_chan = InviteChannelName.of_string channel_name in
    let imports, pkg = ImportsEnv.import_channels imports protocol in
    let role_chan_name =
      ChannelStructName.of_string @@ chan_struct_name new_role
    in
    (namegen, imports, (invite_chan, (pkg, role_chan_name)))

  let new_invite_channel_field namegen channel_name local_protocol =
    let namegen, channel_name = Namegen.unique_name namegen channel_name in
    let invite_chan = InviteChannelName.of_string channel_name in
    let invite_struct = invite_struct_name local_protocol in
    let chan_type = InviteChannelStructName.of_string invite_struct in
    (namegen, (invite_chan, chan_type))

  let get_or_add_send_invitation_channels
      ((namegen, struct_name, invite_chan_fields, imports) as env)
      participant local_protocol new_role protocol =
    let invitation_key = (None, Some participant, local_protocol) in
    match Map.find invite_chan_fields invitation_key with
    | Some (role_chan, role_invite_chan) -> (env, role_chan, role_invite_chan)
    | None ->
        let role_channel_name =
          send_role_chan_name participant local_protocol
        in
        let invite_channel_name =
          send_role_invite_chan_name participant local_protocol
        in
        let namegen, imports, role_chan =
          new_role_channel_field namegen imports role_channel_name new_role
            protocol
        in
        let namegen, role_invite_chan =
          new_invite_channel_field namegen invite_channel_name local_protocol
        in
        let invite_chan_fields =
          Map.add_exn invite_chan_fields ~key:invitation_key
            ~data:(role_chan, role_invite_chan)
        in
        let env = (namegen, struct_name, invite_chan_fields, imports) in
        (env, role_chan, role_invite_chan)

  let get_or_add_recv_invitation_channels
      ((namegen, struct_name, invite_chan_fields, imports) as env) caller
      local_protocol new_role protocol =
    let invitation_key = (Some caller, None, local_protocol) in
    match Map.find invite_chan_fields invitation_key with
    | Some (role_chan, role_invite_chan) -> (env, role_chan, role_invite_chan)
    | None ->
        let role_channel_name = recv_role_chan_name caller local_protocol in
        let invite_channel_name =
          recv_role_invite_chan_name caller local_protocol
        in
        (* TODO: modify new_role_channel_field to return channel as tuple *)
        let namegen, imports, role_chan =
          new_role_channel_field namegen imports role_channel_name new_role
            protocol
        in
        let namegen, role_invite_chan =
          new_invite_channel_field namegen invite_channel_name local_protocol
        in
        let invite_chan_fields =
          Map.add_exn invite_chan_fields ~key:invitation_key
            ~data:(role_chan, role_invite_chan)
        in
        let env = (namegen, struct_name, invite_chan_fields, imports) in
        (env, role_chan, role_invite_chan)

  let gen_invite_channel_struct (_, struct_name, invite_chan_fields, _) =
    let chan_fields, invite_fields =
      List.unzip @@ Map.data invite_chan_fields
    in
    let role_chan_decls = List.map ~f:role_chan_field_decl chan_fields in
    let invite_chan_decls =
      List.map ~f:invite_chan_field_decl invite_fields
    in
    struct_decl
      (InviteChannelStructName.user struct_name)
      (role_chan_decls @ invite_chan_decls)

  let struct_name (_, struct_name, _, _) = struct_name

  let get_invite_imports (_, _, _, imports) = imports

  let create local_protocol imports =
    let struct_name =
      InviteChannelStructName.of_string @@ invite_struct_name local_protocol
    in
    (Namegen.create (), struct_name, Map.empty (module InvitationId), imports)
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

  type setup_functions_env = FunctionName.t Map.M(ProtocolName).t

  type t =
    { role_channels: role_chan_field Map.M(ProtocolName).t
    ; invite_channels: invite_chan_field Map.M(ProtocolName).t
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
          role_chan_field_decl (chan_field, (channels_pkg, chan_struct)) )
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
    -> ProtocolName.t
    -> RoleName.t list
    -> ProtocolName.t
    -> local_proto_name_lookup
    -> t

  val generate_setup_channels :
       global_t
    -> local_proto_name_lookup
    -> t
    -> ImportsEnv.t
       * VariableName.t Map.M(RoleName).t
       * VariableName.t Map.M(RoleName).t
       * string
end = struct
  type setup_channels = VariableName.t Map.M(ChannelName).t Map.M(RoleName).t

  type setup_invite_channels =
    VariableName.t Map.M(InviteChannelName).t Map.M(RoleName).t

  type channel_envs = ChannelEnv.t Map.M(RoleName).t

  type invite_envs = InviteEnv.t Map.M(RoleName).t

  module ChanVarKey = struct
    module T = struct
      type t = RoleName.t * RoleName.t * PayloadTypeName.t option
      [@@deriving sexp_of, ord]
    end

    include T
    include Comparable.Make (T)
  end

  module InviteChanVarKey = struct
    module T = struct
      type t = RoleName.t * RoleName.t * LocalProtocolName.t
      [@@deriving sexp_of, ord]
    end

    include T
    include Comparable.Make (T)
  end

  type setup_env =
    { channel_envs: channel_envs
    ; invite_envs: invite_envs
    ; chan_vars: (VariableName.t * string) list
    ; data_chan_vars: VariableName.t Map.M(ChanVarKey).t
    ; invite_chan_vars:
        (VariableName.t * VariableName.t) Map.M(InviteChanVarKey).t
    ; setup_channels: setup_channels
    ; setup_invite_channels: setup_invite_channels }

  type t = ProtocolName.t * ImportsEnv.t * Namegen.t * setup_env

  let init_channel_envs ({channel_envs; _} as env) entrypoint_protocol roles
      protocol imports =
    let channel_envs =
      List.fold roles ~init:channel_envs ~f:(fun acc role ->
          let chan_env =
            ChannelEnv.create role entrypoint_protocol protocol imports
          in
          Map.add_exn acc ~key:role ~data:chan_env )
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
          Map.add_exn acc ~key:role ~data:chan_env )
    in
    {env with invite_envs}

  let init_setup_channels ({setup_channels; _} as env) roles =
    let setup_channels =
      List.fold roles ~init:setup_channels ~f:(fun acc role ->
          Map.add_exn acc ~key:role ~data:(Map.empty (module ChannelName)) )
    in
    {env with setup_channels}

  let init_setup_invite_channels ({setup_invite_channels; _} as env) roles =
    let setup_invite_channels =
      List.fold roles ~init:setup_invite_channels ~f:(fun acc role ->
          Map.add_exn acc ~key:role
            ~data:(Map.empty (module InviteChannelName)) )
    in
    {env with setup_invite_channels}

  let create_empty_env root_dir protocol : t =
    let var_name_gen = Namegen.create () in
    let setup_channels = Map.empty (module RoleName) in
    let setup_invite_channels = Map.empty (module RoleName) in
    let invite_envs = Map.empty (module RoleName) in
    let channel_envs = Map.empty (module RoleName) in
    let data_chan_vars = Map.empty (module ChanVarKey) in
    let invite_chan_vars = Map.empty (module InviteChanVarKey) in
    let chan_vars = [] in
    let setup_env =
      { channel_envs
      ; invite_envs
      ; chan_vars
      ; data_chan_vars
      ; invite_chan_vars
      ; setup_channels
      ; setup_invite_channels }
    in
    let imports = ImportsEnv.create root_dir in
    (protocol, imports, var_name_gen, setup_env)

  let create root_dir entrypoint_protocol roles protocol protocol_lookup =
    let protocol, imports, var_name_gen, setup_env =
      create_empty_env root_dir protocol
    in
    let dummy_imports = ImportsEnv.create root_dir in
    let setup_env =
      init_channel_envs setup_env entrypoint_protocol roles protocol
        dummy_imports
    in
    let setup_env =
      init_invite_envs setup_env roles protocol protocol_lookup dummy_imports
    in
    let setup_env = init_setup_channels setup_env roles in
    let setup_env = init_setup_invite_channels setup_env roles in
    (protocol, imports, var_name_gen, setup_env)

  (* let new_channel_vars (protocol, imports, var_name_gen, ({chan_vars; _}
     as setup_env)) sender recv label payloads = let update_channel_envs
     imports ({channel_envs; setup_channels; _} as setup_env) role1 role2
     chan_var = let imports, msgs_pkg = ImportsEnv.import_messages imports in
     let channel_env = Map.find_exn channel_envs role1 in let channel_env,
     channel_fields = ChannelEnv.get_or_add_channel channel_env role2 label
     in let channel_envs = Map.update channel_envs role1 ~f:(fun _ ->
     channel_env) in let role_setup_channels = Map.find_exn setup_channels
     role1 in let role_setup_channels = Map.update role_setup_channels
     channel_name ~f:(fun _ -> chan_var) in let setup_channels = Map.update
     setup_channels role1 ~f:(fun _ -> role_setup_channels) in let
     channel_type_name = chan_type (protocol_msg_access msgs_pkg msg_struct)
     in ( imports , {setup_env with channel_envs; setup_channels} ,
     channel_type_name ) in let msg_chan_var = new_msg_chan_var sender recv
     label in let var_name_gen, msg_chan_var = Namegen.unique_name
     var_name_gen msg_chan_var in let msg_chan_var_name =
     VariableName.of_string msg_chan_var in let imports, setup_env, var_type
     = update_channel_envs imports setup_env sender recv msg_chan_var_name in
     let imports, setup_env, _ = update_channel_envs imports setup_env recv
     sender msg_chan_var_name in let chan_vars = (msg_chan_var_name,
     var_type) :: chan_vars in let setup_env = {setup_env with chan_vars} in
     (protocol, imports, var_name_gen, setup_env) *)

  let create_channels_for_interaction
      (protocol, imports, var_name_gen, setup_env) sender recv payloads =
    let add_channel_for_role channel_envs setup_channels role1 chan_key
        chan_var =
      let channel_env = Map.find_exn channel_envs role1 in
      let channel_env, (channel_name, channel_type) =
        ChannelEnv.get_or_add_channel channel_env chan_key
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
      (channel_envs, setup_channels, channel_type)
    in
    (* None payload type represents the protocol label payload type *)
    let all_payloads = None :: List.map payloads ~f:(fun p -> Some p) in
    let imports, var_name_gen, setup_env =
      List.fold all_payloads ~init:(imports, var_name_gen, setup_env)
        ~f:(fun
             ( imports
             , var_name_gen
             , ( {chan_vars; data_chan_vars; channel_envs; setup_channels; _}
               as setup_env ) )
             payload
           ->
          let imports, payload_type =
            match payload with
            | None ->
                let imports, _ = ImportsEnv.import_messages imports in
                (imports, None)
            | Some (PValue (_, p_type)) ->
                (imports, Some (Expr.payload_typename_of_payload_type p_type))
            | Some (PDelegate _) -> Err.violation "Delegation not supported"
          in
          let chan_key = (sender, recv, payload_type) in
          if Map.mem data_chan_vars chan_key then
            (imports, var_name_gen, setup_env)
          else
            let chan_var_name = new_data_chan_var sender recv payload_type in
            let var_name_gen, chan_var_name =
              Namegen.unique_name var_name_gen chan_var_name
            in
            let chan_var = VariableName.of_string chan_var_name in
            let channel_envs, setup_channels, chan_var_type =
              add_channel_for_role channel_envs setup_channels sender
                (recv, payload_type, true)
                chan_var
            in
            let channel_envs, setup_channels, _ =
              add_channel_for_role channel_envs setup_channels recv
                (sender, payload_type, false)
                chan_var
            in
            let data_chan_vars =
              Map.add_exn data_chan_vars ~key:chan_key ~data:chan_var
            in
            let chan_vars = (chan_var, chan_var_type) :: chan_vars in
            ( imports
            , var_name_gen
            , { setup_env with
                chan_vars
              ; data_chan_vars
              ; channel_envs
              ; setup_channels } ) )
    in
    (protocol, imports, var_name_gen, setup_env)

  let gen_send_invite_channels protocol_lookup protocol env
      (participant, new_role) =
    let local_protocol =
      lookup_local_protocol protocol_lookup protocol new_role
    in
    InviteEnv.get_or_add_send_invitation_channels env participant
      local_protocol new_role protocol

  let gen_accept_invite_channels env curr_role caller new_role protocol
      local_protocol =
    if RoleName.equal caller curr_role then
      InviteEnv.get_or_add_send_invitation_channels env curr_role
        local_protocol new_role protocol
    else
      InviteEnv.get_or_add_recv_invitation_channels env caller local_protocol
        new_role protocol

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
      ( ( protocol
        , imports
        , var_name_gen
        , ({chan_vars; invite_chan_vars; _} as setup_env) ) as env ) caller
      participant new_role call_protocol protocol_lookup =
    let local_protocol =
      lookup_local_protocol protocol_lookup call_protocol new_role
    in
    let invite_key = (caller, participant, local_protocol) in
    if Map.mem invite_chan_vars invite_key then env
    else
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
      let setup_env, (_, role_chan_type), invite_chan_type =
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
        ::
        (invite_chan_var_name, chan_type invite_chan_type_str) :: chan_vars
      in
      let invite_chan_vars =
        Map.add_exn invite_chan_vars ~key:invite_key
          ~data:(role_chan_var_name, invite_chan_var_name)
      in
      let setup_env = {setup_env with chan_vars; invite_chan_vars} in
      if not (RoleName.equal caller participant) then
        (* Create Label channel for caller -> participant *)
        create_channels_for_interaction
          (protocol, imports, var_name_gen, setup_env)
          caller participant []
      else (protocol, imports, var_name_gen, setup_env)

  let generate_invitation_vars env caller roles new_roles call_protocol
      protocol_lookup =
    let zipped_roles = List.zip_exn roles new_roles in
    List.fold ~init:env
      ~f:(fun env (role, new_role) ->
        generate_invite_channel_vars env caller role new_role call_protocol
          protocol_lookup )
      zipped_roles

  let rec generate_channel_vars global_t protocol_lookup (env : t) = function
    | EndG | TVarG _ -> env
    | MuG (_, _, gtype) ->
        generate_channel_vars global_t protocol_lookup env gtype
    | MessageG ({payload; _}, sender, recv, gtype) ->
        let env = create_channels_for_interaction env sender recv payload in
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
          , VariableName.user value :: values ) )
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
          , VariableName.user value :: values ) )
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
          ; setup_invite_channels
          ; _ } ) =
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
             invitations_pkg )
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
    t -> LabelName.t -> payload list -> RoleName.t -> t * CallbackName.t

  val new_recv_callback :
    t -> LabelName.t -> payload list -> RoleName.t -> t * CallbackName.t

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
  type params =
    [ `Result of ParameterName.t * (PackageName.t * ResultName.t)
    | `Payloads of (ParameterName.t * PayloadTypeName.t) list ]

  type return_val =
    [ `Env of CallbacksEnvName.t
    | `Result of PackageName.t * ResultName.t
    | `Payloads of PayloadTypeName.t list
    | `Enum of EnumTypeName.t ]

  type callbacks = (CallbackName.t * params option * return_val option) list

  type enum = EnumTypeName.t * EnumName.t list

  type choice_enums = EnumNamesEnv.t * enum list

  type t = Namegen.t * choice_enums * callbacks * ImportsEnv.t

  let new_send_callback (name_gen, choice_enums, callbacks, imports)
      msg_label payloads recv_role =
    (* <msg_label>_To_<role>[_<uid>]() (payload_1, ... , payload_n)*)
    let callback_name = send_callback_name msg_label recv_role in
    let name_gen, callback_name =
      Namegen.unique_name name_gen callback_name
    in
    let callback = CallbackName.of_string callback_name in
    let payload_types =
      List.map payloads ~f:(function
        | PValue (_, payload_type) ->
            Expr.payload_typename_of_payload_type payload_type
        | PDelegate _ -> Err.violation "Delegation not supported" )
    in
    let return_val = Some (`Payloads payload_types) in
    let callbacks = (callback, None, return_val) :: callbacks in
    let env = (name_gen, choice_enums, callbacks, imports) in
    (env, callback)

  let new_recv_callback (name_gen, choice_enums, callbacks, imports)
      msg_label payloads sender_role =
    (* <msg_label>_From_<role>[_uid](p_1 payload_type_1, ..., p_n
       payload_type_n) *)
    let callback_name = recv_callback_name msg_label sender_role in
    let name_gen, callback_name =
      Namegen.unique_name name_gen callback_name
    in
    let callback = CallbackName.of_string callback_name in
    (* Assumes unique, valid parameter names (preprocessing already applied) *)
    let callback_params =
      List.map payloads ~f:(function
        | PValue (param_name, payload_type) ->
            ( var_to_param_name param_name
            , Expr.payload_typename_of_payload_type payload_type )
        | PDelegate _ -> Err.violation "Delegation not supported" )
    in
    let callbacks =
      (callback, Some (`Payloads callback_params), None) :: callbacks
    in
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
      (callback, Some (`Result (result_param, (pkg, result_name))), None)
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
    -> ProtocolName.t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RootDirName.t
    -> protocol_env
    -> t

  val get_or_add_channel :
    t -> RoleName.t * PayloadTypeName.t option * bool -> t * ChannelName.t

  val gen_channel_struct : t -> string

  val get_or_add_send_invitation_channels :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * InviteChannelName.t * InviteChannelName.t

  val get_or_add_recv_invitation_channels :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * InviteChannelName.t * InviteChannelName.t

  val gen_invite_channel_struct : t -> string

  val new_send_callback :
    t -> LabelName.t -> payload list -> RoleName.t -> t * CallbackName.t

  val new_recv_callback :
    t -> LabelName.t -> payload list -> RoleName.t -> t * CallbackName.t

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

  val import_messages : t -> t * PackageName.t

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

  let create entrypoint_protocol protocol role local_protocol root_dir
      {callback_enum_names; channel_imports; invite_imports} =
    let channel_env =
      ChannelEnv.create role entrypoint_protocol protocol channel_imports
    in
    let invite_env = InviteEnv.create local_protocol invite_imports in
    let callbacks_env = CallbacksEnv.create root_dir callback_enum_names in
    let role_imports = ImportsEnv.create root_dir in
    {protocol; role; channel_env; invite_env; callbacks_env; role_imports}

  let get_or_add_channel ({channel_env; _} as t) chan_key =
    let channel_env, (channel_name, _) =
      ChannelEnv.get_or_add_channel channel_env chan_key
    in
    ({t with channel_env}, channel_name)

  let gen_channel_struct {channel_env; _} =
    ChannelEnv.gen_channel_struct channel_env

  let get_or_add_send_invitation_channels ({invite_env; _} as env)
      participant local_protocol new_role protocol =
    let invite_env, (role_chan, _), (invite_chan, _) =
      InviteEnv.get_or_add_send_invitation_channels invite_env participant
        local_protocol new_role protocol
    in
    ({env with invite_env}, role_chan, invite_chan)

  let get_or_add_recv_invitation_channels ({invite_env; _} as env) caller
      local_protocol new_role protocol =
    let invite_env, (role_chan, _), (invite_chan, _) =
      InviteEnv.get_or_add_recv_invitation_channels invite_env caller
        local_protocol new_role protocol
    in
    ({env with invite_env}, role_chan, invite_chan)

  let gen_invite_channel_struct {invite_env; _} =
    InviteEnv.gen_invite_channel_struct invite_env

  let new_send_callback ({callbacks_env; _} as env) msg_label payloads
      recv_role =
    let callbacks_env, callback =
      CallbacksEnv.new_send_callback callbacks_env msg_label payloads
        recv_role
    in
    ({env with callbacks_env}, callback)

  let new_recv_callback ({callbacks_env; _} as env) msg_label payloads
      sender_role =
    let callbacks_env, callback =
      CallbacksEnv.new_recv_callback callbacks_env msg_label payloads
        sender_role
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

  let import_messages ({role_imports; _} as env) =
    let role_imports, pkg_alias = ImportsEnv.import_messages role_imports in
    ({env with role_imports}, pkg_alias)

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
