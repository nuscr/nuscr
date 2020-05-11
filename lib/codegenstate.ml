open Names
open! Base
open Ltype
open Printf
open Gtype

module UniqueNameGen : sig
  type t

  val create : unit -> t

  val unique_name : t -> string -> t * string

  val curr_unique_name : t -> string -> string option
end = struct
  type t = (string, int, String.comparator_witness) Map.t

  let create () = Map.empty (module String)

  let make_unique_name name uid =
    if uid < 2 then name else sprintf "%s_%d" name uid

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
    match Map.find uids name with
    | None -> None
    | Some uid -> Some (make_unique_name name uid)
end

(* GOLANG PACKAGE NAMES *)
let pkg_messages = PackageName.of_string "messages"

let pkg_channels = PackageName.of_string "channels"

let pkg_invitations = PackageName.of_string "invitations"

let pkg_callbacks = PackageName.of_string "callbacks"

let pkg_roles = PackageName.of_string "roles"

let pkg_results = PackageName.of_string "results"

let init_callback = CallbackName.of_string "Init"

let done_callback = CallbackName.of_string "Done"

let env_var = VariableName.of_string "env"

let invite_chan = VariableName.of_string "inviteChannels"

let role_chan = VariableName.of_string "roleChannels"

let wait_group = VariableName.of_string "wg"

let int_type = "int"

(* CODEGEN FUNCTIONS *)

let capitalize_role_name role = String.capitalize @@ RoleName.user role

let lowercase_role_name role = String.lowercase @@ RoleName.user role

let capitalize_local_protocol local_protocol =
  String.capitalize @@ LocalProtocolName.user local_protocol

let capitalize_protocol protocol =
  String.capitalize @@ ProtocolName.user protocol

let capitalize_label label = String.capitalize @@ LabelName.user label

let lowercase_label label = String.lowercase @@ LabelName.user label

let lowercase_local_protocol local_protocol =
  String.lowercase @@ LocalProtocolName.user local_protocol

let var_to_param var_name =
  ParameterName.of_string @@ VariableName.user var_name

(* MSGS *)
let msg_type_name msg = capitalize_label msg

let msg_field_name_from_type payload_type =
  String.capitalize @@ PayloadTypeName.user payload_type

let msg_field_name field_name =
  String.capitalize @@ VariableName.user field_name

(* CHANNELS *)
let chan_struct_field_name role msg_type =
  sprintf "%s_%s" (capitalize_role_name role) (msg_type_name msg_type)

let chan_struct_name role_name =
  sprintf "%s_Chan" (capitalize_role_name role_name)

(* INVITATIONS *)
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

(* let setup_chan_struct_name protocol = sprintf "%s_RoleSetupChan"
   (capitalize_protocol protocol)

   let setup_invite_struct_name protocol = sprintf "%s_InviteSetupChan"
   (capitalize_protocol protocol)

   let setup_role_chan_name role = sprintf "%s_Chan" (capitalize_role_name
   role)

   let setup_invite_chan_name role = sprintf "%s_InviteChan"
   (capitalize_role_name role) *)

(* CALLBACKS *)
let send_callback_name msg_label recv_role =
  sprintf "%s_To_%s" (msg_type_name msg_label)
    (capitalize_role_name recv_role)

let recv_callback_name msg_label recv_role =
  sprintf "%s_From_%s" (msg_type_name msg_label)
    (capitalize_role_name recv_role)

let protocol_setup_callback_name protocol =
  sprintf "%s_Setup" (capitalize_protocol protocol)

let convert_env_callback_name local_protocol =
  sprintf "To_%s_Env" (capitalize_local_protocol local_protocol)

let choice_callback_name role =
  sprintf "%s_Choice" (capitalize_role_name role)

let callbacks_env_name local_protocol =
  sprintf "%s_Env" (capitalize_local_protocol local_protocol)

let result_callback_name local_protocol =
  sprintf "ResultFrom_%s" (capitalize_local_protocol local_protocol)

let choice_enum_value local_protocol label =
  sprintf "%s_%s"
    (capitalize_local_protocol local_protocol)
    (capitalize_label label)

let choice_enum_type local_protocol =
  sprintf "%s_Choice" (capitalize_local_protocol local_protocol)

let msg_var_name msg_label = String.lowercase @@ LabelName.user msg_label

(* RESULTS *)
let result_struct_name role = sprintf "%s_Result" (capitalize_role_name role)

(* IMPORTS *)
let messages_import_path root pkg =
  sprintf "\"%s/%s/%s\"" (RootDirName.user root)
    (PackageName.user pkg_messages)
    (PackageName.user pkg)

let channels_import_path root pkg =
  sprintf "\"%s/%s/%s\"" (RootDirName.user root)
    (PackageName.user pkg_channels)
    (PackageName.user pkg)

let roles_import_path root pkg =
  sprintf "\"%s/%s\"" (RootDirName.user root) (PackageName.user pkg)

let invitations_import_path root pkg =
  sprintf "\"%s/%s\"" (RootDirName.user root) (PackageName.user pkg)

let callbacks_import_path root pkg =
  sprintf "\"%s/%s\"" (RootDirName.user root) (PackageName.user pkg)

let results_import_path root pkg =
  sprintf "\"%s/%s/%s\"" (RootDirName.user root)
    (PackageName.user pkg_results)
    (PackageName.user pkg)

let gen_import import_path = sprintf "import %s" import_path

let gen_aliased_import import_path alias =
  sprintf "import %s %s" alias import_path

(* PKGS *)
let protocol_pkg_name protocol =
  String.lowercase @@ ProtocolName.user protocol

(* GO CODE *)
let incr_indent curr_indent = sprintf "\t%s" curr_indent

let indent_line indent line = sprintf "%s%s" indent line

let package_stmt pkg_name = sprintf "package %s" (PackageName.user pkg_name)

let join_non_empty_lines ?(sep = "\n") lines =
  let lines = List.filter ~f:(fun line -> String.length line > 0) lines in
  String.concat ~sep lines

let struct_decl struct_name field_decls =
  let field_decls = List.sort field_decls ~compare:String.compare in
  let field_decls_str = join_non_empty_lines field_decls in
  sprintf "type %s struct {\n%s\n}" struct_name field_decls_str

let enum_type_decl enum_type =
  sprintf "type %s %s" (EnumTypeName.user enum_type) int_type

let enum_value_decl enum_value enum_type =
  sprintf "%s %s = iota"
    (EnumName.user enum_value)
    (EnumTypeName.user enum_type)

let chan_type type_str = sprintf "chan %s" type_str

let enum_decl enum_type enum_values =
  let enum_value_decls =
    match enum_values with
    | fst_value :: values ->
        let fst_decl = enum_value_decl fst_value enum_type in
        let decls = List.map ~f:EnumName.user values in
        String.concat ~sep:"\n\t" (fst_decl :: decls)
    | _ -> failwith "Enum decl should have at least one value"
  in
  sprintf "const (\n\t%s\n)" enum_value_decls

let protocol_result_access pkg result =
  sprintf "%s.%s" (PackageName.user pkg) (ResultName.user result)

let protocol_msg_access pkg msg =
  sprintf "%s.%s" (PackageName.user pkg) (MessageStructName.user msg)

let protocol_channel_access pkg channel =
  sprintf "%s.%s" (PackageName.user pkg) (ChannelStructName.user channel)

let var_type_decl var type_name = sprintf "%s %s" var type_name

let struct_field_decl field_name field_type =
  sprintf "\t%s" (var_type_decl field_name field_type)

let msg_field_decl (field_name, field_type) =
  struct_field_decl
    (VariableName.user field_name)
    (PayloadTypeName.user field_type)

let gen_msg_struct (msg_struct_name, fields) =
  let struct_name = MessageStructName.user msg_struct_name in
  let field_decls = List.map ~f:msg_field_decl fields in
  struct_decl struct_name field_decls

let role_chan_field_decl (chan_name, pkg, role_chan) =
  let type_of_channel = protocol_channel_access pkg role_chan in
  let channel_type = chan_type type_of_channel in
  struct_field_decl (InviteChannelName.user chan_name) channel_type

let invite_chan_field_decl (chan_name, invite_chan_struct) =
  let channel_type =
    chan_type (InviteChannelStructName.user invite_chan_struct)
  in
  struct_field_decl (InviteChannelName.user chan_name) channel_type

let callbacks_env_interface env_name callbacks =
  let gen_callback (callback_name, param, return_val) =
    let param_str =
      match param with
      | None -> ""
      | Some (param_name, `Result (pkg, result)) ->
          var_type_decl
            (ParameterName.user param_name)
            (protocol_result_access pkg result)
      | Some (param_name, `Msg (pkg, msg)) ->
          var_type_decl
            (ParameterName.user param_name)
            (protocol_msg_access pkg msg)
    in
    let return_type =
      match return_val with
      | None -> ""
      | Some (`Msg (pkg, msg)) -> protocol_msg_access pkg msg
      | Some (`Result (pkg, result)) -> protocol_result_access pkg result
      | Some (`Env env) -> CallbacksEnvName.user env
      | Some (`Enum enum) -> EnumTypeName.user enum
    in
    sprintf "%s(%s) %s"
      (CallbackName.user callback_name)
      param_str return_type
  in
  let str_callbacks = List.map ~f:gen_callback callbacks in
  let callbacks_str = String.concat ~sep:"\n\t" str_callbacks in
  sprintf "type %s interface {\n\t%s\n}"
    (CallbacksEnvName.user env_name)
    callbacks_str

(* let local_protocol_function_name local_protocol =
   capitalize_local_protocol local_protocol *)

let return_stmt return_val = sprintf "return %s" return_val

let call_callback env_var callback param =
  let param_name =
    match param with None -> "" | Some name -> ParameterName.user name
  in
  sprintf "%s.%s(%s)"
    (VariableName.user env_var)
    (CallbackName.user callback)
    param_name

let continue_stmt label = sprintf "continue %s" (TypeVariableName.user label)

let recursion_label rec_var = sprintf "%s:" (TypeVariableName.user rec_var)

let recursion_loop loop_body indent =
  sprintf "%sfor {\n%s\n%s}" indent loop_body indent

let recursion_impl label loop = sprintf "%s\n%s" label loop

let channel_struct_field_access chan_var chan_name =
  sprintf "%s.%s" (VariableName.user chan_var) (ChannelName.user chan_name)

let new_role_chan_var local_protocol =
  let role_chan =
    sprintf "%s_chan" (lowercase_local_protocol local_protocol)
  in
  VariableName.of_string role_chan

let new_role_invite_chan_var local_protocol =
  let invite_chan =
    sprintf "%s_inviteChan" (lowercase_local_protocol local_protocol)
  in
  VariableName.of_string invite_chan

let new_env_var local_protocol =
  let env_var = sprintf "%s_env" (lowercase_local_protocol local_protocol) in
  VariableName.of_string env_var

let new_result_var local_protocol =
  let result_var =
    sprintf "%s_result" (lowercase_local_protocol local_protocol)
  in
  VariableName.of_string result_var

let new_msg_var msg_label =
  let msg_var = sprintf "%s_msg" (lowercase_label msg_label) in
  VariableName.of_string msg_var

let new_choice_enum_var role =
  let enum_var = sprintf "%s_choice" (lowercase_role_name role) in
  VariableName.of_string enum_var

let invite_channel_struct_field_access chan_var chan_name =
  sprintf "%s.%s"
    (VariableName.user chan_var)
    (InviteChannelName.user chan_name)

let msg_from_channel chan_str = sprintf "<-%s" chan_str

let send_msg_over_channel chan_struct chan_field msg_var =
  let chan = channel_struct_field_access chan_struct chan_field in
  sprintf "%s <- %s" chan (VariableName.user msg_var)

let new_var_assignment var_name rhs =
  sprintf "%s := %s" (VariableName.user var_name) rhs

let recv_from_invite_chan var_name invite_chan_struct chan_field =
  let chan =
    invite_channel_struct_field_access invite_chan_struct chan_field
  in
  let recv_from_chan = msg_from_channel chan in
  new_var_assignment var_name recv_from_chan

let recv_from_msg_chan var_name chan_struct chan_field =
  let chan = channel_struct_field_access chan_struct chan_field in
  let recv_from_chan = msg_from_channel chan in
  new_var_assignment var_name recv_from_chan

let join_params params = String.concat ~sep:", " params

let call_local_protocol local_protocol wait_group_var role_chan_var
    invite_chan_var new_env_var =
  let params =
    [wait_group_var; role_chan_var; invite_chan_var; new_env_var]
  in
  let param_names = List.map ~f:VariableName.user params in
  sprintf "%s(%s)"
    (capitalize_local_protocol local_protocol)
    (join_params param_names)

let gen_case_stmt case_str = sprintf "case %s:" case_str

let gen_switch_stmt switch_var enum_values cases_impl indent =
  let gen_switch_case (case_stmt, impl) =
    sprintf "%s%s\n%s" indent case_stmt impl
  in
  let case_stmts =
    List.map ~f:(fun enum -> gen_case_stmt (EnumName.user enum)) enum_values
  in
  let cases_and_impl = List.zip_exn case_stmts cases_impl in
  let switch_cases = List.map ~f:gen_switch_case cases_and_impl in
  let switch_cases_str = join_non_empty_lines switch_cases in
  sprintf "%sswitch %s {\n%s\n%s}" indent
    (VariableName.user switch_var)
    switch_cases_str indent

let gen_select_case impl case_indent =
  match impl with
  | recv_stmt :: impl' ->
      let select_case = gen_case_stmt recv_stmt in
      let select_case = indent_line case_indent select_case in
      let impl_indent = incr_indent case_indent in
      let impl' = List.map ~f:(indent_line impl_indent) impl' in
      (select_case :: impl', impl_indent)
  | [] -> failwith "Cannot have empty choice branch"

let gen_select_stmt select_cases indent =
  (* Assume each of the implementations is of the form: case <recv>:\n <impl> *)
  let select_cases_str = join_non_empty_lines select_cases in
  sprintf "%sselect {\n%s\n%s}" indent select_cases_str indent

module ImportsEnv : sig
  type t

  val import_messages : t -> ProtocolName.t -> t * PackageName.t

  val import_channels : t -> ProtocolName.t -> t * PackageName.t

  val import_results : t -> ProtocolName.t -> t * PackageName.t

  (* These next 3 might be overkill *)
  val import_invitations : t -> t * PackageName.t

  val import_callbacks : t -> t * PackageName.t

  val import_roles : t -> t * PackageName.t

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
    ; roles: aliases }

  type t = UniqueNameGen.t * RootDirName.t * import_aliases

  let get_or_add_import_alias name_gen aliases pkg_name =
    let pkg_key = PackageName.of_string pkg_name in
    match Map.find aliases pkg_key with
    | Some alias -> (name_gen, aliases, alias)
    | None ->
        let name_gen, alias = UniqueNameGen.unique_name name_gen pkg_name in
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
    let joined_imports_list =
      List.map ~f:join_imports
        [ messages_imports
        ; channels_imports
        ; invitations_imports
        ; callbacks_imports
        ; results_imports
        ; roles_imports ]
    in
    let non_empty_imports =
      List.filter
        ~f:(fun import -> not (String.equal import ""))
        joined_imports_list
    in
    join_imports non_empty_imports

  let create root_dir =
    let empty_aliases () = Map.empty (module PackageName) in
    let name_gen = UniqueNameGen.create () in
    let imports =
      { messages= empty_aliases ()
      ; channels= empty_aliases ()
      ; results= empty_aliases ()
      ; invitations= empty_aliases ()
      ; callbacks= empty_aliases ()
      ; roles= empty_aliases () }
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
    UniqueNameGen.t
    * (LabelName.t, message_info, LabelName.comparator_witness) Map.t

  let gen_unnamed_payload_field_names ((name_gen, named_fields) as acc)
      payload =
    match payload with
    | PValue (None, payload_type) ->
        let field_name = msg_field_name_from_type payload_type in
        let name_gen, field_name =
          UniqueNameGen.unique_name name_gen field_name
        in
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
        let name_gen, field_name =
          UniqueNameGen.unique_name name_gen field_name
        in
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
          UniqueNameGen.unique_name name_gen struct_name
        in
        let msg_struct_name = MessageStructName.of_string struct_name in
        let payload_name_gen = UniqueNameGen.create () in
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

  let create () = (UniqueNameGen.create (), Map.empty (module LabelName))
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
  type t = UniqueNameGen.t * UniqueNameGen.t

  let create () =
    let enum_type_name_gen = UniqueNameGen.create () in
    let enum_name_gen = UniqueNameGen.create () in
    (enum_type_name_gen, enum_name_gen)

  let new_enum (enum_type_name_gen, enum_name_gen) local_protocol labels =
    let enum_name_of_label name_gen label =
      let enum_name = choice_enum_value local_protocol label in
      let name_gen, enum_name =
        UniqueNameGen.unique_name name_gen enum_name
      in
      (name_gen, EnumName.of_string enum_name)
    in
    let enum_type = choice_enum_type local_protocol in
    let enum_type_name_gen, enum_type =
      UniqueNameGen.unique_name enum_type_name_gen enum_type
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

  val new_channel : t -> RoleName.t -> LabelName.t -> t * ChannelName.t

  val gen_channel_struct : t -> RoleName.t -> string

  val get_channel_imports : t -> ImportsEnv.t

  val create : ProtocolName.t -> ImportsEnv.t -> t
end = struct
  type channel_fields = (ChannelName.t * MessageStructName.t) list

  type t = UniqueNameGen.t * ProtocolName.t * channel_fields * ImportsEnv.t

  let new_channel (name_gen, protocol, channel_fields, imports) role
      msg_label =
    let chan_name = chan_struct_field_name role msg_label in
    let name_gen, chan_name = UniqueNameGen.unique_name name_gen chan_name in
    let channel_name = ChannelName.of_string chan_name in
    let msg_struct_name =
      MessageStructName.of_string (msg_type_name msg_label)
    in
    let channel_fields = (channel_name, msg_struct_name) :: channel_fields in
    (* Add messages/protocol import if the role receives any message *)
    let imports, _ = ImportsEnv.import_messages imports protocol in
    let env = (name_gen, protocol, channel_fields, imports) in
    (env, channel_name)

  let gen_channel_struct ((_, protocol, channel_fields, imports) : t) role =
    let gen_chan_field_decl (chan_name, msg_struct) =
      (* Get pkg name *)
      let _, pkg = ImportsEnv.import_messages imports protocol in
      let msg_struct_type = protocol_msg_access pkg msg_struct in
      let chan_type = chan_type msg_struct_type in
      struct_field_decl (ChannelName.user chan_name) chan_type
    in
    let chan_decls = List.map ~f:gen_chan_field_decl channel_fields in
    struct_decl (chan_struct_name role) chan_decls

  let get_channel_imports (_, _, _, imports) = imports

  let create protocol imports =
    (UniqueNameGen.create (), protocol, [], imports)
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

  val get_invite_imports : t -> ImportsEnv.t

  val create : ImportsEnv.t -> t
end = struct
  type role_channel_fields =
    (InviteChannelName.t * PackageName.t * ChannelStructName.t) list

  type role_invite_channel_fields =
    (InviteChannelName.t * InviteChannelStructName.t) list

  type t =
    UniqueNameGen.t
    * role_channel_fields
    * role_invite_channel_fields
    * ImportsEnv.t

  let new_role_channel_field
      (name_gen, channel_fields, invite_channel_fields, imports) channel_name
      new_role protocol =
    let name_gen, channel_name =
      UniqueNameGen.unique_name name_gen channel_name
    in
    let invite_chan = InviteChannelName.of_string channel_name in
    let imports, pkg = ImportsEnv.import_channels imports protocol in
    let role_chan_name =
      ChannelStructName.of_string @@ chan_struct_name new_role
    in
    let env =
      ( name_gen
      , (invite_chan, pkg, role_chan_name) :: channel_fields
      , invite_channel_fields
      , imports )
    in
    (env, invite_chan)

  let new_invite_channel
      (name_gen, channel_fields, invite_channel_fields, imports) channel_name
      local_protocol =
    let name_gen, channel_name =
      UniqueNameGen.unique_name name_gen channel_name
    in
    let invite_chan = InviteChannelName.of_string channel_name in
    let invite_struct = invite_struct_name local_protocol in
    let chan_type = InviteChannelStructName.of_string invite_struct in
    let invite_channel_fields =
      (invite_chan, chan_type) :: invite_channel_fields
    in
    let env = (name_gen, channel_fields, invite_channel_fields, imports) in
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

  let send_self_role_channel (name_gen, _, _, _) role local_protocol =
    let channel_name = send_role_chan_name role local_protocol in
    let channel_name =
      UniqueNameGen.curr_unique_name name_gen channel_name
    in
    InviteChannelName.of_string (Option.value_exn channel_name)

  let send_self_invite_channel (name_gen, _, _, _) role local_protocol =
    let channel_name = send_role_invite_chan_name role local_protocol in
    let channel_name =
      UniqueNameGen.curr_unique_name name_gen channel_name
    in
    InviteChannelName.of_string (Option.value_exn channel_name)

  let new_recv_role_channel env caller local_protocol new_role protocol =
    let channel_name = recv_role_chan_name caller local_protocol in
    new_role_channel_field env channel_name new_role protocol

  let new_recv_invite_channel env caller local_protocol =
    let channel_name = recv_role_invite_chan_name caller local_protocol in
    new_invite_channel env channel_name local_protocol

  let gen_invite_channel_struct (_, channel_fields, invite_channel_fields, _)
      local_protocol =
    let role_chan_decls = List.map ~f:role_chan_field_decl channel_fields in
    let invite_chan_decls =
      List.map ~f:invite_chan_field_decl invite_channel_fields
    in
    let struct_name = invite_struct_name local_protocol in
    struct_decl struct_name (role_chan_decls @ invite_chan_decls)

  let get_invite_imports (_, _, _, imports) = imports

  let create imports = (UniqueNameGen.create (), [], [], imports)
end

(* module SetupChanEnv : sig type t

   val new_protocol_setup_channel_struct : t -> ProtocolName.t -> RoleName.t
   list -> t

   val new_protocol_setup_invite_struct : t -> local_proto_name_lookup ->
   ProtocolName.t -> RoleName.t list -> t

   val get_setup_channel_struct : t -> ProtocolName.t ->
   InviteChannelStructName.t * InviteChannelName.t list

   val get_setup_invite_struct : t -> ProtocolName.t ->
   InviteChannelStructName.t * InviteChannelName.t list

   val create : unit -> t end = struct type role_chan_type = PackageName.t *
   ChannelStructName.t

   type role_chan_field = InviteChannelStructName.t * (InviteChannelName.t *
   role_chan_type) list

   type invite_chan_field = InviteChannelStructName.t * (InviteChannelName.t
   * InviteChannelStructName.t) list

   type t = { role_channels: ( ProtocolName.t , role_chan_field ,
   ProtocolName.comparator_witness ) Map.t ; invite_channels: (
   ProtocolName.t , invite_chan_field , ProtocolName.comparator_witness )
   Map.t }

   let new_protocol_setup_channel_struct env protocol roles = let
   gen_field_decl pkg role = let chan_name = InviteChannelName.of_string @@
   setup_role_chan_name role in let chan_type = ChannelStructName.of_string
   @@ chan_struct_name role in (chan_name, (pkg, chan_type)) in let
   struct_name = InviteChannelStructName.of_string @@ setup_chan_struct_name
   protocol in let pkg = PackageName.of_string @@ protocol_pkg_name protocol
   in let chan_fields = List.map ~f:(gen_field_decl pkg) roles in let
   role_channels = Map.add_exn env.role_channels ~key:protocol
   ~data:(struct_name, chan_fields) in {env with role_channels}

   let new_protocol_setup_invite_struct env protocol_lookup protocol roles =
   let gen_field_decl role = let chan_name = InviteChannelName.of_string @@
   setup_invite_chan_name role in let local_protocol = lookup_local_protocol
   protocol_lookup protocol role in let chan_type =
   InviteChannelStructName.of_string @@ invite_struct_name local_protocol in
   (chan_name, chan_type) in let struct_name =
   InviteChannelStructName.of_string @@ setup_invite_struct_name protocol in
   let chan_fields = List.map ~f:gen_field_decl roles in let invite_channels
   = Map.add_exn env.invite_channels ~key:protocol ~data:(struct_name,
   chan_fields) in {env with invite_channels}

   let get_setup_channel_struct {role_channels; _} protocol = let
   struct_name, chan_fields = Map.find_exn role_channels protocol in let
   chan_names, _ = List.unzip chan_fields in (struct_name, chan_names)

   let get_setup_invite_struct {invite_channels; _} protocol = let
   struct_name, chan_fields = Map.find_exn invite_channels protocol in let
   chan_names, _ = List.unzip chan_fields in (struct_name, chan_names)

   let create () = { role_channels= Map.empty (module ProtocolName) ;
   invite_channels= Map.empty (module ProtocolName) } end *)

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
    t -> ProtocolName.t -> RoleName.t -> t * CallbackName.t

  val get_enum_names_env : t -> EnumNamesEnv.t

  val gen_callbacks_file : t -> LocalProtocolName.t -> string

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

  type t = UniqueNameGen.t * choice_enums * callbacks * ImportsEnv.t

  let new_send_callback (name_gen, choice_enums, callbacks, imports)
      msg_label recv_role protocol =
    (* <msg_struct>_To_<role>[_<uid>]() <pkg>.<msg_struct> *)
    let callback_name = send_callback_name msg_label recv_role in
    let name_gen, callback_name =
      UniqueNameGen.unique_name name_gen callback_name
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
      UniqueNameGen.unique_name name_gen callback_name
    in
    let callback = CallbackName.of_string callback_name in
    let msg_struct_name =
      MessageStructName.of_string (msg_type_name msg_label)
    in
    let imports, pkg = ImportsEnv.import_messages imports protocol in
    let param_name = ParameterName.of_string @@ msg_var_name msg_label in
    let param_decl = Some (param_name, `Msg (pkg, msg_struct_name)) in
    let callbacks = (callback, param_decl, None) :: callbacks in
    let env = (name_gen, choice_enums, callbacks, imports) in
    (env, callback)

  let new_protocol_setup_callback
      (name_gen, choice_enums, callbacks, imports) protocol =
    (* <protocol>_Setup[_uid]() *)
    let callback_name = protocol_setup_callback_name protocol in
    let name_gen, callback_name =
      UniqueNameGen.unique_name name_gen callback_name
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
      UniqueNameGen.unique_name name_gen callback_name
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
      UniqueNameGen.unique_name name_gen callback_name
    in
    let callback = CallbackName.of_string callback_name in
    let imports, pkg = ImportsEnv.import_results imports protocol in
    let result_name = ResultName.of_string @@ result_struct_name role in
    let result_param = ParameterName.of_string "result" in
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
      UniqueNameGen.unique_name name_gen callback_name
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
      protocol role =
    let callback_name = CallbackName.user done_callback in
    match UniqueNameGen.curr_unique_name name_gen callback_name with
    | None ->
        let name_gen, callback_name =
          UniqueNameGen.unique_name name_gen callback_name
        in
        let callback = CallbackName.of_string callback_name in
        let result_type = ResultName.of_string @@ result_struct_name role in
        let imports, pkg = ImportsEnv.import_results imports protocol in
        let callbacks =
          (callback, None, Some (`Result (pkg, result_type))) :: callbacks
        in
        let env = (name_gen, (enum_name_gen, enums), callbacks, imports) in
        (env, callback)
    | Some callback_name -> (env, CallbackName.of_string callback_name)

  let new_init_callback callbacks = (init_callback, None, None) :: callbacks

  let get_enum_names_env (_, (enum_names_env, _), _, _) = enum_names_env

  let gen_callbacks_file (_, (_, enums), callbacks, imports) local_protocol =
    (*TODO: Init() and Done() *)
    (* let gen_enum_type enum_type enum_values = *)
    let gen_enum (enum_type, enum_values) =
      let type_decl = enum_type_decl enum_type in
      let value_decls = enum_decl enum_type enum_values in
      join_non_empty_lines [type_decl; value_decls]
    in
    let pkg = package_stmt pkg_callbacks in
    let enum_decls = List.map ~f:gen_enum enums in
    let enum_decls_str = String.concat ~sep:"\n\n" enum_decls in
    let imports_str = ImportsEnv.generate_imports imports in
    let callbacks = new_init_callback callbacks in
    let env_name =
      CallbacksEnvName.of_string @@ callbacks_env_name local_protocol
    in
    let callbacks_interface = callbacks_env_interface env_name callbacks in
    join_non_empty_lines ~sep:"\n\n"
      [pkg; imports_str; enum_decls_str; callbacks_interface]

  let create root_dir enum_names_env =
    let name_gen = UniqueNameGen.create () in
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
    ProtocolName.t -> RoleName.t -> RootDirName.t -> protocol_env -> t

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
    t -> ProtocolName.t -> RoleName.t -> t * CallbackName.t

  val gen_result_struct : RoleName.t -> string

  val get_protocol_env : t -> protocol_env

  val gen_callbacks_file : t -> LocalProtocolName.t -> string
end = struct
  type t =
    { protocol: ProtocolName.t
    ; role: RoleName.t
    ; channel_env: ChannelEnv.t
    ; invite_env: InviteEnv.t
    ; callbacks_env: CallbacksEnv.t }

  let create protocol role root_dir
      {callback_enum_names; channel_imports; invite_imports} =
    let channel_env = ChannelEnv.create protocol channel_imports in
    let invite_env = InviteEnv.create invite_imports in
    let callbacks_env = CallbacksEnv.create root_dir callback_enum_names in
    {protocol; role; channel_env; invite_env; callbacks_env}

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

  let add_done_callback ({callbacks_env; _} as env) protocol role =
    let callbacks_env, callback =
      CallbacksEnv.add_done_callback callbacks_env protocol role
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

  let gen_callbacks_file {callbacks_env; _} local_protocol =
    CallbacksEnv.gen_callbacks_file callbacks_env local_protocol

  let get_protocol_env {callbacks_env; channel_env; invite_env; _} =
    { channel_imports= ChannelEnv.get_channel_imports channel_env
    ; invite_imports= InviteEnv.get_invite_imports invite_env
    ; callback_enum_names= CallbacksEnv.get_enum_names_env callbacks_env }
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

let extract_choice_labels ltypes =
  let extract_label = function
    | InviteCreateL (_, _, protocol, _) ->
        LabelName.of_string @@ ProtocolName.user protocol
    | SendL ({label; _}, _, _) -> label
    | _ ->
        failwith
          "First choice interaction should always be a Send or InviteCreate"
  in
  List.map ~f:extract_label ltypes

let gen_accept_impl local_protocol role_chan role_invite_chan accept_cb
    result_cb =
  (* <local_proto>_chan := <-inviteChan.<role_chan> *)
  let role_chan_var = new_role_chan_var local_protocol in
  let role_chan_assign =
    recv_from_invite_chan role_chan_var invite_chan role_chan
  in
  (* <local_proto>_invite_chan := <-inviteChan.<role_invite_chan> *)
  let role_invite_chan_var = new_role_invite_chan_var local_protocol in
  let invite_chan_assign =
    recv_from_invite_chan role_invite_chan_var invite_chan role_invite_chan
  in
  (* <local_protocol>_env := env.<accept_cb>() *)
  let new_env_var = new_env_var local_protocol in
  let new_env_assign =
    new_var_assignment new_env_var (call_callback env_var accept_cb None)
  in
  (* <local_protocol>_result := <local_protocol>(wg, <local_protocol>_chan,
     <local_protocol>_inviteChan, <local_protocol>_env) *)
  let result_var = new_result_var local_protocol in
  let protocol_call =
    call_local_protocol local_protocol wait_group role_chan_var
      role_invite_chan_var new_env_var
  in
  let result_assign = new_var_assignment result_var protocol_call in
  (* env.ResultFrom_<local_protocol>(<local_protocol>_result) *)
  let result_param = var_to_param result_var in
  let result_callback_call =
    call_callback env_var result_cb (Some result_param)
  in
  [ role_chan_assign
  ; invite_chan_assign
  ; new_env_assign
  ; result_assign
  ; result_callback_call ]

let gen_recv_impl msg_label msg_chan recv_cb =
  (* <msg> := <-roleChannels.<msg_chan> *)
  let msg_var = new_msg_var msg_label in
  let msg_assign = recv_from_msg_chan msg_var role_chan msg_chan in
  (* env.<msg>_From_<sender>(<msg>) *)
  let msg_param = var_to_param msg_var in
  let call_recv_callback = call_callback env_var recv_cb (Some msg_param) in
  [msg_assign; call_recv_callback]

let gen_send_impl msg_label msg_chan send_cb =
  (* <msg_var> := env.<msg>_To_<recv>() *)
  let msg_var = new_msg_var msg_label in
  let msg_assign =
    new_var_assignment msg_var (call_callback env_var send_cb None)
  in
  (* roleChannels.<msg_chan> <- <msg> *)
  let send_msg_stmt = send_msg_over_channel role_chan msg_chan msg_var in
  [msg_assign; send_msg_stmt]

(* let gen_invite_impl () = (* *) () *)

let gen_make_choice_impl role choice_cb choice_enums choice_impls indent =
  (* choice := env.<role>_Choice() *)
  (* switch choice {case <enum_val>: <impl>} *)
  let choice_enum_var = new_choice_enum_var role in
  let choice_enum_assign =
    new_var_assignment choice_enum_var (call_callback env_var choice_cb None)
  in
  let choice_enum_assign = indent_line indent choice_enum_assign in
  let choice_switch =
    gen_switch_stmt choice_enum_var choice_enums choice_impls indent
  in
  join_non_empty_lines [choice_enum_assign; choice_switch]

let gen_recv_choice_impl choice_impls indent =
  gen_select_stmt choice_impls indent

(* let uid_gen_of_local_type local_t = let add_local_proto_function
   ~key:local_protocol ~data:_ name_gen = let impl_function_name =
   local_protocol_function_name local_protocol in let name_gen, _ =
   UniqueNameGen.unique_name name_gen impl_function_name in name_gen in
   Map.fold ~init:(UniqueNameGen.create ()) ~f:add_local_proto_function
   local_t *)

(* gen_role_implementation protocol role local_protocol ltype -> env * string*)
(* gen_impl env ltype indent -> env string *)
(* TODO: Are recursion labels unique? *)
(* TODO code gen of ltype *)
let gen_role_implementation ltype_env global_t protocol_lookup protocol role
    (local_proto_name : LocalProtocolName.t) ltype =
  let rec gen_implementation indent is_recv_choice_msg env ltype =
    match ltype with
    | EndL ->
        let env, done_callback =
          LTypeCodeGenEnv.add_done_callback env protocol role
        in
        let call = call_callback env_var done_callback None in
        let impl = return_stmt call in
        (env, indent_line indent impl)
    | TVarL var ->
        let impl = continue_stmt var in
        (env, indent_line indent impl)
    | MuL (var, ltype') ->
        let loop_label = recursion_label var in
        let env, recursion_body =
          gen_implementation (incr_indent indent) false env ltype'
        in
        let loop = recursion_loop recursion_body indent in
        let impl = recursion_impl loop_label loop in
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
        let env, _ =
          LTypeCodeGenEnv.new_protocol_setup_callback env protocol'
        in
        let env, impl = gen_implementation indent false env ltype' in
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
        let env, accept_cb =
          LTypeCodeGenEnv.new_convert_env_callback env local_protocol
        in
        let env, result_cb =
          LTypeCodeGenEnv.new_protocol_result_callback env local_protocol
            protocol' new_role
        in
        let accept_impl =
          gen_accept_impl local_protocol role_channel invite_channel
            accept_cb result_cb
        in
        let accept_impl, cont_indent =
          if is_recv_choice_msg then gen_select_case accept_impl indent
          else (List.map ~f:(indent_line indent) accept_impl, indent)
        in
        let accept_impl_str = join_non_empty_lines accept_impl in
        let env, impl = gen_implementation cont_indent false env ltype' in
        (env, join_non_empty_lines ~sep:"\n\n" [accept_impl_str; impl])
    | ChoiceL (r, ltys) ->
        if RoleName.equal role r then
          let choice_labels = extract_choice_labels ltys in
          let env, choice_cb, choice_enums =
            LTypeCodeGenEnv.new_choice_callback env role choice_labels
              local_proto_name
          in
          let env, choice_impls =
            List.fold_map ~init:env
              ~f:(gen_implementation (incr_indent indent) false)
              ltys
          in
          let impl =
            gen_make_choice_impl role choice_cb choice_enums choice_impls
              indent
          in
          (env, impl)
        else
          let env, choice_impls =
            List.fold_map ~init:env
              ~f:(gen_implementation (incr_indent indent) true)
              ltys
          in
          let impl = gen_recv_choice_impl choice_impls indent in
          (env, impl)
    | RecvL ({label; _}, r, ltype') ->
        let env, channel_name = LTypeCodeGenEnv.new_channel env r label in
        let env, recv_cb =
          LTypeCodeGenEnv.new_recv_callback env label r protocol
        in
        let recv_impl = gen_recv_impl label channel_name recv_cb in
        let recv_impl, cont_indent =
          if is_recv_choice_msg then gen_select_case recv_impl indent
          else (List.map ~f:(indent_line indent) recv_impl, indent)
        in
        let recv_impl_str = join_non_empty_lines recv_impl in
        let env, impl = gen_implementation cont_indent false env ltype' in
        (env, join_non_empty_lines ~sep:"\n\n" [recv_impl_str; impl])
    | SendL ({label; _}, r, ltype') ->
        let env, channel_name = LTypeCodeGenEnv.new_channel env r label in
        let env, send_cb =
          LTypeCodeGenEnv.new_send_callback env label r protocol
        in
        let send_impl = gen_send_impl label channel_name send_cb in
        let send_impl = List.map ~f:(indent_line indent) send_impl in
        let send_impl_str = join_non_empty_lines send_impl in
        let env, impl = gen_implementation indent false env ltype' in
        (env, join_non_empty_lines ~sep:"\n\n" [send_impl_str; impl])
  in
  (* TODO: pass in as parameter *)
  let env, impl =
    gen_implementation (incr_indent "") false ltype_env ltype
  in
  let impl =
    sprintf "fun %s() {\n%s\n}"
      (LocalProtocolName.user local_proto_name)
      impl
  in
  (env, impl)

let gen_channels_file pkg_name roles_and_envs imports =
  let pkg = package_stmt pkg_name in
  let channel_imports = ImportsEnv.generate_imports imports in
  let channel_structs =
    List.map
      ~f:(fun (role, env) -> LTypeCodeGenEnv.gen_channel_struct env role)
      roles_and_envs
  in
  join_non_empty_lines ~sep:"\n\n" (pkg :: channel_imports :: channel_structs)

let gen_invitations_file protocol_lookup protocol roles_and_envs imports =
  let gen_invite_struct (role, env) =
    let local_protocol =
      lookup_local_protocol protocol_lookup protocol role
    in
    LTypeCodeGenEnv.gen_invite_channel_struct env local_protocol
  in
  let pkg_stmt = package_stmt pkg_invitations in
  let imports_str = ImportsEnv.generate_imports imports in
  let channel_structs = List.map ~f:gen_invite_struct roles_and_envs in
  join_non_empty_lines ~sep:"\n\n"
    (pkg_stmt :: imports_str :: channel_structs)

let gen_results_file pkg_name roles =
  let pkg = package_stmt pkg_name in
  let result_structs = List.map ~f:LTypeCodeGenEnv.gen_result_struct roles in
  join_non_empty_lines ~sep:"\n\n" (pkg :: result_structs)

type codegen_result =
  { messages: (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; channels: (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; invite_channels:
      (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; results: (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; impl:
      ( LocalProtocolName.t
      , string
      , LocalProtocolName.comparator_witness )
      Map.t
  ; callbacks:
      ( LocalProtocolName.t
      , string
      , LocalProtocolName.comparator_witness )
      Map.t }

let empty_result () : codegen_result =
  { messages= Map.empty (module ProtocolName)
  ; channels= Map.empty (module ProtocolName)
  ; results= Map.empty (module ProtocolName)
  ; invite_channels= Map.empty (module ProtocolName)
  ; impl= Map.empty (module LocalProtocolName)
  ; callbacks= Map.empty (module LocalProtocolName) }

let rec gen_message_structs msgs_env = function
  | EndG | TVarG _ -> msgs_env
  | MuG (_, g) | CallG (_, _, _, g) -> gen_message_structs msgs_env g
  | ChoiceG (_, gtypes) ->
      List.fold ~init:msgs_env ~f:gen_message_structs gtypes
  | MessageG ({label; payload}, _, _, g) ->
      let msgs_env, _ =
        MessagesEnv.add_message_struct msgs_env label payload
      in
      gen_message_structs msgs_env g

let gen_code root_dir (global_t : global_t) (local_t : local_t) =
  (* TODO: Move build_local_proto_name_lookup to codegen *)
  let protocol_lookup = build_local_proto_name_lookup local_t in
  let gen_protocol_role_implementation ~key:protocol ~data result =
    (* TODO: Imports *)
    let (roles, new_roles), _, gtype = data in
    let protocol_env =
      { channel_imports= ImportsEnv.create root_dir
      ; invite_imports= ImportsEnv.create root_dir
      ; callback_enum_names= EnumNamesEnv.create () }
    in
    let messages_env = MessagesEnv.create () in
    let messages_env = gen_message_structs messages_env gtype in
    let (result, protocol_env), envs =
      List.fold_map ~init:(result, protocol_env)
        ~f:(fun (({impl; callbacks; _} as result), protocol_env) role ->
          let local_protocol_id = LocalProtocolId.create protocol role in
          let _, ltype = Map.find_exn local_t local_protocol_id in
          let local_protocol =
            lookup_local_protocol protocol_lookup protocol role
          in
          let env =
            LTypeCodeGenEnv.create protocol role root_dir protocol_env
          in
          let env, protocol_impl =
            gen_role_implementation env global_t protocol_lookup protocol
              role local_protocol ltype
          in
          let callbacks_file =
            LTypeCodeGenEnv.gen_callbacks_file env local_protocol
          in
          let result =
            { result with
              impl= Map.add_exn impl ~key:local_protocol ~data:protocol_impl
            ; callbacks=
                Map.add_exn callbacks ~key:local_protocol
                  ~data:callbacks_file }
          in
          let protocol_env = LTypeCodeGenEnv.get_protocol_env env in
          ((result, protocol_env), env))
        (roles @ new_roles)
    in
    let pkg = PackageName.of_string @@ protocol_pkg_name protocol in
    let roles_and_envs = List.zip_exn roles envs in
    let messages_file =
      MessagesEnv.generate_messages_file messages_env pkg
    in
    let messages =
      Map.add_exn result.messages ~key:protocol ~data:messages_file
    in
    let chan_file =
      gen_channels_file pkg roles_and_envs protocol_env.channel_imports
    in
    let channels =
      Map.add_exn result.channels ~key:protocol ~data:chan_file
    in
    let invitations_file =
      gen_invitations_file protocol_lookup protocol roles_and_envs
        protocol_env.invite_imports
    in
    let invite_channels =
      Map.add_exn result.invite_channels ~key:protocol ~data:invitations_file
    in
    let results_file = gen_results_file pkg roles in
    let results =
      Map.add_exn result.results ~key:protocol ~data:results_file
    in
    {result with messages; channels; invite_channels; results}
  in
  Map.fold ~init:(empty_result ()) ~f:gen_protocol_role_implementation
    global_t

(* TODO: Remove *)
let _ =
  let imports = ImportsEnv.create (RootDirName.of_string "Test") in
  let _ = ImportsEnv.import_roles imports in
  let _ = ImportsEnv.import_callbacks imports in
  let _ = ImportsEnv.import_invitations imports in
  ()
