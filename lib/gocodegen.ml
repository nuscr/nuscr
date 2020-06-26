open! Base
open Ltype
open Goenvs
open Names
open Goimpl
open Gonames
open Gtype

(** Validate protocols to ensure Go implementation can be generated

    For current code generation scheme:

    - protocol names must be unique when lowercased
    - role names must be unique when lowercased
    - msg labels must be unique when capitalised (can't have m1() and M1())
      and have the same payload whenever they are used within the same
      protocol
    - payload field names must be unique when capitalised *)
let ensure_unique_identifiers (global_t : global_t) =
  let add_unique_protocol_name protocols protocol_name =
    let name_str = protocol_pkg_name protocol_name in
    if Set.mem protocols name_str then
      raise (Err.Violation "Protocol names must be unique when lowercased") ;
    Set.add protocols name_str
  in
  let add_unique_role_name roles role =
    let role_str = lowercase_role_name role in
    if Set.mem roles role_str then
      raise (Err.Violation "Role names must be unique when lowercased") ;
    Set.add roles role_str
  in
  (* Ensure message labels are unique when capitalised *)
  let add_consistent_msg msgs {Gtype.label; payload} =
    let add_unique_payload_field fields payload =
      match payload with
      | PValue (None, _) -> fields
      | PValue (Some field_name, _) ->
          let field_str = msg_field_name field_name in
          if Set.mem fields field_str then
            Err.uerr
              (Err.DuplicatePayloadField
                 (label, VariableName.of_string field_str)) ;
          Set.add fields field_str
      | PDelegate _ -> raise (Err.Violation "Delegation not supported")
    in
    let label_str = msg_type_name label in
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
          raise
            (Err.Violation
               "Message labels must be unique when capitalized cased") ;
        if not (List.equal equal_pvalue_payload payload payload') then
          raise
            (Err.Violation
               "Within a protocol, messages with the same label should  \
                have the same payloads") ;
        msgs
  in
  let validate_protocol ~key ~data protocol_names =
    let rec validate_protocol_msgs messages gtype =
      match gtype with
      | EndG | TVarG _ -> messages
      | MuG (_, g) | CallG (_, _, _, g) -> validate_protocol_msgs messages g
      | ChoiceG (_, gtypes) ->
          List.fold ~init:messages ~f:validate_protocol_msgs gtypes
      | MessageG (msg, _, _, g) ->
          let messages = add_consistent_msg messages msg in
          validate_protocol_msgs messages g
    in
    let protocol_names = add_unique_protocol_name protocol_names key in
    let (roles, new_roles), _, gtype = data in
    let _ =
      List.fold
        ~init:(Set.empty (module String))
        ~f:add_unique_role_name (roles @ new_roles)
    in
    let _ = validate_protocol_msgs (Map.empty (module String)) gtype in
    protocol_names
  in
  let _ =
    Map.fold ~init:(Set.empty (module String)) ~f:validate_protocol global_t
  in
  ()

(** Generate the struct field names for the channels over which to send the
    invitations to a role *)
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

(** Generate the struct field names for the channels over which to accept the
    invitations from the caller role *)
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

(** Extract labels from the first interactions in each local type. These
    labels will not necessarily be unique in the case of protocol calls *)
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

(** Generate implementation of accept local type *)
let gen_accept_impl var_name_gen local_protocol role_chan role_invite_chan
    accept_cb result_cb =
  (* <local_proto>_chan := <-inviteChan.<role_chan> *)
  let role_chan_var = new_role_chan_var local_protocol in
  let var_name_gen, role_chan_var_name =
    new_variable var_name_gen role_chan_var
  in
  let role_chan_assign =
    recv_from_invite_chan role_chan_var_name invite_chan role_chan
  in
  (* <local_proto>_invite_chan := <-inviteChan.<role_invite_chan> *)
  let role_invite_chan_var = new_role_invite_chan_var local_protocol in
  let var_name_gen, role_invite_chan_var_name =
    new_variable var_name_gen role_invite_chan_var
  in
  let invite_chan_assign =
    recv_from_invite_chan role_invite_chan_var_name invite_chan
      role_invite_chan
  in
  (* <local_protocol>_env := env.<accept_cb>() *)
  let new_env_var = new_env_var local_protocol in
  let var_name_gen, new_env_var_name =
    new_variable var_name_gen new_env_var
  in
  let accept_function =
    FunctionName.of_string @@ CallbackName.user accept_cb
  in
  let new_env_assign =
    new_var_assignment new_env_var_name
      (call_method env_var accept_function [])
  in
  (* <local_protocol>_result := <local_protocol>(wg, <local_protocol>_chan,
     <local_protocol>_inviteChan, <local_protocol>_env) *)
  let result_var = new_result_var local_protocol in
  let var_name_gen, result_var_name = new_variable var_name_gen result_var in
  let role_function = local_protocol_function_name local_protocol in
  let params =
    [ wait_group
    ; role_chan_var_name
    ; role_invite_chan_var_name
    ; new_env_var_name ]
  in
  let protocol_call = call_function role_function params in
  let result_assign = new_var_assignment result_var_name protocol_call in
  (* env.ResultFrom_<local_protocol>(<local_protocol>_result) *)
  let result_function =
    FunctionName.of_string @@ CallbackName.user result_cb
  in
  let result_callback_call =
    call_method env_var result_function [VariableName.user result_var_name]
  in
  ( [ role_chan_assign
    ; invite_chan_assign
    ; new_env_assign
    ; result_assign
    ; result_callback_call ]
  , var_name_gen )

(** Generate implementation of receive local type *)
let gen_recv_impl var_name_gen msg_label msg_chan recv_cb =
  (* <msg> := <-roleChannels.<msg_chan> *)
  let msg_var = new_msg_var msg_label in
  let var_name_gen, msg_var_name = new_variable var_name_gen msg_var in
  let msg_assign = recv_from_msg_chan msg_var_name role_chan msg_chan in
  (* env.<msg>_From_<sender>(<msg>) *)
  let recv_function = FunctionName.of_string @@ CallbackName.user recv_cb in
  let call_recv_callback =
    call_method env_var recv_function [VariableName.user msg_var_name]
  in
  ([msg_assign; call_recv_callback], var_name_gen)

(** Generate implementation of send local type *)
let gen_send_impl var_name_gen msg_label msg_chan send_cb =
  (* <msg_var> := env.<msg>_To_<recv>() *)
  let msg_var = new_msg_var msg_label in
  let var_name_gen, msg_var_name = new_variable var_name_gen msg_var in
  let send_function = FunctionName.of_string @@ CallbackName.user send_cb in
  let msg_assign =
    new_var_assignment msg_var_name (call_method env_var send_function [])
  in
  (* roleChannels.<msg_chan> <- <msg> *)
  let send_msg_stmt =
    send_msg_over_channel role_chan msg_chan msg_var_name
  in
  ([msg_assign; send_msg_stmt], var_name_gen)

(** Generate implementation of invite + create local types *)
let gen_invite_impl var_name_gen protocol invite_pkg setup_env
    invite_channels setup_cb indent =
  (* env.<protocol>_Setup() *)
  let setup_function =
    FunctionName.of_string @@ CallbackName.user setup_cb
  in
  let setup_callback_call = call_method env_var setup_function [] in
  let role_channels, invite_channels = List.unzip invite_channels in
  (* <protocol>_rolechan := <protocol>_RoleSetupChan{<role1>_Chan:
     roleChannels.<role_channel>, ...} *)
  let role_struct, role_struct_fields =
    ProtocolSetupEnv.get_setup_channel_struct setup_env protocol
  in
  let str_role_channels =
    List.map
      ~f:(invite_channel_struct_field_access invite_chan)
      role_channels
  in
  let str_role_struct_fields =
    List.map ~f:InviteChannelName.user role_struct_fields
  in
  let role_struct_var = new_role_setup_var protocol in
  let var_name_gen, role_struct_var_name =
    new_variable var_name_gen role_struct_var
  in
  let role_struct_literal =
    struct_literal
      (invitation_pkg_access invite_pkg role_struct)
      str_role_struct_fields str_role_channels indent
  in
  let role_struct_assign =
    new_var_assignment role_struct_var_name role_struct_literal
  in
  (* <protocol>_invitechan := <protocol>_InviteSetupChan{<role1>_InviteChan:
     inviteChannels.<role_invitechannel>, ...} *)
  let invite_struct, invite_struct_fields =
    ProtocolSetupEnv.get_setup_invite_struct setup_env protocol
  in
  let str_invite_channels =
    List.map
      ~f:(invite_channel_struct_field_access invite_chan)
      invite_channels
  in
  let str_invite_struct_fields =
    List.map ~f:InviteChannelName.user invite_struct_fields
  in
  let invite_struct_var = new_invite_setup_var protocol in
  let var_name_gen, invite_struct_var_name =
    new_variable var_name_gen invite_struct_var
  in
  let invite_struct_literal =
    struct_literal
      (invitation_pkg_access invite_pkg invite_struct)
      str_invite_struct_fields str_invite_channels indent
  in
  let invite_struct_assign =
    new_var_assignment invite_struct_var_name invite_struct_literal
  in
  (* <protocol>_SendCommChannels(wg, <protocol>_rolechan,
     <protocol>_invitechan) *)
  let protocol_setup_function =
    ProtocolSetupEnv.get_setup_function_name setup_env protocol
  in
  let setup_params =
    [wait_group; role_struct_var_name; invite_struct_var_name]
  in
  let protocol_setup_call =
    call_function (FunctionName.user protocol_setup_function) setup_params
  in
  ( [ setup_callback_call
    ; role_struct_assign
    ; invite_struct_assign
    ; protocol_setup_call ]
  , var_name_gen )

(** Generate implementation of choice local type when the current role is
    making the choice*)
let gen_make_choice_impl var_name_gen role callbacks_pkg choice_cb
    choice_enums choice_impls indent =
  (* choice := env.<role>_Choice() *)
  (* switch choice {case <enum_val>: <impl>} *)
  let choice_enum_var = new_choice_enum_var role in
  let var_name_gen, choice_enum_var_name =
    new_variable var_name_gen choice_enum_var
  in
  let choice_function =
    FunctionName.of_string @@ CallbackName.user choice_cb
  in
  let choice_enum_assign =
    new_var_assignment choice_enum_var_name
      (call_method env_var choice_function [])
  in
  let choice_enum_assign = indent_line indent choice_enum_assign in
  let invalid_choice_panic = panic_with_msg invalid_choice_panic_msg in
  let invalid_choice_panic =
    indent_line (incr_indent indent) invalid_choice_panic
  in
  let default_impl = Some invalid_choice_panic in
  let choice_switch =
    gen_switch_stmt choice_enum_var_name callbacks_pkg choice_enums
      choice_impls indent default_impl
  in
  (join_non_empty_lines [choice_enum_assign; choice_switch], var_name_gen)

(** Generate implementation of choice local type when the current role is not
    making the choice*)
let gen_recv_choice_impl choice_impls indent =
  gen_select_stmt choice_impls indent

(** Generate implementation of end local type *)
let gen_end_of_protocol indent is_dynamic_role done_cb =
  let done_function = FunctionName.of_string @@ CallbackName.user done_cb in
  let call = call_method env_var done_function [] in
  let impl =
    if is_dynamic_role then
      let impl_stmts =
        List.map [call; return_stmt ""] ~f:(indent_line indent)
      in
      join_non_empty_lines impl_stmts
    else indent_line indent (return_stmt call)
  in
  impl

(** Generate source file containing the channel struct declarations for a
    protocol *)
let gen_channels_file pkg_name envs imports =
  let pkg = package_stmt pkg_name in
  let channel_imports = ImportsEnv.generate_imports imports in
  let channel_structs =
    List.map ~f:LTypeCodeGenEnv.gen_channel_struct envs
  in
  join_non_empty_lines ~sep:"\n\n" (pkg :: channel_imports :: channel_structs)

(** Generate source file containing the invitation struct declarations for a
    protocol *)
let gen_invitations_file envs imports setup_role_chan setup_invite_chan =
  let pkg_stmt = package_stmt pkg_invitations in
  let imports_str = ImportsEnv.generate_imports imports in
  let channel_structs =
    List.map ~f:LTypeCodeGenEnv.gen_invite_channel_struct envs
  in
  join_non_empty_lines ~sep:"\n\n"
    ( pkg_stmt :: imports_str :: setup_role_chan :: setup_invite_chan
    :: channel_structs )

(** Generate source file containing the result struct declarations for a
    protocol *)
let gen_results_file pkg_name roles =
  let pkg = package_stmt pkg_name in
  let result_structs = List.map ~f:LTypeCodeGenEnv.gen_result_struct roles in
  join_non_empty_lines ~sep:"\n\n" (pkg :: result_structs)

(** Generate the parameters for a role's implementation function *)
let role_function_params sync_pkg channels_pkg invitations_pkg callbacks_pkg
    local_protocol role =
  let wait_group_type = pkg_var_access sync_pkg wait_group_type in
  let wait_group_param = (wait_group, pointer_type wait_group_type) in
  let role_chan_name =
    ChannelStructName.of_string @@ chan_struct_name role
  in
  let role_chan_type = protocol_channel_access channels_pkg role_chan_name in
  let role_chan_param = (role_chan, role_chan_type) in
  let invite_chan_name =
    InviteChannelStructName.of_string @@ invite_struct_name local_protocol
  in
  let invite_chan_type =
    protocol_invite_channel_access invitations_pkg invite_chan_name
  in
  let invite_chan_param = (invite_chan, invite_chan_type) in
  let env_type_name =
    CallbacksEnvName.of_string @@ callbacks_env_name local_protocol
  in
  let env_type = callbacks_pkg_env callbacks_pkg env_type_name in
  let env_param = (env_var, env_type) in
  [wait_group_param; role_chan_param; invite_chan_param; env_param]

(** Generate the full function implementing a role's behaviour *)
let gen_role_impl_function env protocol role local_protocol impl
    is_dynamic_role =
  (* pkgs *)
  let env, channels_pkg = LTypeCodeGenEnv.import_channels env protocol in
  (* let env, results_pkg = LTypeCodeGenEnv.import_results env protocol in *)
  let env, callbacks_pkg = LTypeCodeGenEnv.import_callbacks env in
  let env, sync_pkg = LTypeCodeGenEnv.import_sync env in
  let env, invitations_pkg = LTypeCodeGenEnv.import_invitations env in
  (* params *)
  let params =
    role_function_params sync_pkg channels_pkg invitations_pkg callbacks_pkg
      local_protocol role
  in
  (* return type *)
  (* Dynamic roles don't return a result *)
  let env, return_type =
    if is_dynamic_role then (env, None)
    else
      let env, results_pkg = LTypeCodeGenEnv.import_results env protocol in
      let result_struct = ResultName.of_string @@ result_struct_name role in
      (env, Some (protocol_result_access results_pkg result_struct))
  in
  (* function *)
  let impl_function =
    FunctionName.of_string @@ local_protocol_function_name local_protocol
  in
  let function_impl = function_decl impl_function params return_type impl in
  (env, function_impl)

(** Generate interface used for initialising the environments for the roles
    of the first protocol *)
let generate_init_protocol_env_interface imports protocol roles
    protocol_lookup =
  let gen_role_env_function_decl callbacks_pkg role =
    let local_protocol =
      lookup_local_protocol protocol_lookup protocol role
    in
    let func_name = new_init_role_env_function_name role in
    let callbacks_env =
      CallbacksEnvName.of_string @@ callbacks_env_name local_protocol
    in
    let return_type = callbacks_pkg_env callbacks_pkg callbacks_env in
    let params = [] in
    (func_name, params, return_type)
  in
  let gen_role_result_env_function_decl results_pkg role =
    let func_name = new_init_role_result_function_name role in
    let result_struct = ResultName.of_string @@ result_struct_name role in
    let result_struct_type =
      protocol_result_access results_pkg result_struct
    in
    let result_param =
      var_type_decl (VariableName.user result_var) result_struct_type
    in
    let params = [result_param] in
    let return_type = "" in
    (func_name, params, return_type)
  in
  let imports, callbacks_pkg = ImportsEnv.import_callbacks imports in
  let imports, result_pkg = ImportsEnv.import_results imports protocol in
  let interface_name = entry_point_protocol_env_interface_name protocol in
  let interface_init_method_decls =
    List.map roles ~f:(gen_role_env_function_decl callbacks_pkg)
  in
  let interface_role_method_decls =
    List.map roles ~f:(gen_role_result_env_function_decl result_pkg)
  in
  let interface_methods =
    interface_init_method_decls @ interface_role_method_decls
  in
  (imports, interface_name, gen_interface interface_name interface_methods)

(** Generate the implementation of the entry point function body. This
    function creates the channels used by the roles for communicating with
    each other, generates the initial state for all the roles in the entry
    point protocol, starts their execution and waits for all of the roles to
    terminate *)
let gen_entry_point_setup_function_impl imports indent protocol_lookup
    protocol roles role_chan_vars invite_chan_vars create_chans_impl =
  let gen_role_env envs role =
    let env_var_name = new_role_env_var role in
    let envs = Map.add_exn envs ~key:role ~data:env_var_name in
    let new_env_function = new_init_role_env_function_name role in
    let call_create_env_method =
      call_method protocol_env_var new_env_function []
    in
    let create_role_env_assign =
      new_var_assignment env_var_name call_create_env_method
    in
    (envs, indent_line indent create_role_env_assign)
  in
  let gen_role_start_function_call role_envs role =
    let local_protocol =
      lookup_local_protocol protocol_lookup protocol role
    in
    let role_start_function = role_start_function_name local_protocol in
    let role_chan_var = Map.find_exn role_chan_vars role in
    let invite_chan_var = Map.find_exn invite_chan_vars role in
    let role_env_var = Map.find_exn role_envs role in
    let params =
      [ protocol_env_var
      ; reference_var wait_group
      ; role_chan_var
      ; invite_chan_var
      ; role_env_var ]
    in
    let start_function_call = call_function role_start_function params in
    indent_line indent (goroutine_call start_function_call)
  in
  let imports, sync_pkg = ImportsEnv.import_sync imports in
  let wait_group_type = pkg_var_access sync_pkg wait_group_type in
  let sync_group_decl = new_var_decl wait_group wait_group_type in
  let wait_group_add_stmt =
    call_method wait_group wait_group_add [Int.to_string (List.length roles)]
  in
  let role_envs, gen_role_envs_impl =
    List.fold_map roles ~init:(Map.empty (module RoleName)) ~f:gen_role_env
  in
  let gen_role_envs_impl_str = join_non_empty_lines gen_role_envs_impl in
  let role_start_function_calls =
    List.map roles ~f:(gen_role_start_function_call role_envs)
  in
  let role_start_function_calls_str =
    join_non_empty_lines role_start_function_calls
  in
  let wait_group_wait_stmt = call_method wait_group wait_group_wait [] in
  ( imports
  , join_non_empty_lines ~sep:"\n\n"
      [ create_chans_impl
      ; indent_line indent sync_group_decl
      ; indent_line indent wait_group_add_stmt
      ; gen_role_envs_impl_str
      ; role_start_function_calls_str
      ; indent_line indent wait_group_wait_stmt ] )

(** Generates the function declaration for the entry point protocol setup *)
let gen_entry_point_setup_function imports indent protocol_lookup protocol
    roles role_chan_vars invite_chan_vars protocol_env_interface
    create_chans_impl =
  let func_name = entry_point_function_name protocol in
  let protocol_env_type = InterfaceName.user protocol_env_interface in
  let protocol_env_param = (protocol_env_var, protocol_env_type) in
  let params = [protocol_env_param] in
  let return_type = None in
  let imports, setup_function_impl =
    gen_entry_point_setup_function_impl imports indent protocol_lookup
      protocol roles role_chan_vars invite_chan_vars create_chans_impl
  in
  let entry_point_function_decl =
    function_decl func_name params return_type setup_function_impl
  in
  (imports, entry_point_function_decl)

(** Generate the functions which start the execution of all the roles. These
    functions decrement the counter of the waitgroup used for synchronising
    the end of the protocol after terminating, execute their respective
    role's implementation and aggregate the result they generate in the
    protocol environment *)
let gen_start_role_functions imports indent init_protocol_interface
    protocol_lookup protocol roles =
  let gen_start_function_impl roles_pkg role =
    let local_protocol =
      lookup_local_protocol protocol_lookup protocol role
    in
    let call_wg_done = call_method wait_group wait_group_done [] in
    let defer_wg_done = defer_call call_wg_done in
    let role_function_name =
      FunctionName.of_string @@ local_protocol_function_name local_protocol
    in
    let role_impl_params = [wait_group; role_chan; invite_chan; env_var] in
    let role_function = pkg_function roles_pkg role_function_name in
    let call_role_impl_function =
      call_function role_function role_impl_params
    in
    let result_assign =
      new_var_assignment result_var call_role_impl_function
    in
    let result_callback = new_init_role_result_function_name role in
    let result_cb_params = [VariableName.user result_var] in
    let call_result_cb =
      call_method protocol_env_var result_callback result_cb_params
    in
    let impl_stmts = [defer_wg_done; result_assign; call_result_cb] in
    let impl_stmts = List.map impl_stmts ~f:(indent_line indent) in
    join_non_empty_lines impl_stmts
  in
  let gen_start_role_function sync_pkg channels_pkg invitations_pkg
      callbacks_pkg roles_pkg role =
    let local_protocol =
      lookup_local_protocol protocol_lookup protocol role
    in
    let start_function =
      FunctionName.of_string @@ role_start_function_name local_protocol
    in
    let protocol_env_type = InterfaceName.user init_protocol_interface in
    let protocol_env_param = (protocol_env_var, protocol_env_type) in
    let role_func_params =
      role_function_params sync_pkg channels_pkg invitations_pkg
        callbacks_pkg local_protocol role
    in
    let params = protocol_env_param :: role_func_params in
    let return_type = None in
    let func_impl = gen_start_function_impl roles_pkg role in
    function_decl start_function params return_type func_impl
  in
  let imports, sync_pkg = ImportsEnv.import_sync imports in
  let imports, channels_pkg = ImportsEnv.import_channels imports protocol in
  let imports, invitations_pkg = ImportsEnv.import_invitations imports in
  let imports, callbacks_pkg = ImportsEnv.import_callbacks imports in
  let imports, roles_pkg = ImportsEnv.import_roles imports in
  let start_functions =
    List.map roles
      ~f:
        (gen_start_role_function sync_pkg channels_pkg invitations_pkg
           callbacks_pkg roles_pkg)
  in
  (imports, join_non_empty_lines ~sep:"\n\n" start_functions)

(** Generate source file containing implementation of role *)
let gen_role_impl_file env impl is_dynamic_role role protocol local_type =
  let pkg_stmt = package_stmt pkg_roles in
  let env, function_impl =
    gen_role_impl_function env protocol role local_type impl is_dynamic_role
  in
  let imports = LTypeCodeGenEnv.generate_role_imports env in
  (env, join_non_empty_lines ~sep:"\n\n" [pkg_stmt; imports; function_impl])

(** Generate the source file containing the all the logic for setting up the
    entry point protocol *)
let gen_entry_point_file protocol_lookup protocol roles imports
    role_chan_vars invite_chan_vars chan_setup_impl =
  let pkg_stmt = package_stmt pkg_protocol in
  let imports, interface_name, interface_impl =
    generate_init_protocol_env_interface imports protocol roles
      protocol_lookup
  in
  let indent = incr_indent "" in
  let imports, start_functions_impl =
    gen_start_role_functions imports indent interface_name protocol_lookup
      protocol roles
  in
  let imports, entry_point_function =
    gen_entry_point_setup_function imports indent protocol_lookup protocol
      roles role_chan_vars invite_chan_vars interface_name chan_setup_impl
  in
  join_non_empty_lines ~sep:"\n\n"
    [ pkg_stmt
    ; ImportsEnv.generate_imports imports
    ; interface_impl
    ; start_functions_impl
    ; entry_point_function ]

(** Generate logic for setting up dynamic participants *)
let gen_dynamic_participants_init imports indent new_roles role_chan_vars
    invite_chan_vars protocol protocol_lookup =
  let gen_dynamic_participant_init callbacks_pkg role =
    let local_protocol =
      lookup_local_protocol protocol_lookup protocol role
    in
    let role_env_var = new_role_env_var role in
    let create_env_func =
      FunctionName.of_string @@ new_create_env_function_name local_protocol
    in
    let pkg_create_env_func = pkg_function callbacks_pkg create_env_func in
    let call_create_env_func = call_function pkg_create_env_func [] in
    let role_env_assign =
      new_var_assignment role_env_var call_create_env_func
    in
    let role_function = local_protocol_function_name local_protocol in
    let role_channel = Map.find_exn role_chan_vars role in
    let invite_channel = Map.find_exn invite_chan_vars role in
    let params = [wait_group; role_channel; invite_channel; role_env_var] in
    let role_function_call =
      goroutine_call @@ call_function role_function params
    in
    let init_stmts =
      List.map [role_env_assign; role_function_call] ~f:(indent_line indent)
    in
    join_non_empty_lines init_stmts
  in
  let imports, callbacks_pkg = ImportsEnv.import_callbacks imports in
  let wait_group_add_stmt =
    call_method wait_group wait_group_add
      [Int.to_string (List.length new_roles)]
  in
  let wait_group_add_stmt = indent_line indent wait_group_add_stmt in
  let new_role_initialisations =
    List.map new_roles ~f:(gen_dynamic_participant_init callbacks_pkg)
  in
  ( imports
  , join_non_empty_lines ~sep:"\n\n"
      (wait_group_add_stmt :: new_role_initialisations) )

(** Generate logic for setting up a protocol call:

    - Create all the channels
    - Create the role channel and invitation structs
    - Send the invitations
    - Start the execution of any dynamic participants in the protocol *)
let gen_setup_function_impl protocol_setup_env imports indent
    setup_channels_impl role_chan_vars invite_chan_vars protocol
    protocol_lookup (roles, new_roles) =
  (* TODO: import sync when generating function signature. handle imports*)
  let _, role_struct_fields =
    ProtocolSetupEnv.get_setup_channel_struct protocol_setup_env protocol
  in
  let role_channel_vars = List.map roles ~f:(Map.find_exn role_chan_vars) in
  let role_chans_and_vars =
    List.zip_exn role_struct_fields role_channel_vars
  in
  let send_role_channels =
    List.map role_chans_and_vars ~f:(send_invite_over_channel role_chan)
  in
  let send_role_channels =
    List.map send_role_channels ~f:(indent_line indent)
  in
  let send_role_channels_str = join_non_empty_lines send_role_channels in
  let _, invite_struct_fields =
    ProtocolSetupEnv.get_setup_invite_struct protocol_setup_env protocol
  in
  let invite_channel_vars =
    List.map roles ~f:(Map.find_exn invite_chan_vars)
  in
  let invite_chans_and_vars =
    List.zip_exn invite_struct_fields invite_channel_vars
  in
  let send_invite_channels =
    List.map invite_chans_and_vars ~f:(send_invite_over_channel invite_chan)
  in
  let send_invite_channels =
    List.map send_invite_channels ~f:(indent_line indent)
  in
  let send_invite_channels_str = join_non_empty_lines send_invite_channels in
  let imports, init_new_roles_impl =
    if List.length new_roles > 0 then
      gen_dynamic_participants_init imports indent new_roles role_chan_vars
        invite_chan_vars protocol protocol_lookup
    else (imports, "")
  in
  ( imports
  , join_non_empty_lines ~sep:"\n\n"
      [ setup_channels_impl
      ; send_role_channels_str
      ; send_invite_channels_str
      ; init_new_roles_impl ] )

(** Generate function declaration for setting up a protocol call *)
let gen_setup_function imports protocol_setup_env protocol impl =
  let imports, sync_pkg = ImportsEnv.import_sync imports in
  let imports, invitations_pkg = ImportsEnv.import_invitations imports in
  let func_name =
    ProtocolSetupEnv.get_setup_function_name protocol_setup_env protocol
  in
  let wait_group_type = pkg_var_access sync_pkg wait_group_type in
  let wait_group_param = (wait_group, pointer_type wait_group_type) in
  let role_chan_struct, _ =
    ProtocolSetupEnv.get_setup_channel_struct protocol_setup_env protocol
  in
  let role_chan_type =
    protocol_invite_channel_access invitations_pkg role_chan_struct
  in
  let role_chan_param = (role_chan, role_chan_type) in
  let invite_chan_struct, _ =
    ProtocolSetupEnv.get_setup_invite_struct protocol_setup_env protocol
  in
  let invite_chan_type =
    protocol_invite_channel_access invitations_pkg invite_chan_struct
  in
  let invite_chan_param = (invite_chan, invite_chan_type) in
  let params = [wait_group_param; role_chan_param; invite_chan_param] in
  (imports, function_decl func_name params None impl)

(** Generate source file containing the logic for setting up a call to a
    particular protocol *)
let gen_setup_file protocol_setup_env imports indent setup_channels_impl
    role_chan_vars invite_chan_vars protocol protocol_lookup
    (roles, new_roles) =
  let pkg_stmt = package_stmt pkg_roles in
  let imports, setup_func_body =
    gen_setup_function_impl protocol_setup_env imports indent
      setup_channels_impl role_chan_vars invite_chan_vars protocol
      protocol_lookup (roles, new_roles)
  in
  let imports, setup_function =
    gen_setup_function imports protocol_setup_env protocol setup_func_body
  in
  join_non_empty_lines ~sep:"\n\n"
    [pkg_stmt; ImportsEnv.generate_imports imports; setup_function]

(** Generate all the environment containing all the messages in a protocol *)
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

(** Generate implementation of a role from its local type *)
let gen_role_implementation protocol_setup_env ltype_env global_t
    protocol_lookup is_dynamic_role protocol role
    (local_proto_name : LocalProtocolName.t) ltype =
  let rec gen_implementation indent is_recv_choice_msg (env, var_name_gen)
      ltype =
    match ltype with
    | EndL ->
        let env, done_callback =
          LTypeCodeGenEnv.add_done_callback env protocol role is_dynamic_role
        in
        let impl =
          gen_end_of_protocol indent is_dynamic_role done_callback
        in
        ((env, var_name_gen), impl)
    | TVarL _ | MuL _ ->
        (* TODO: Support recursion *)
        raise
          (Err.Violation
             "Explicit recursion constructs not currently supported in this \
              code generation scheme")
    | InviteCreateL (invite_roles, _, protocol', ltype') ->
        let (new_proto_roles, _), _, _ = Map.find_exn global_t protocol' in
        let acting_roles = List.zip_exn invite_roles new_proto_roles in
        let env, invite_channels =
          List.fold_map ~init:env
            ~f:(gen_send_invite_chan_names protocol_lookup protocol')
            acting_roles
        in
        let env, setup_cb =
          LTypeCodeGenEnv.new_protocol_setup_callback env protocol'
        in
        let env, invite_pkg = LTypeCodeGenEnv.import_invitations env in
        let invite_impl, var_name_gen =
          gen_invite_impl var_name_gen protocol' invite_pkg
            protocol_setup_env invite_channels setup_cb indent
        in
        let invite_impl = List.map ~f:(indent_line indent) invite_impl in
        let invite_impl_str = join_non_empty_lines invite_impl in
        let (env, var_name_gen), impl =
          gen_implementation indent false (env, var_name_gen) ltype'
        in
        ( (env, var_name_gen)
        , join_non_empty_lines ~sep:"\n\n" [invite_impl_str; impl] )
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
        let accept_impl, var_name_gen =
          gen_accept_impl var_name_gen local_protocol role_channel
            invite_channel accept_cb result_cb
        in
        let accept_impl, cont_indent =
          if is_recv_choice_msg then gen_select_case accept_impl indent
          else (List.map ~f:(indent_line indent) accept_impl, indent)
        in
        let accept_impl_str = join_non_empty_lines accept_impl in
        let (env, var_name_gen), impl =
          gen_implementation cont_indent false (env, var_name_gen) ltype'
        in
        ( (env, var_name_gen)
        , join_non_empty_lines ~sep:"\n\n" [accept_impl_str; impl] )
    | ChoiceL (r, ltys) ->
        if RoleName.equal role r then
          let choice_labels = extract_choice_labels ltys in
          let env, choice_cb, choice_enums =
            LTypeCodeGenEnv.new_choice_callback env role choice_labels
              local_proto_name
          in
          let env, callbacks_pkg = LTypeCodeGenEnv.import_callbacks env in
          let (env, var_name_gen), choice_impls =
            List.fold_map ~init:(env, var_name_gen)
              ~f:(gen_implementation (incr_indent indent) false)
              ltys
          in
          let impl, var_name_gen =
            gen_make_choice_impl var_name_gen role callbacks_pkg choice_cb
              choice_enums choice_impls indent
          in
          ((env, var_name_gen), impl)
        else
          let env, choice_impls =
            List.fold_map ~init:(env, var_name_gen)
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
        let recv_impl, var_name_gen =
          gen_recv_impl var_name_gen label channel_name recv_cb
        in
        let recv_impl, cont_indent =
          if is_recv_choice_msg then gen_select_case recv_impl indent
          else (List.map ~f:(indent_line indent) recv_impl, indent)
        in
        let recv_impl_str = join_non_empty_lines recv_impl in
        let (env, var_name_gen), impl =
          gen_implementation cont_indent false (env, var_name_gen) ltype'
        in
        ( (env, var_name_gen)
        , join_non_empty_lines ~sep:"\n\n" [recv_impl_str; impl] )
    | SendL ({label; _}, r, ltype') ->
        let env, channel_name = LTypeCodeGenEnv.new_channel env r label in
        let env, send_cb =
          LTypeCodeGenEnv.new_send_callback env label r protocol
        in
        let send_impl, var_name_gen =
          gen_send_impl var_name_gen label channel_name send_cb
        in
        let send_impl = List.map ~f:(indent_line indent) send_impl in
        let send_impl_str = join_non_empty_lines send_impl in
        let (env, var_name_gen), impl =
          gen_implementation indent false (env, var_name_gen) ltype'
        in
        ( (env, var_name_gen)
        , join_non_empty_lines ~sep:"\n\n" [send_impl_str; impl] )
  in
  let indent = incr_indent "" in
  let var_name_gen = Namegen.create () in
  let env, impl =
    gen_implementation indent false (ltype_env, var_name_gen) ltype
  in
  let impl =
    if is_dynamic_role then
      let call_wg_done = call_method wait_group wait_group_done [] in
      let defer_wg_done = defer_call call_wg_done in
      join_non_empty_lines [indent_line indent defer_wg_done; impl]
    else impl
  in
  (env, impl)

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
      Map.t
  ; protocol_setup:
      (ProtocolName.t, string, ProtocolName.comparator_witness) Map.t
  ; entry_point: string }
(** Result of code generation process *)

(** Generate empty code gen result *)
let empty_result () : codegen_result =
  { messages= Map.empty (module ProtocolName)
  ; channels= Map.empty (module ProtocolName)
  ; results= Map.empty (module ProtocolName)
  ; invite_channels= Map.empty (module ProtocolName)
  ; impl= Map.empty (module LocalProtocolName)
  ; callbacks= Map.empty (module LocalProtocolName)
  ; protocol_setup= Map.empty (module ProtocolName)
  ; entry_point= "" }

(** Generate all the elements of the implementation of a Scribble module and
    the entry point protocol *)
let gen_code root_dir gen_protocol (global_t : global_t) (local_t : local_t)
    =
  (* TODO: Move build_local_proto_name_lookup to codegen *)
  let protocol_lookup = build_local_proto_name_lookup local_t in
  let protocol_setup_env =
    ProtocolSetupEnv.create protocol_lookup local_t global_t
  in
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
            LTypeCodeGenEnv.create protocol role local_protocol root_dir
              protocol_env
          in
          let is_dynamic_role =
            List.mem new_roles role ~equal:RoleName.equal
          in
          let (env, _), protocol_impl =
            gen_role_implementation protocol_setup_env env global_t
              protocol_lookup is_dynamic_role protocol role local_protocol
              ltype
          in
          let env, impl_file =
            gen_role_impl_file env protocol_impl is_dynamic_role role
              protocol local_protocol
          in
          let is_dynamic_role =
            List.mem new_roles role ~equal:RoleName.equal
          in
          let callbacks_file =
            LTypeCodeGenEnv.gen_callbacks_file env local_protocol
              is_dynamic_role
          in
          let result =
            { result with
              impl= Map.add_exn impl ~key:local_protocol ~data:impl_file
            ; callbacks=
                Map.add_exn callbacks ~key:local_protocol
                  ~data:callbacks_file }
          in
          let protocol_env = LTypeCodeGenEnv.get_protocol_env env in
          ((result, protocol_env), env))
        (roles @ new_roles)
    in
    let pkg = PackageName.of_string @@ protocol_pkg_name protocol in
    let messages_file =
      MessagesEnv.generate_messages_file messages_env pkg
    in
    let messages =
      Map.add_exn result.messages ~key:protocol ~data:messages_file
    in
    let invite_imports, channels_pkg =
      ImportsEnv.import_channels protocol_env.invite_imports protocol
    in
    let protocol_setup_chan =
      ProtocolSetupEnv.gen_setup_channel_struct protocol_setup_env
        channels_pkg protocol
    in
    let protocol_setup_invite_chan =
      ProtocolSetupEnv.gen_setup_invite_struct protocol_setup_env protocol
    in
    let chan_file =
      gen_channels_file pkg envs protocol_env.channel_imports
    in
    let channels =
      Map.add_exn result.channels ~key:protocol ~data:chan_file
    in
    let invitations_file =
      gen_invitations_file envs invite_imports protocol_setup_chan
        protocol_setup_invite_chan
    in
    let invite_channels =
      Map.add_exn result.invite_channels ~key:protocol ~data:invitations_file
    in
    let results_file = gen_results_file pkg roles in
    let results =
      Map.add_exn result.results ~key:protocol ~data:results_file
    in
    let protocol_setup_gen =
      ProtocolSetupGen.create root_dir (roles @ new_roles) protocol
        protocol_lookup
    in
    let setup_imports, role_chan_vars, invite_chan_vars, setup_channels_impl
        =
      ProtocolSetupGen.generate_setup_channels global_t protocol_lookup
        protocol_setup_gen
    in
    let indent = incr_indent "" in
    let setup_file =
      gen_setup_file protocol_setup_env setup_imports indent
        setup_channels_impl role_chan_vars invite_chan_vars protocol
        protocol_lookup (roles, new_roles)
    in
    let protocol_setup =
      Map.add_exn result.protocol_setup ~key:protocol ~data:setup_file
    in
    let entry_point =
      if ProtocolName.equal gen_protocol protocol then
        gen_entry_point_file protocol_lookup protocol roles setup_imports
          role_chan_vars invite_chan_vars setup_channels_impl
      else result.entry_point
    in
    { result with
      messages
    ; channels
    ; invite_channels
    ; results
    ; protocol_setup
    ; entry_point }
  in
  Map.fold ~init:(empty_result ()) ~f:gen_protocol_role_implementation
    global_t
