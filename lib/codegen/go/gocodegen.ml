open! Base
open Ltype
open Goenvs
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

(** Generate the struct field names for the channels over which to send the
    invitations to a role *)
let gen_send_invite_chan_names protocol_lookup protocol env
    (participant, new_role) =
  let local_protocol =
    lookup_local_protocol protocol_lookup protocol new_role
  in
  let env, role_chan, invite_chan =
    LTypeCodeGenEnv.get_or_add_send_invitation_channels env participant
      local_protocol new_role protocol
  in
  (* TODO: Fix label channels *)
  (env, (role_chan, invite_chan))

(** Generate the struct field names for the channels over which to accept the
    invitations from the caller role *)
let gen_accept_invite_chan_names env curr_role caller new_role protocol
    local_protocol =
  if RoleName.equal caller curr_role then
    LTypeCodeGenEnv.get_or_add_send_invitation_channels env curr_role
      local_protocol new_role protocol
  else
    LTypeCodeGenEnv.get_or_add_recv_invitation_channels env caller
      local_protocol new_role protocol

(** Extract labels from the first interactions in each local type. These
    labels will not necessarily be unique in the case of protocol calls *)
let extract_choice_labels ltypes =
  let rec extract_label = function
    | InviteCreateL (_, _, protocol, _) ->
        LabelName.of_string @@ ProtocolName.user protocol
    | SendL ({label; _}, _, _) -> label
    | MuL (_, _, ltype) -> extract_label ltype
    | TVarL _ ->
        Err.violation ~here:[%here]
          "Currently unfolding of recursion not supported for extracting \
           choice labels - branches of choices should have an explit first \
           message"
    | _ ->
        Err.violation ~here:[%here]
          "Cannot generate code for nested choices - choices should be \
           flattened or local type should be normalised to ensure this"
  in
  List.map ~f:extract_label ltypes

let extract_choice_label_enums msgs_env ltypes =
  let rec extract_enum_label = function
    | AcceptL (_, protocol, roles, _, _, _) ->
        MessagesEnv.get_invitation_enum msgs_env protocol roles
    | RecvL ({label; _}, _, _) -> MessagesEnv.get_message_enum msgs_env label
    | _ ->
        Err.violation ~here:[%here]
          "Cannot extract choice msg labels: choices should be flattened \
           and recursion should be unfolded (local type should be \
           normalised) to ensure that all branches have an explict first \
           message "
  in
  List.map ltypes ~f:extract_enum_label

(** Generate implementation of accept local type *)
let gen_accept_impl env var_name_gen caller curr_role local_protocol
    role_channel role_invite_chan accept_cb result_cb is_recv_choice_msg =
  (* <local_proto>_chan := <-inviteChan.<role_chan> *)
  let role_chan_var = VariableName.new_role_chan_var local_protocol in
  let var_name_gen, role_chan_var_name =
    new_variable var_name_gen role_chan_var
  in
  let role_chan_assign =
    recv_from_invite_chan role_chan_var_name VariableName.invite_chan
      role_channel
  in
  (* <local_proto>_invite_chan := <-inviteChan.<role_invite_chan> *)
  let role_invite_chan_var =
    VariableName.new_role_invite_chan_var local_protocol
  in
  let var_name_gen, role_invite_chan_var_name =
    new_variable var_name_gen role_invite_chan_var
  in
  let invite_chan_assign =
    recv_from_invite_chan role_invite_chan_var_name VariableName.invite_chan
      role_invite_chan
  in
  (* <local_protocol>_env := env.<accept_cb>() *)
  let new_env_var = VariableName.new_env_var local_protocol in
  let var_name_gen, new_env_var_name =
    new_variable var_name_gen new_env_var
  in
  let accept_function =
    FunctionName.of_string @@ CallbackName.user accept_cb
  in
  let new_env_assign =
    new_var_assignment new_env_var_name
      (call_method VariableName.env_var accept_function [])
  in
  (* <local_protocol>_result := <local_protocol>(wg, <local_protocol>_chan,
     <local_protocol>_inviteChan, <local_protocol>_env) *)
  let result_var = VariableName.new_result_var local_protocol in
  let var_name_gen, result_var_name = new_variable var_name_gen result_var in
  let role_function =
    FunctionName.local_protocol_function_name local_protocol
  in
  let params =
    [ VariableName.wait_group
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
    call_method VariableName.env_var result_function
      [VariableName.user result_var_name]
  in
  let accept_impl =
    [ role_chan_assign
    ; invite_chan_assign
    ; new_env_assign
    ; result_assign
    ; result_callback_call ]
  in
  if is_recv_choice_msg || RoleName.equal caller curr_role then
    (env, var_name_gen, accept_impl)
  else
    let env, chan_name =
      LTypeCodeGenEnv.get_or_add_channel env (caller, None, false)
    in
    let recv_label_stmt = ignore_recv_msg VariableName.role_chan chan_name in
    (env, var_name_gen, recv_label_stmt :: accept_impl)

(** Generate implementation of receive local type *)
let gen_recv_impl env var_name_gen sender payloads recv_cb is_recv_choice_msg
    =
  let env, var_name_gen, chan_recv_stmts, chan_vars =
    List.fold (List.rev payloads) ~init:(env, var_name_gen, [], [])
      ~f:(fun (env, var_name_gen, chan_recv_stmts, chan_vars) -> function
      | PValue (payload_name, payload_type) ->
          let env, chan_name =
            LTypeCodeGenEnv.get_or_add_channel env
              ( sender
              , Some (Expr.payload_typename_of_payload_type payload_type)
              , false )
          in
          (* Assume all payloads are named *)
          let payload_var_str = Option.value_exn payload_name in
          let var_name_gen, payload_var =
            new_variable var_name_gen payload_var_str
          in
          let recv_payload_stmt =
            recv_from_msg_chan payload_var VariableName.role_chan chan_name
          in
          ( env
          , var_name_gen
          , recv_payload_stmt :: chan_recv_stmts
          , VariableName.user payload_var :: chan_vars )
      | PDelegate _ -> Err.violation ~here:[%here] "Delegation not supported" )
  in
  (* env.<msg>_From_<sender>(<msg>) *)
  let recv_function =
    FunctionName.of_other_name (module CallbackName) recv_cb
  in
  let call_recv_callback =
    call_method VariableName.env_var recv_function chan_vars
  in
  let recv_impl_stmts = chan_recv_stmts @ [call_recv_callback] in
  if is_recv_choice_msg then
    (* let _ = in *)
    (env, var_name_gen, recv_impl_stmts)
  else
    let env, chan_name =
      LTypeCodeGenEnv.get_or_add_channel env (sender, None, false)
    in
    let recv_label_stmt = ignore_recv_msg VariableName.role_chan chan_name in
    (env, var_name_gen, recv_label_stmt :: recv_impl_stmts)

(** Generate implementation of send local type *)
let gen_send_impl env var_name_gen receiver msg_enum payloads send_cb =
  let env, var_name_gen, chan_send_stmts, chan_vars =
    List.fold (List.rev payloads) ~init:(env, var_name_gen, [], [])
      ~f:(fun (env, var_name_gen, chan_recv_stmts, chan_vars) -> function
      | PValue (payload_name, payload_type) ->
          let env, chan_name =
            LTypeCodeGenEnv.get_or_add_channel env
              ( receiver
              , Some (Expr.payload_typename_of_payload_type payload_type)
              , true )
          in
          (* Assume all payloads are named *)
          let payload_var_str = Option.value_exn payload_name in
          let var_name_gen, payload_var =
            new_variable var_name_gen payload_var_str
          in
          let recv_payload_stmt =
            send_msg_over_channel VariableName.role_chan chan_name
              payload_var
          in
          ( env
          , var_name_gen
          , recv_payload_stmt :: chan_recv_stmts
          , VariableName.user payload_var :: chan_vars )
      | PDelegate _ -> Err.violation ~here:[%here] "Delegation not supported" )
  in
  (* env.<msg>_From_<sender>(<msg>) *)
  let send_function = FunctionName.of_string @@ CallbackName.user send_cb in
  let call_send_callback =
    call_method VariableName.env_var send_function []
  in
  let call_send_callback =
    if List.length chan_vars > 0 then
      multiple_var_assignment chan_vars call_send_callback
    else call_send_callback
  in
  let env, label_chan =
    LTypeCodeGenEnv.get_or_add_channel env (receiver, None, true)
  in
  let env, msgs_pkg = LTypeCodeGenEnv.import_messages env in
  let send_label_stmt =
    send_value_over_channel VariableName.role_chan label_chan
      (pkg_enum_access msgs_pkg msg_enum)
  in
  ( env
  , var_name_gen
  , call_send_callback :: send_label_stmt :: chan_send_stmts )

(** Generate implementation of invite + create local types *)
let gen_invite_impl env var_name_gen protocol curr_role invite_enum
    invite_roles invite_pkg setup_env invite_channels setup_cb indent =
  (* env.<protocol>_Setup() *)
  let setup_function =
    FunctionName.of_string @@ CallbackName.user setup_cb
  in
  let setup_callback_call =
    call_method VariableName.env_var setup_function []
  in
  let env, send_invite_label_stmts =
    List.fold_map invite_roles ~init:env ~f:(fun env invite_role ->
        (* TODO: make less hacky *)
        if RoleName.equal invite_role curr_role then (env, "")
        else
          let env, label_chan =
            LTypeCodeGenEnv.get_or_add_channel env (invite_role, None, true)
          in
          let env, msgs_pkg = LTypeCodeGenEnv.import_messages env in
          let send_label_stmt =
            send_value_over_channel VariableName.role_chan label_chan
              (pkg_enum_access msgs_pkg invite_enum)
          in
          (env, send_label_stmt) )
  in
  let role_channels, invite_channels = List.unzip invite_channels in
  (* <protocol>_rolechan := <protocol>_RoleSetupChan{<role1>_Chan:
     roleChannels.<role_channel>, ...} *)
  let role_struct, role_struct_fields =
    ProtocolSetupEnv.get_setup_channel_struct setup_env protocol
  in
  let str_role_channels =
    List.map
      ~f:(invite_channel_struct_field_access VariableName.invite_chan)
      role_channels
  in
  let str_role_struct_fields =
    List.map ~f:InviteChannelName.user role_struct_fields
  in
  let role_struct_var = VariableName.new_role_setup_var protocol in
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
      ~f:(invite_channel_struct_field_access VariableName.invite_chan)
      invite_channels
  in
  let str_invite_struct_fields =
    List.map ~f:InviteChannelName.user invite_struct_fields
  in
  let invite_struct_var = VariableName.new_invite_setup_var protocol in
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
    [VariableName.wait_group; role_struct_var_name; invite_struct_var_name]
  in
  let protocol_setup_call =
    call_function protocol_setup_function setup_params
  in
  ( env
  , var_name_gen
  , setup_callback_call
    :: ( send_invite_label_stmts
       @ [role_struct_assign; invite_struct_assign; protocol_setup_call] ) )

(** Generate implementation of choice local type when the current role is
    making the choice*)
let gen_make_choice_impl var_name_gen role callbacks_pkg choice_cb
    choice_enums choice_impls indent =
  (* choice := env.<role>_Choice() *)
  (* switch choice {case <enum_val>: <impl>} *)
  let choice_enum_var = VariableName.new_choice_enum_var role in
  let var_name_gen, choice_enum_var_name =
    new_variable var_name_gen choice_enum_var
  in
  let choice_function =
    FunctionName.of_string @@ CallbackName.user choice_cb
  in
  let choice_enum_assign =
    new_var_assignment choice_enum_var_name
      (call_method VariableName.env_var choice_function [])
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
let gen_recv_choice_impl env var_name_gen choice_role label_msg_enums
    choice_impls msgs_pkg indent =
  let choice_var_str = VariableName.new_choice_enum_var choice_role in
  let var_name_gen, label_msg_var =
    new_variable var_name_gen choice_var_str
  in
  let env, chan_name =
    LTypeCodeGenEnv.get_or_add_channel env (choice_role, None, false)
  in
  let recv_choice_stmt =
    indent_line indent
    @@ recv_from_msg_chan label_msg_var VariableName.role_chan chan_name
  in
  let invalid_choice_panic = panic_with_msg invalid_choice_panic_msg in
  let invalid_choice_panic =
    indent_line (incr_indent indent) invalid_choice_panic
  in
  let default_impl = Some invalid_choice_panic in
  let choice_switch =
    gen_switch_stmt label_msg_var msgs_pkg label_msg_enums choice_impls
      indent default_impl
  in
  (env, var_name_gen, join_non_empty_lines [recv_choice_stmt; choice_switch])

(** Generate implementation of end local type *)
let gen_end_of_protocol indent is_dynamic_role done_cb =
  let done_function = FunctionName.of_string @@ CallbackName.user done_cb in
  let call = call_method VariableName.env_var done_function [] in
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
  let pkg_stmt = package_stmt PackageName.pkg_invitations in
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
  let wait_group_type =
    pkg_var_access sync_pkg VariableName.wait_group_type
  in
  let wait_group_param =
    (VariableName.wait_group, pointer_type wait_group_type)
  in
  let role_chan_name = ChannelStructName.chan_struct_name role in
  let role_chan_type = protocol_channel_access channels_pkg role_chan_name in
  let role_chan_param = (VariableName.role_chan, role_chan_type) in
  let invite_chan_name =
    InviteChannelStructName.invite_struct_name local_protocol
  in
  let invite_chan_type =
    protocol_invite_channel_access invitations_pkg invite_chan_name
  in
  let invite_chan_param = (VariableName.invite_chan, invite_chan_type) in
  let env_type_name = CallbacksEnvName.callbacks_env_name local_protocol in
  let env_type = callbacks_pkg_env callbacks_pkg env_type_name in
  let env_param = (VariableName.env_var, env_type) in
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
      let result_struct = ResultName.result_struct_name role in
      (env, Some (protocol_result_access results_pkg result_struct))
  in
  (* function *)
  let impl_function =
    FunctionName.local_protocol_function_name local_protocol
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
    let func_name = FunctionName.new_init_role_env_function_name role in
    let callbacks_env = CallbacksEnvName.callbacks_env_name local_protocol in
    let return_type = callbacks_pkg_env callbacks_pkg callbacks_env in
    let params = [] in
    (func_name, params, return_type)
  in
  let gen_role_result_env_function_decl results_pkg role =
    let func_name = FunctionName.new_init_role_result_function_name role in
    let result_struct = ResultName.result_struct_name role in
    let result_struct_type =
      protocol_result_access results_pkg result_struct
    in
    let result_param =
      var_type_decl VariableName.result_var result_struct_type
    in
    let params = [result_param] in
    let return_type = "" in
    (func_name, params, return_type)
  in
  let imports, callbacks_pkg = ImportsEnv.import_callbacks imports in
  let imports, result_pkg = ImportsEnv.import_results imports protocol in
  let interface_name =
    InterfaceName.entry_point_protocol_env_interface_name protocol
  in
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
    let env_var_name = VariableName.new_role_env_var role in
    let envs = Map.add_exn envs ~key:role ~data:env_var_name in
    let new_env_function =
      FunctionName.new_init_role_env_function_name role
    in
    let call_create_env_method =
      call_method VariableName.protocol_env_var new_env_function []
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
    let role_start_function =
      FunctionName.role_start_function_name local_protocol
    in
    let role_chan_var = Map.find_exn role_chan_vars role in
    let invite_chan_var = Map.find_exn invite_chan_vars role in
    let role_env_var = Map.find_exn role_envs role in
    let params =
      [ VariableName.protocol_env_var
      ; reference_var VariableName.wait_group
      ; role_chan_var
      ; invite_chan_var
      ; role_env_var ]
    in
    let start_function_call = call_function role_start_function params in
    indent_line indent (goroutine_call start_function_call)
  in
  let imports, sync_pkg = ImportsEnv.import_sync imports in
  let wait_group_type =
    pkg_var_access sync_pkg VariableName.wait_group_type
  in
  let sync_group_decl =
    new_var_decl VariableName.wait_group wait_group_type
  in
  let wait_group_add_stmt =
    call_method VariableName.wait_group FunctionName.wait_group_add
      [Int.to_string (List.length roles)]
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
  let wait_group_wait_stmt =
    call_method VariableName.wait_group FunctionName.wait_group_wait []
  in
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
  let func_name = FunctionName.entry_point_function_name protocol in
  let protocol_env_type = InterfaceName.user protocol_env_interface in
  let protocol_env_param =
    (VariableName.protocol_env_var, protocol_env_type)
  in
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
    let call_wg_done =
      call_method VariableName.wait_group FunctionName.wait_group_done []
    in
    let defer_wg_done = defer_call call_wg_done in
    let role_function_name =
      FunctionName.local_protocol_function_name local_protocol
    in
    let role_impl_params =
      [ VariableName.wait_group
      ; VariableName.role_chan
      ; VariableName.invite_chan
      ; VariableName.env_var ]
    in
    let role_function = pkg_function roles_pkg role_function_name in
    let call_role_impl_function =
      call_function role_function role_impl_params
    in
    let result_assign =
      new_var_assignment VariableName.result_var call_role_impl_function
    in
    let result_callback =
      FunctionName.new_init_role_result_function_name role
    in
    let result_cb_params = [VariableName.user VariableName.result_var] in
    let call_result_cb =
      call_method VariableName.protocol_env_var result_callback
        result_cb_params
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
      FunctionName.role_start_function_name local_protocol
    in
    let protocol_env_type = InterfaceName.user init_protocol_interface in
    let protocol_env_param =
      (VariableName.protocol_env_var, protocol_env_type)
    in
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
           callbacks_pkg roles_pkg )
  in
  (imports, join_non_empty_lines ~sep:"\n\n" start_functions)

(** Generate source file containing implementation of role *)
let gen_role_impl_file env impl is_dynamic_role role protocol local_type =
  let pkg_stmt = package_stmt PackageName.pkg_roles in
  let env, function_impl =
    gen_role_impl_function env protocol role local_type impl is_dynamic_role
  in
  let imports = LTypeCodeGenEnv.generate_role_imports env in
  (env, join_non_empty_lines ~sep:"\n\n" [pkg_stmt; imports; function_impl])

(** Generate the source file containing the all the logic for setting up the
    entry point protocol *)
let gen_entry_point_file protocol_lookup protocol roles imports
    role_chan_vars invite_chan_vars chan_setup_impl =
  let pkg_stmt = package_stmt PackageName.pkg_protocol in
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
    let role_env_var = VariableName.new_role_env_var role in
    let create_env_func =
      FunctionName.new_create_env_function_name local_protocol
    in
    let pkg_create_env_func = pkg_function callbacks_pkg create_env_func in
    let call_create_env_func = call_function pkg_create_env_func [] in
    let role_env_assign =
      new_var_assignment role_env_var call_create_env_func
    in
    let role_function =
      FunctionName.local_protocol_function_name local_protocol
    in
    let role_channel = Map.find_exn role_chan_vars role in
    let invite_channel = Map.find_exn invite_chan_vars role in
    let params =
      [VariableName.wait_group; role_channel; invite_channel; role_env_var]
    in
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
    call_method VariableName.wait_group FunctionName.wait_group_add
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
  let _, role_struct_fields =
    ProtocolSetupEnv.get_setup_channel_struct protocol_setup_env protocol
  in
  let role_channel_vars = List.map roles ~f:(Map.find_exn role_chan_vars) in
  let role_chans_and_vars =
    List.zip_exn role_struct_fields role_channel_vars
  in
  let send_role_channels =
    List.map role_chans_and_vars
      ~f:(send_invite_over_channel VariableName.role_chan)
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
    List.map invite_chans_and_vars
      ~f:(send_invite_over_channel VariableName.invite_chan)
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
  let wait_group_type =
    pkg_var_access sync_pkg VariableName.wait_group_type
  in
  let wait_group_param =
    (VariableName.wait_group, pointer_type wait_group_type)
  in
  let role_chan_struct, _ =
    ProtocolSetupEnv.get_setup_channel_struct protocol_setup_env protocol
  in
  let role_chan_type =
    protocol_invite_channel_access invitations_pkg role_chan_struct
  in
  let role_chan_param = (VariableName.role_chan, role_chan_type) in
  let invite_chan_struct, _ =
    ProtocolSetupEnv.get_setup_invite_struct protocol_setup_env protocol
  in
  let invite_chan_type =
    protocol_invite_channel_access invitations_pkg invite_chan_struct
  in
  let invite_chan_param = (VariableName.invite_chan, invite_chan_type) in
  let params = [wait_group_param; role_chan_param; invite_chan_param] in
  (imports, function_decl func_name params None impl)

(** Generate source file containing the logic for setting up a call to a
    particular protocol *)
let gen_setup_file protocol_setup_env imports indent setup_channels_impl
    role_chan_vars invite_chan_vars protocol protocol_lookup
    (roles, new_roles) =
  let pkg_stmt = package_stmt PackageName.pkg_roles in
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
let rec gen_message_label_enums msgs_env = function
  | EndG | TVarG _ -> msgs_env
  | MuG (_, _, g) -> gen_message_label_enums msgs_env g
  | CallG (_, protocol, roles, g) ->
      let msgs_env =
        MessagesEnv.add_invitation_enum msgs_env protocol roles
      in
      gen_message_label_enums msgs_env g
  | ChoiceG (_, gtypes) ->
      List.fold ~init:msgs_env ~f:gen_message_label_enums gtypes
  | MessageG ({label; _}, _, _, g) ->
      let msgs_env = MessagesEnv.add_message_enum msgs_env label in
      gen_message_label_enums msgs_env g

(** Generate implementation of a role from its local type *)
let gen_role_implementation msgs_env protocol_setup_env ltype_env global_t
    protocol_lookup is_dynamic_role protocol role
    (local_proto_name : LocalProtocolName.t) ltype =
  let module Namegen = Namegen.Make (VariableName) in
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
    | TVarL (var, _) ->
        let impl = continue_stmt var in
        ((env, var_name_gen), indent_line indent impl)
    | MuL (var, _, ltype') ->
        let loop_label = recursion_label var in
        let (env, var_name_gen), recursion_body =
          gen_implementation (incr_indent indent) is_recv_choice_msg
            (env, var_name_gen) ltype'
        in
        let loop = recursion_loop recursion_body indent in
        let impl = recursion_impl loop_label loop in
        ((env, var_name_gen), impl)
    | InviteCreateL (invite_roles, _, protocol', ltype') ->
        let {static_roles; _} = Map.find_exn global_t protocol' in
        let acting_roles = List.zip_exn invite_roles static_roles in
        let env, invite_channels =
          List.fold_map ~init:env
            ~f:(gen_send_invite_chan_names protocol_lookup protocol')
            acting_roles
        in
        let env, setup_cb =
          LTypeCodeGenEnv.new_protocol_setup_callback env protocol'
        in
        let env, invite_pkg = LTypeCodeGenEnv.import_invitations env in
        let invite_enum =
          MessagesEnv.get_invitation_enum msgs_env protocol' invite_roles
        in
        let env, var_name_gen, invite_impl =
          gen_invite_impl env var_name_gen protocol' role invite_enum
            invite_roles invite_pkg protocol_setup_env invite_channels
            setup_cb indent
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
        let env, var_name_gen, accept_impl =
          gen_accept_impl env var_name_gen caller role local_protocol
            role_channel invite_channel accept_cb result_cb
            is_recv_choice_msg
        in
        let accept_impl = List.map ~f:(indent_line indent) accept_impl in
        let accept_impl_str = join_non_empty_lines accept_impl in
        let (env, var_name_gen), impl =
          gen_implementation indent false (env, var_name_gen) ltype'
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
          let msg_label_enums = extract_choice_label_enums msgs_env ltys in
          let (env, var_name_gen), choice_impls =
            List.fold_map ~init:(env, var_name_gen)
              ~f:(gen_implementation (incr_indent indent) true)
              ltys
          in
          let env, msgs_pkg = LTypeCodeGenEnv.import_messages env in
          let env, var_name_gen, impl =
            gen_recv_choice_impl env var_name_gen r msg_label_enums
              choice_impls msgs_pkg indent
          in
          ((env, var_name_gen), impl)
    | RecvL ({label; payload= payloads}, r, ltype') ->
        let env, recv_cb =
          LTypeCodeGenEnv.new_recv_callback env label payloads r
        in
        let env, var_name_gen, recv_impl =
          gen_recv_impl env var_name_gen r payloads recv_cb
            is_recv_choice_msg
        in
        let recv_impl = List.map ~f:(indent_line indent) recv_impl in
        let recv_impl_str = join_non_empty_lines recv_impl in
        let (env, var_name_gen), impl =
          gen_implementation indent false (env, var_name_gen) ltype'
        in
        ( (env, var_name_gen)
        , join_non_empty_lines ~sep:"\n\n" [recv_impl_str; impl] )
    | SendL ({label; payload= payloads}, r, ltype') ->
        let env, send_cb =
          LTypeCodeGenEnv.new_send_callback env label payloads r
        in
        let label_enum = MessagesEnv.get_message_enum msgs_env label in
        let env, var_name_gen, send_impl =
          gen_send_impl env var_name_gen r label_enum payloads send_cb
        in
        let send_impl = List.map ~f:(indent_line indent) send_impl in
        let send_impl_str = join_non_empty_lines send_impl in
        let (env, var_name_gen), impl =
          gen_implementation indent false (env, var_name_gen) ltype'
        in
        ( (env, var_name_gen)
        , join_non_empty_lines ~sep:"\n\n" [send_impl_str; impl] )
    | SilentL _ -> assert false
  in
  let indent = incr_indent "" in
  let var_name_gen = Namegen.create () in
  let env, impl =
    gen_implementation indent false (ltype_env, var_name_gen) ltype
  in
  let impl =
    if is_dynamic_role then
      let call_wg_done =
        call_method VariableName.wait_group FunctionName.wait_group_done []
      in
      let defer_wg_done = defer_call call_wg_done in
      join_non_empty_lines [indent_line indent defer_wg_done; impl]
    else impl
  in
  (env, impl)

(** Result of code generation process *)
type codegen_result =
  { messages: string
  ; channels: string Map.M(ProtocolName).t
  ; invite_channels: string Map.M(ProtocolName).t
  ; results: string Map.M(ProtocolName).t
  ; impl: string Map.M(LocalProtocolName).t
  ; callbacks: string Map.M(LocalProtocolName).t
  ; protocol_setup: string Map.M(ProtocolName).t
  ; entry_point: string }

(** Generate empty code gen result *)
let init_result messages_file : codegen_result =
  { messages= messages_file
  ; channels= Map.empty (module ProtocolName)
  ; results= Map.empty (module ProtocolName)
  ; invite_channels= Map.empty (module ProtocolName)
  ; impl= Map.empty (module LocalProtocolName)
  ; callbacks= Map.empty (module LocalProtocolName)
  ; protocol_setup= Map.empty (module ProtocolName)
  ; entry_point= "" }

let show_codegen_result
    { messages
    ; channels
    ; results
    ; invite_channels
    ; impl
    ; callbacks
    ; protocol_setup
    ; entry_point } fst_protocol root_dir =
  let protocol_pkg_file_path pkg file_name protocol =
    let protocol_pkg = PackageName.protocol_pkg_name protocol in
    Printf.sprintf "%s/%s/%s/%s"
      (RootDirName.user root_dir)
      (PackageName.user pkg)
      (PackageName.user protocol_pkg)
      (FileName.user file_name)
  in
  let invitations_file_path protocol =
    Printf.sprintf "%s/%s/%s"
      (RootDirName.user root_dir)
      (PackageName.user PackageName.pkg_invitations)
      (FileName.user (FileName.invitations_file_name protocol))
  in
  let local_protocol_file_path local_protocol =
    Printf.sprintf "%s/%s/%s"
      (RootDirName.user root_dir)
      (PackageName.user PackageName.pkg_roles)
      (FileName.user (FileName.role_impl_file_name local_protocol))
  in
  let protocol_setup_file_path protocol =
    Printf.sprintf "%s/%s/%s"
      (RootDirName.user root_dir)
      (PackageName.user PackageName.pkg_roles)
      (FileName.user (FileName.protocol_setup_file_name protocol))
  in
  let entry_point_file_path protocol =
    Printf.sprintf "%s/%s/%s"
      (RootDirName.user root_dir)
      (PackageName.user PackageName.pkg_protocol)
      (FileName.user (FileName.protocol_file_name protocol))
  in
  let messages_file_path =
    Printf.sprintf "%s/%s/%s"
      (RootDirName.user root_dir)
      (PackageName.user PackageName.pkg_messages)
      (FileName.user FileName.messages_file_name)
  in
  let show_file file_path impl = Printf.sprintf "%s:\n\n%s" file_path impl in
  let show_files files ~gen_file_path =
    let files =
      Map.mapi files ~f:(fun ~key ~data:impl ->
          let file_path = gen_file_path key in
          show_file file_path impl )
    in
    String.concat ~sep:"\n\n" (Map.data files)
  in
  let message_file = show_file messages_file_path messages in
  let channel_files =
    show_files channels
      ~gen_file_path:
        (protocol_pkg_file_path PackageName.pkg_channels
           FileName.channels_file_name )
  in
  let result_files =
    show_files results
      ~gen_file_path:
        (protocol_pkg_file_path PackageName.pkg_results
           FileName.results_file_name )
  in
  let invitation_files =
    show_files invite_channels ~gen_file_path:invitations_file_path
  in
  let impl_files = show_files impl ~gen_file_path:local_protocol_file_path in
  let callback_files =
    show_files callbacks ~gen_file_path:local_protocol_file_path
  in
  let protocol_setup_files =
    show_files protocol_setup ~gen_file_path:protocol_setup_file_path
  in
  let entry_point_file =
    show_file (entry_point_file_path fst_protocol) entry_point
  in
  String.concat ~sep:"\n\n"
    [ message_file
    ; channel_files
    ; result_files
    ; invitation_files
    ; callback_files
    ; protocol_setup_files
    ; impl_files
    ; entry_point_file ]

module RolePair = struct
  module T = struct
    type t = ProtocolName.t * RoleName.t * RoleName.t
    [@@deriving sexp_of, ord]
  end

  include T
  include Comparable.Make (T)
end

module Namegen = Namegen.Make (VariableName)

module GoGenM = struct
  type ctx =
    { channels: (VariableName.t * goType) Map.M(RolePair).t
    ; ctx_vars: VariableName.t Map.M(LocalProtocolId).t
    ; curr_fn: LocalProtocolId.t option }

  type state =
    { namegen: Namegen.t
    ; lp_fns: LocalProtocolName.t Map.M(LocalProtocolId).t
    ; msg_iface: goType Map.M(ProtocolName).t
    ; ctx_type: goType Map.M(LocalProtocolId).t
    ; lp_ctx: ctx }
  (* type 'a t = state -> state * 'a *)

  let init =
    { namegen= Namegen.create ()
    ; lp_fns= Map.empty (module LocalProtocolId)
    ; msg_iface= Map.empty (module ProtocolName)
    ; ctx_type= Map.empty (module LocalProtocolId)
    ; lp_ctx=
        { channels= Map.empty (module RolePair)
        ; ctx_vars= Map.empty (module LocalProtocolId)
        ; curr_fn= None } }

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

let mk_sr_callback cb_ty lbl from =
  let open GoGenM.Syntax in
  pure
    (FunctionName.of_string
       (cb_ty ^ "_" ^ LabelName.user lbl ^ "_" ^ RoleName.user from ^ "_TODO") )

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

let get_proto_call _ _ =
  let open GoGenM.Syntax in
  pure (FunctionName.of_string "TODO_call_protocol")

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

let gen_local ~proto ~role lty =
  let open GoGenM.Syntax in
  let rec go pre go_impl lty =
    match lty with
    | RecvL ({label; _}, from, cont) ->
        let* var, k =
          match pre with
          | Some var -> pure (var, [])
          | None ->
              let* chan, _ = get_chan ~proto ~src:role ~dst:from in
              let* var = fresh "x" in
              let recv_stmt =
                GoAssign
                  ( var
                  , GoAssert
                      ( GoRecv (GoVar chan)
                      , VariableName.of_string
                          (LabelName.to_capitalize_string label) ) )
              in
              pure (var, [recv_stmt])
        in
        let* cb_nm = mk_sr_callback "Recv" label from in
        let recv_callback = GoExpr (GoCall (cb_nm, [GoVar var])) in
        go None ((recv_callback :: k) @ go_impl) cont
    | SendL ({label; _}, dst, cont) ->
        let* var, k =
          match pre with
          | Some var -> pure (var, [])
          | None ->
              let* cb_nm = mk_sr_callback "Send" label dst in
              let* var = fresh "x" in
              let send_callback = GoAssign (var, GoCall (cb_nm, [])) in
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

let gen_decl_body ~proto ~role lty =
  let open GoGenM.Syntax in
  let* _ = GoGenM.cleanup in
  let* _ = enter_decl ~proto ~role in
  let chans = required_channels ~role lty in
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

let gen_code_alt _root_dir _gen_protocol _global_t local_t =
  let genf =
    Map.fold local_t ~init:(GoGenM.Syntax.pure []) ~f:gen_local_func
  in
  ppr_prog (GoGenM.eval genf GoGenM.init)

(** Generate all the elements of the implementation of a Scribble module and
    the entry point protocol *)
let gen_code root_dir gen_protocol (global_t : Gtype.nested_t)
    (local_t : Ltype.nested_t) =
  let protocol_lookup = build_local_proto_name_lookup local_t in
  let protocol_setup_env =
    ProtocolSetupEnv.create protocol_lookup local_t global_t
  in
  (* TODO: fix messages env - should be preserved across protocols *)
  let messages_env = MessagesEnv.create gen_protocol in
  let messages_env =
    Map.fold global_t ~init:messages_env
      ~f:(fun ~key:_ ~data:{gtype; _} msgs_env ->
        gen_message_label_enums msgs_env gtype )
  in
  let gen_protocol_role_implementation ~key:protocol ~data result =
    let {static_roles; dynamic_roles; _} = data in
    let protocol_env =
      { channel_imports= ImportsEnv.create root_dir
      ; invite_imports= ImportsEnv.create root_dir
      ; callback_enum_names= EnumNamesEnv.create () }
    in
    let (result, protocol_env), envs =
      List.fold_map ~init:(result, protocol_env)
        ~f:(fun (({impl; callbacks; _} as result), protocol_env) role ->
          let local_protocol_id = LocalProtocolId.create protocol role in
          let _, ltype = Map.find_exn local_t local_protocol_id in
          let local_protocol =
            lookup_local_protocol protocol_lookup protocol role
          in
          let env =
            LTypeCodeGenEnv.create gen_protocol protocol role local_protocol
              root_dir protocol_env
          in
          let is_dynamic_role =
            List.mem dynamic_roles role ~equal:RoleName.equal
          in
          let (env, _), protocol_impl =
            gen_role_implementation messages_env protocol_setup_env env
              global_t protocol_lookup is_dynamic_role protocol role
              local_protocol ltype
          in
          let env, impl_file =
            gen_role_impl_file env protocol_impl is_dynamic_role role
              protocol local_protocol
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
          ((result, protocol_env), env) )
        (static_roles @ dynamic_roles)
    in
    let pkg = PackageName.protocol_pkg_name protocol in
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
    let results_file = gen_results_file pkg static_roles in
    let results =
      Map.add_exn result.results ~key:protocol ~data:results_file
    in
    let protocol_setup_gen =
      ProtocolSetupGen.create root_dir gen_protocol
        (static_roles @ dynamic_roles)
        protocol protocol_lookup
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
        protocol_lookup
        (static_roles, dynamic_roles)
    in
    let protocol_setup =
      Map.add_exn result.protocol_setup ~key:protocol ~data:setup_file
    in
    let entry_point =
      if ProtocolName.equal gen_protocol protocol then
        gen_entry_point_file protocol_lookup protocol static_roles
          setup_imports role_chan_vars invite_chan_vars setup_channels_impl
      else result.entry_point
    in
    { result with
      channels
    ; invite_channels
    ; results
    ; protocol_setup
    ; entry_point }
  in
  let messages_file = MessagesEnv.generate_messages_file messages_env in
  let initial_result = init_result messages_file in
  Map.fold ~init:initial_result ~f:gen_protocol_role_implementation global_t

(* let write_go_impl { messages ; channels ; invite_channels ; results ; impl
   ; callbacks ; protocol_setup ; entry_point } project_root root_pkg
   first_protocol = let write_protocol_pkgs protocol_impls pkg_name file_name
   = create_pkg pkg_name ; Map.iteri protocol_impls ~f:(fun ~key:protocol
   ~data:impl -> let protocol_pkg = PackageName.protocol_pkg_name protocol in
   let protocol_pkg_path = pkg_path [pkg_name; protocol_pkg] in create_dir
   protocol_pkg_path ; let file_name = FileName.user file_name in let
   file_path = gen_file_path protocol_pkg_path file_name in write_file
   file_path impl ) in let write_single_file_pkg pkg file_name impl =
   create_pkg pkg ; let file_path = gen_file_path (PackageName.user pkg)
   (FileName.user file_name) in write_file file_path impl in let
   write_messages () = write_single_file_pkg PackageName.pkg_messages
   FileName.messages_file_name messages in let write_results () =
   write_protocol_pkgs results PackageName.pkg_results
   FileName.results_file_name in let write_channels () = write_protocol_pkgs
   channels PackageName.pkg_channels FileName.channels_file_name in let
   write_invite_channels () = create_pkg PackageName.pkg_invitations ;
   Map.iteri invite_channels ~f:(fun ~key:protocol ~data:impl -> let
   file_name = FileName.invitations_file_name protocol in let file_name =
   FileName.user file_name in let file_path = gen_file_path (PackageName.user
   PackageName.pkg_invitations) file_name in write_file file_path impl ) in
   let write_entry_point () = write_single_file_pkg PackageName.pkg_protocol
   (FileName.protocol_file_name first_protocol) entry_point in let
   write_callbacks () = create_pkg PackageName.pkg_callbacks ; Map.iteri
   callbacks ~f:(fun ~key:local_protocol ~data:impl -> let file_name =
   FileName.callbacks_file_name local_protocol in let file_name =
   FileName.user file_name in let file_path = gen_file_path (PackageName.user
   PackageName.pkg_callbacks) file_name in write_file file_path impl ) in let
   write_roles () = create_pkg PackageName.pkg_roles ; let module Namegen =
   Namegen.Make (FileName) in let file_name_gen = Namegen.create () in let
   file_name_gen = Map.fold impl ~init:file_name_gen ~f:(fun
   ~key:local_protocol ~data:impl file_name_gen -> let file_name =
   FileName.role_impl_file_name local_protocol in let file_name_gen,
   file_name = Namegen.unique_name file_name_gen file_name in let file_path =
   gen_file_path (PackageName.user PackageName.pkg_roles) (FileName.user
   file_name) in write_file file_path impl ; file_name_gen ) in let _ =
   Map.fold protocol_setup ~init:file_name_gen ~f:(fun ~key:protocol
   ~data:impl file_name_gen -> let file_name =
   FileName.protocol_setup_file_name protocol in let file_name_gen, file_name
   = Namegen.unique_name file_name_gen file_name in let file_path =
   gen_file_path (PackageName.user PackageName.pkg_roles) (FileName.user
   file_name) in write_file file_path impl ; file_name_gen ) in () in
   change_dir (RootDirName.user project_root) ; create_pkg root_pkg ;
   change_dir (PackageName.user root_pkg) ; write_messages () ; write_results
   () ; write_channels () ; write_invite_channels () ; write_entry_point () ;
   write_callbacks () ; write_roles () *)

let generate_from_scr ast protocol root_dir =
  let global_t = nested_t_of_module ast in
  ensure_unique_identifiers global_t ;
  let local_t = Ltype.project_nested_t global_t in
  let local_t = Ltype.ensure_unique_tvars local_t in
  gen_code_alt root_dir protocol global_t local_t

(* let write_code_to_files result go_path out_dir protocol = let project_root
   = RootDirName.of_string @@ Printf.sprintf "%s/%s" go_path out_dir in let
   protocol_root_pkg = PackageName.protocol_pkg_name protocol in
   write_go_impl result project_root protocol_root_pkg protocol *)

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
