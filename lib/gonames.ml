open! Base
open Printf
open Names

(* GOLANG PACKAGE NAMES *)
let pkg_messages = PackageName.of_string "messages"

let pkg_channels = PackageName.of_string "channels"

let pkg_invitations = PackageName.of_string "invitations"

let pkg_callbacks = PackageName.of_string "callbacks"

let pkg_roles = PackageName.of_string "roles"

let pkg_protocol = PackageName.of_string "protocol"

let pkg_sync = PackageName.of_string "sync"

let pkg_results = PackageName.of_string "results"

(* VARIABLE NAMES *)
let env_var = VariableName.of_string "env"

let invite_chan = VariableName.of_string "inviteChannels"

let role_chan = VariableName.of_string "roleChannels"

let protocol_env_var = VariableName.of_string "protocolEnv"

let wait_group = VariableName.of_string "wg"

let result_var = VariableName.of_string "result"

(* TYPES *)
let int_type = "int"

let wait_group_type = VariableName.of_string "WaitGroup"

(* WAIT GROUP METHODS *)
let wait_group_add = FunctionName.of_string "Add"

let wait_group_done = FunctionName.of_string "Done"

let wait_group_wait = FunctionName.of_string "Wait"

(* PANIC MESSAGES *)
let todo_panic_msg = "TODO: implement me"

let invalid_choice_panic_msg = "Invalid choice was made"

(* DEFAULT CASE *)
let default_case = "default:"

(* CODEGEN FUNCTIONS *)

let capitalize_role_name role = String.capitalize @@ RoleName.user role

let lowercase_role_name role = String.lowercase @@ RoleName.user role

let capitalize_local_protocol local_protocol =
  String.capitalize @@ LocalProtocolName.user local_protocol

let capitalize_protocol protocol =
  String.capitalize @@ ProtocolName.user protocol

let lowercase_protocol protocol =
  String.lowercase @@ ProtocolName.user protocol

let capitalize_label label = String.capitalize @@ LabelName.user label

let lowercase_label label = String.lowercase @@ LabelName.user label

let lowercase_payload_type payload_type =
  String.lowercase @@ PayloadTypeName.user payload_type

let lowercase_local_protocol local_protocol =
  String.lowercase @@ LocalProtocolName.user local_protocol

let capitalize_payload_type payload_type =
  String.capitalize @@ PayloadTypeName.user payload_type

(* FILE NAMES *)

let gen_protocol_file_name protocol =
  sprintf "%s.go" (lowercase_protocol protocol)

let gen_local_protocol_file_name local_protocol =
  sprintf "%s.go" (lowercase_local_protocol local_protocol)

let messages_file_name = "messages.go"

let channels_file_name = "channels.go"

let results_file_name = "results.go"

let protocol_file_name protocol = gen_protocol_file_name protocol

let invitations_file_name protocol = gen_protocol_file_name protocol

let callbacks_file_name local_protocol =
  gen_local_protocol_file_name local_protocol

let role_impl_file_name local_protocol =
  gen_local_protocol_file_name local_protocol

let protocol_setup_file_name protocol =
  sprintf "%s_setup.go" (lowercase_protocol protocol)

(* UIDs for protocol calls *)
let gen_call_label protocol roles =
  let roles_str = String.concat ~sep:"," (List.map roles ~f:RoleName.user) in
  LabelName.of_string
  @@ Printf.sprintf "%s(%s)" (ProtocolName.user protocol) roles_str

(* MSGS *)
let msg_type_name msg = capitalize_label msg

let msg_field_name_from_type payload_type =
  String.capitalize @@ PayloadTypeName.user payload_type

let msg_field_name field_name =
  String.capitalize @@ VariableName.user field_name

let msg_enum_name msg_label = capitalize_label msg_label

let invitation_enum_name protocol roles =
  let roles_str =
    List.map roles ~f:(fun role -> String.capitalize @@ RoleName.user role)
  in
  let capitalized_protocol = capitalize_protocol protocol in
  String.concat ~sep:"_" (capitalized_protocol :: roles_str)

let message_label_enum_name protocol =
  EnumTypeName.of_string @@ sprintf "%s_Label" (capitalize_protocol protocol)

(* CHANNELS *)
let chan_struct_field_name role msg_type =
  sprintf "%s_%s" (capitalize_role_name role) (msg_type_name msg_type)

let chan_field_name role payload_str is_send =
  if is_send then sprintf "%s_To_%s" payload_str (RoleName.user role)
  else sprintf "%s_From_%s" payload_str (RoleName.user role)

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

let setup_chan_struct_name protocol =
  sprintf "%s_RoleSetupChan" (capitalize_protocol protocol)

let setup_invite_struct_name protocol =
  sprintf "%s_InviteSetupChan" (capitalize_protocol protocol)

let setup_role_chan_name role = sprintf "%s_Chan" (capitalize_role_name role)

let setup_invite_chan_name role =
  sprintf "%s_InviteChan" (capitalize_role_name role)

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

let entry_point_protocol_env_interface_name protocol =
  let interface_name = sprintf "%s_Env" (capitalize_protocol protocol) in
  InterfaceName.of_string interface_name

let result_callback_name local_protocol =
  sprintf "ResultFrom_%s" (capitalize_local_protocol local_protocol)

let done_callback = CallbackName.of_string "Done"

let extract_var_name_str var_name_opt =
  VariableName.user @@ Option.value_exn var_name_opt

let var_to_param_name var_name_opt =
  ParameterName.of_string @@ extract_var_name_str var_name_opt

(* CHOICE ENUMS *)
let choice_enum_value local_protocol label =
  sprintf "%s_%s"
    (capitalize_local_protocol local_protocol)
    (capitalize_label label)

let choice_enum_type local_protocol =
  sprintf "%s_Choice" (capitalize_local_protocol local_protocol)

(* RESULTS *)
let result_struct_name role = sprintf "%s_Result" (capitalize_role_name role)

(* PKGS *)
let protocol_pkg_name protocol =
  String.lowercase @@ ProtocolName.user protocol

(* FUNCTION NAMES *)
let local_protocol_function_name local_protocol =
  capitalize_local_protocol local_protocol

let protocol_setup_function_name protocol =
  sprintf "%s_SendCommChannels" (capitalize_protocol protocol)

let new_create_env_function_name local_protocol =
  sprintf "New_%s_State" (capitalize_local_protocol local_protocol)

let new_init_role_env_function_name role =
  FunctionName.of_string @@ sprintf "New_%s_Env" (capitalize_role_name role)

let new_init_role_result_function_name role =
  FunctionName.of_string @@ sprintf "%s_Result" (capitalize_role_name role)

let role_start_function_name local_protocol =
  sprintf "Start_%s" (capitalize_local_protocol local_protocol)

let entry_point_function_name protocol =
  FunctionName.of_string @@ capitalize_protocol protocol

(* VARIABLES *)
let new_role_chan_var local_protocol =
  sprintf "%s_chan" (lowercase_local_protocol local_protocol)

let new_role_invite_chan_var local_protocol =
  sprintf "%s_inviteChan" (lowercase_local_protocol local_protocol)

let new_env_var local_protocol =
  sprintf "%s_env" (lowercase_local_protocol local_protocol)

let new_result_var local_protocol =
  sprintf "%s_result" (lowercase_local_protocol local_protocol)

let new_msg_var msg_label = sprintf "%s_msg" (lowercase_label msg_label)

let new_choice_enum_var role = sprintf "%s_choice" (lowercase_role_name role)

let new_role_setup_var protocol =
  sprintf "%s_rolechan" (lowercase_protocol protocol)

let new_invite_setup_var protocol =
  sprintf "%s_invitechan" (lowercase_protocol protocol)

let new_role_chan_setup_var role =
  sprintf "%s_chan" (lowercase_role_name role)

let new_invite_chan_setup_var role =
  sprintf "%s_inviteChan" (lowercase_role_name role)

let new_msg_chan_var sender recv label =
  sprintf "%s_%s_%s"
    (lowercase_role_name sender)
    (lowercase_role_name recv)
    (lowercase_label label)

let new_data_chan_var sender recv payload =
  (* TODO: assumes that the types are valid identifiers *)
  let payload_name =
    match payload with None -> "label" | Some p -> lowercase_payload_type p
  in
  sprintf "%s_%s_%s"
    (lowercase_role_name sender)
    (lowercase_role_name recv)
    payload_name

let new_setup_role_chan_var caller participant =
  sprintf "%s_invite_%s"
    (lowercase_role_name caller)
    (lowercase_role_name participant)

let new_setup_invite_chan_var caller participant =
  sprintf "%s_invite_%s_invitechan"
    (lowercase_role_name caller)
    (lowercase_role_name participant)

let new_role_env_var role =
  VariableName.of_string @@ sprintf "%s_env" (lowercase_role_name role)
