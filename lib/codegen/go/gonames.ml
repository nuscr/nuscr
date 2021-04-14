open! Base
open Printf
open Names

let extract_var_name_str var_name_opt = Option.value_exn var_name_opt

module type ExtendedTaggedName = sig
  include TaggedName

  val to_capitalize_string : t -> string

  val to_lowercase_string : t -> string

  val capitalize : t -> t

  val lowercase : t -> t
end

module ExtendName (N : TaggedName) = struct
  include N

  let to_capitalize_string n = String.capitalize (user n)

  let to_lowercase_string n = String.lowercase (user n)

  let capitalize = update ~f:String.capitalize

  let lowercase = update ~f:String.lowercase
end

module RoleName = ExtendName (RoleName)
module ProtocolName = ExtendName (ProtocolName)
module LocalProtocolName = ExtendName (LocalProtocolName)
module PayloadTypeName = ExtendName (PayloadTypeName)

module LabelName = struct
  include ExtendName (LabelName)

  (* UIDs for protocol calls *)
  let gen_call_label protocol roles =
    let roles_str =
      String.concat ~sep:"," (List.map roles ~f:RoleName.user)
    in
    of_string
    @@ Printf.sprintf "%s(%s)" (ProtocolName.user protocol) roles_str
end

module VariableName = struct
  include ExtendName (VariableName)

  let new_role_chan_var local_protocol =
    of_string
      (sprintf "%s_chan"
         (LocalProtocolName.to_lowercase_string local_protocol) )

  let new_role_invite_chan_var local_protocol =
    of_string
      (sprintf "%s_inviteChan"
         (LocalProtocolName.to_lowercase_string local_protocol) )

  let new_env_var local_protocol =
    of_string
      (sprintf "%s_env"
         (LocalProtocolName.to_lowercase_string local_protocol) )

  let new_result_var local_protocol =
    of_string
      (sprintf "%s_result"
         (LocalProtocolName.to_lowercase_string local_protocol) )

  let new_choice_enum_var role =
    of_string (sprintf "%s_choice" (RoleName.to_lowercase_string role))

  let new_role_setup_var protocol =
    of_string
      (sprintf "%s_rolechan" (ProtocolName.to_lowercase_string protocol))

  let new_invite_setup_var protocol =
    of_string
      (sprintf "%s_invitechan" (ProtocolName.to_lowercase_string protocol))

  let new_role_chan_setup_var role =
    of_string (sprintf "%s_chan" (RoleName.to_lowercase_string role))

  let new_invite_chan_setup_var role =
    of_string (sprintf "%s_inviteChan" (RoleName.to_lowercase_string role))

  let new_data_chan_var sender recv payload =
    (* TODO: assumes that the types are valid identifiers *)
    let payload_name =
      match payload with
      | None -> "label"
      | Some p -> PayloadTypeName.to_lowercase_string p
    in
    of_string
      (sprintf "%s_%s_%s"
         (RoleName.to_lowercase_string sender)
         (RoleName.to_lowercase_string recv)
         payload_name )

  let new_setup_role_chan_var caller participant =
    of_string
      (sprintf "%s_invite_%s"
         (RoleName.to_lowercase_string caller)
         (RoleName.to_lowercase_string participant) )

  let new_setup_invite_chan_var caller participant =
    of_string
      (sprintf "%s_invite_%s_invitechan"
         (RoleName.to_lowercase_string caller)
         (RoleName.to_lowercase_string participant) )

  let new_role_env_var role =
    of_string (sprintf "%s_env" (RoleName.to_lowercase_string role))

  (* VARIABLE NAMES *)
  let env_var = VariableName.of_string "env"

  let invite_chan = VariableName.of_string "inviteChannels"

  let role_chan = VariableName.of_string "roleChannels"

  let protocol_env_var = VariableName.of_string "protocolEnv"

  let wait_group = VariableName.of_string "wg"

  let result_var = VariableName.of_string "result"

  let wait_group_type = VariableName.of_string "WaitGroup"
end

module ChannelStructName = struct
  include Make ()

  let chan_struct_name role_name =
    of_string (sprintf "%s_Chan" (RoleName.to_capitalize_string role_name))
end

module ChannelName = struct
  include Make ()

  let chan_field_name role payload_str ~is_send =
    let str =
      if is_send then sprintf "%s_To_%s" payload_str (RoleName.user role)
      else sprintf "%s_From_%s" payload_str (RoleName.user role)
    in
    of_string str
end

module InviteChannelStructName = struct
  include Make ()

  let invite_struct_name local_protocol =
    of_string
      (sprintf "%s_InviteChan"
         (LocalProtocolName.to_capitalize_string local_protocol) )

  let setup_chan_struct_name protocol =
    of_string
      (sprintf "%s_RoleSetupChan"
         (ProtocolName.to_capitalize_string protocol) )

  let setup_invite_struct_name protocol =
    of_string
      (sprintf "%s_InviteSetupChan"
         (ProtocolName.to_capitalize_string protocol) )
end

module InviteChannelName = struct
  include Make ()

  let send_role_chan_name_str role_name local_protocol =
    sprintf "Invite_%s_To_%s"
      (RoleName.to_capitalize_string role_name)
      (LocalProtocolName.to_capitalize_string local_protocol)

  let send_role_chan_name role_name local_protocol =
    of_string (send_role_chan_name_str role_name local_protocol)

  let send_role_invite_chan_name role_name local_protocol =
    of_string
      (sprintf "%s_InviteChan"
         (send_role_chan_name_str role_name local_protocol) )

  let recv_role_chan_name_str caller local_protocol =
    sprintf "%s_Invite_To_%s"
      (RoleName.to_capitalize_string caller)
      (LocalProtocolName.to_capitalize_string local_protocol)

  let recv_role_chan_name caller local_protocol =
    of_string (recv_role_chan_name_str caller local_protocol)

  let recv_role_invite_chan_name role_name local_protocol =
    of_string
      (sprintf "%s_InviteChan"
         (recv_role_chan_name_str role_name local_protocol) )

  let setup_role_chan_name role =
    of_string (sprintf "%s_Chan" (RoleName.to_capitalize_string role))

  let setup_invite_chan_name role =
    of_string (sprintf "%s_InviteChan" (RoleName.to_capitalize_string role))
end

module CallbackName = struct
  include Make ()

  let send_callback_name msg_label recv_role =
    of_string
      (sprintf "%s_To_%s"
         (LabelName.to_capitalize_string msg_label)
         (RoleName.to_capitalize_string recv_role) )

  let recv_callback_name msg_label recv_role =
    of_string
      (sprintf "%s_From_%s"
         (LabelName.to_capitalize_string msg_label)
         (RoleName.to_capitalize_string recv_role) )

  let protocol_setup_callback_name protocol =
    of_string
      (sprintf "%s_Setup" (ProtocolName.to_capitalize_string protocol))

  let convert_env_callback_name local_protocol =
    of_string
      (sprintf "To_%s_Env"
         (LocalProtocolName.to_capitalize_string local_protocol) )

  let choice_callback_name role =
    of_string (sprintf "%s_Choice" (RoleName.to_capitalize_string role))

  let result_callback_name local_protocol =
    of_string
      (sprintf "ResultFrom_%s"
         (LocalProtocolName.to_capitalize_string local_protocol) )

  let done_callback = of_string "Done"
end

module CallbacksEnvName = struct
  include Make ()

  let callbacks_env_name local_protocol =
    of_string
      (sprintf "%s_Env"
         (LocalProtocolName.to_capitalize_string local_protocol) )
end

module MessageStructName : TaggedName = Make ()

module FileName = struct
  include Make ()

  let gen_protocol_file_name protocol =
    of_string (sprintf "%s.go" (ProtocolName.to_lowercase_string protocol))

  let gen_local_protocol_file_name local_protocol =
    of_string
      (sprintf "%s.go"
         (LocalProtocolName.to_lowercase_string local_protocol) )

  let messages_file_name = of_string "messages.go"

  let channels_file_name = of_string "channels.go"

  let results_file_name = of_string "results.go"

  let protocol_file_name protocol = gen_protocol_file_name protocol

  let invitations_file_name protocol = gen_protocol_file_name protocol

  let callbacks_file_name local_protocol =
    gen_local_protocol_file_name local_protocol

  let role_impl_file_name local_protocol =
    gen_local_protocol_file_name local_protocol

  let protocol_setup_file_name protocol =
    of_string
      (sprintf "%s_setup.go" (ProtocolName.to_lowercase_string protocol))
end

(* TODO: Is it needed? *)
module ResultName = struct
  include Make ()

  let result_struct_name role =
    of_string (sprintf "%s_Result" (RoleName.to_capitalize_string role))
end

module PackageName = struct
  include Make ()

  let pkg_messages = of_string "messages"

  let pkg_channels = of_string "channels"

  let pkg_invitations = of_string "invitations"

  let pkg_callbacks = of_string "callbacks"

  let pkg_roles = of_string "roles"

  let pkg_protocol = of_string "protocol"

  let pkg_sync = of_string "sync"

  let pkg_results = of_string "results"

  let protocol_pkg_name protocol =
    of_string (String.lowercase (ProtocolName.user protocol))
end

module ParameterName = struct
  include Make ()

  let of_var_opt var_name_opt =
    of_other_name (module VariableName) (extract_var_name_str var_name_opt)
end

module RootDirName : TaggedName = Make ()

module EnumTypeName = struct
  include Make ()

  let message_label_enum_name protocol =
    of_string
      (sprintf "%s_Label" (ProtocolName.to_capitalize_string protocol))

  let choice_enum_type local_protocol =
    of_string
      (sprintf "%s_Choice"
         (LocalProtocolName.to_capitalize_string local_protocol) )
end

module InterfaceName = struct
  include Make ()

  let entry_point_protocol_env_interface_name protocol =
    of_string (sprintf "%s_Env" (ProtocolName.to_capitalize_string protocol))
end

module FunctionName = struct
  include ExtendName (Make ())

  (* WAIT GROUP METHODS *)
  let wait_group_add = of_string "Add"

  let wait_group_done = of_string "Done"

  let wait_group_wait = of_string "Wait"

  let local_protocol_function_name local_protocol =
    of_string (LocalProtocolName.to_capitalize_string local_protocol)

  let protocol_setup_function_name protocol =
    of_string
      (sprintf "%s_SendCommChannels"
         (ProtocolName.to_capitalize_string protocol) )

  let new_init_role_env_function_name role =
    of_string (sprintf "New_%s_Env" (RoleName.to_capitalize_string role))

  let new_init_role_result_function_name role =
    of_string (sprintf "%s_Result" (RoleName.to_capitalize_string role))

  let new_create_env_function_name local_protocol =
    of_string
      (sprintf "New_%s_State"
         (LocalProtocolName.to_capitalize_string local_protocol) )

  let role_start_function_name local_protocol =
    of_string
      (sprintf "Start_%s"
         (LocalProtocolName.to_capitalize_string local_protocol) )

  let entry_point_function_name protocol =
    of_string (ProtocolName.to_capitalize_string protocol)
end

module EnumName = struct
  include Make ()

  let msg_enum_name msg_label =
    of_string (LabelName.to_capitalize_string msg_label)

  let invitation_enum_name protocol roles =
    let roles_str = List.map roles ~f:RoleName.to_capitalize_string in
    let capitalized_protocol = ProtocolName.to_capitalize_string protocol in
    of_string (String.concat ~sep:"_" (capitalized_protocol :: roles_str))

  let choice_enum_value local_protocol label =
    of_string
      (sprintf "%s_%s"
         (LocalProtocolName.to_capitalize_string local_protocol)
         (LabelName.to_capitalize_string label) )
end

(* PANIC MESSAGES *)
let todo_panic_msg = "TODO: implement me"

let invalid_choice_panic_msg = "Invalid choice was made"

let default_case = "default:"

let pkg_path pkgs =
  let str_pkgs = List.map ~f:PackageName.user pkgs in
  String.concat ~sep:"/" str_pkgs
