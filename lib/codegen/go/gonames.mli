open Names

module type ExtendedTaggedName = sig
  include TaggedName

  val to_capitalize_string : t -> string

  val to_lowercase_string : t -> string

  val capitalize : t -> t

  val lowercase : t -> t
end

module LabelName : sig
  include ExtendedTaggedName with type t = LabelName.t

  val gen_call_label : ProtocolName.t -> RoleName.t list -> t
end

module ChannelStructName : sig
  include TaggedName

  val chan_struct_name : RoleName.t -> t
end

module ChannelName : sig
  include TaggedName

  val chan_field_name : RoleName.t -> string -> is_send:bool -> t
end

module InviteChannelStructName : sig
  include TaggedName

  val invite_struct_name : LocalProtocolName.t -> t

  val setup_invite_struct_name : ProtocolName.t -> t

  val setup_chan_struct_name : ProtocolName.t -> t
end

module InviteChannelName : sig
  include TaggedName

  val send_role_chan_name : RoleName.t -> LocalProtocolName.t -> t

  val send_role_invite_chan_name : RoleName.t -> LocalProtocolName.t -> t

  val recv_role_chan_name : RoleName.t -> LocalProtocolName.t -> t

  val recv_role_invite_chan_name : RoleName.t -> LocalProtocolName.t -> t

  val setup_role_chan_name : RoleName.t -> t

  val setup_invite_chan_name : RoleName.t -> t
end

module CallbackName : sig
  include TaggedName

  val send_callback_name : LabelName.t -> RoleName.t -> t

  val recv_callback_name : LabelName.t -> RoleName.t -> t

  val protocol_setup_callback_name : ProtocolName.t -> t

  val convert_env_callback_name : LocalProtocolName.t -> t

  val result_callback_name : LocalProtocolName.t -> t

  val choice_callback_name : RoleName.t -> t

  val done_callback : t
end

module CallbacksEnvName : sig
  include TaggedName

  val callbacks_env_name : LocalProtocolName.t -> t
end

module MessageStructName : TaggedName

module FileName : sig
  include TaggedName

  val invitations_file_name : ProtocolName.t -> t

  val role_impl_file_name : LocalProtocolName.t -> t

  val protocol_setup_file_name : ProtocolName.t -> t

  val protocol_file_name : ProtocolName.t -> t

  val callbacks_file_name : LocalProtocolName.t -> t

  val messages_file_name : t

  val channels_file_name : t

  val results_file_name : t
end

(* TODO: Is it needed? *)
module ResultName : sig
  include TaggedName

  val result_struct_name : RoleName.t -> t
end

module PackageName : sig
  include TaggedName

  val pkg_messages : t

  val pkg_channels : t

  val pkg_invitations : t

  val pkg_callbacks : t

  val pkg_roles : t

  val pkg_protocol : t

  val pkg_sync : t

  val pkg_results : t

  val protocol_pkg_name : ProtocolName.t -> t
end

module ParameterName : sig
  include TaggedName

  val of_var_opt : VariableName.t option -> t
end

module RootDirName : TaggedName

module EnumName : sig
  include TaggedName

  val msg_enum_name : LabelName.t -> t

  val invitation_enum_name : ProtocolName.t -> RoleName.t list -> t

  val choice_enum_value : LocalProtocolName.t -> LabelName.t -> t
end

module EnumTypeName : sig
  include TaggedName

  val message_label_enum_name : ProtocolName.t -> t

  val choice_enum_type : LocalProtocolName.t -> t
end

module FunctionName : sig
  include TaggedName

  val protocol_setup_function_name : ProtocolName.t -> t

  val local_protocol_function_name : LocalProtocolName.t -> t

  val new_create_env_function_name : LocalProtocolName.t -> t

  val new_init_role_env_function_name : RoleName.t -> t

  val new_init_role_result_function_name : RoleName.t -> t

  val role_start_function_name : LocalProtocolName.t -> t

  val entry_point_function_name : ProtocolName.t -> t

  val wait_group_add : t

  val wait_group_done : t

  val wait_group_wait : t
end

module InterfaceName : sig
  include TaggedName

  val entry_point_protocol_env_interface_name : ProtocolName.t -> t
end

module RoleName : sig
  include ExtendedTaggedName with type t = RoleName.t
end

module VariableName : sig
  include ExtendedTaggedName with type t = VariableName.t

  val new_choice_enum_var : RoleName.t -> t

  val new_data_chan_var :
    RoleName.t -> RoleName.t -> PayloadTypeName.t option -> t

  val new_env_var : LocalProtocolName.t -> t

  val new_invite_chan_setup_var : RoleName.t -> t

  val new_invite_setup_var : ProtocolName.t -> t

  val new_result_var : LocalProtocolName.t -> t

  val new_role_chan_setup_var : RoleName.t -> t

  val new_role_chan_var : LocalProtocolName.t -> t

  val new_role_invite_chan_var : LocalProtocolName.t -> t

  val new_role_setup_var : ProtocolName.t -> t

  val new_role_env_var : RoleName.t -> t

  val new_setup_invite_chan_var : RoleName.t -> RoleName.t -> t

  val new_setup_role_chan_var : RoleName.t -> RoleName.t -> t

  val result_var : t

  val invite_chan : t

  val env_var : t

  val wait_group : t

  val role_chan : t

  val wait_group_type : t

  val protocol_env_var : t
end

module PayloadTypeName : sig
  include ExtendedTaggedName with type t = PayloadTypeName.t
end

val default_case : string

val todo_panic_msg : string

val invalid_choice_panic_msg : string

val pkg_path : PackageName.t list -> string

module LocalProtocolName : sig
  include ExtendedTaggedName with type t = LocalProtocolName.t
end
