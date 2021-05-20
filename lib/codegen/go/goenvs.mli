open! Base
open Names
open Mpst
open Gtype
open Ltype

(** Environment which keeps track of the package imports which which need to
    be generated. Importing new packages multiple times doesn't change the
    environment, and importing two packages with the same name will result in
    different package aliases being generated*)
module ImportsEnv : sig
  type t

  val import_messages : t -> t * PackageName.t
  (** Import package 'messages/<protocol>'*)

  val import_channels : t -> ProtocolName.t -> t * PackageName.t
  (** Import package 'channels/<protocol>'*)

  val import_results : t -> ProtocolName.t -> t * PackageName.t
  (** Import package 'results/<protocol>'*)

  val import_invitations : t -> t * PackageName.t
  (** Import package 'invitations'*)

  val import_callbacks : t -> t * PackageName.t
  (** Import package 'callbacks'*)

  val import_roles : t -> t * PackageName.t
  (** Import package 'roles'*)

  val import_sync : t -> t * PackageName.t
  (** Import package 'sync'*)

  val generate_imports : t -> string
  (** Generate all the Go import statements from the given environment *)

  val create : RootDirName.t -> t
  (** Create a new empty environment. Root directory is used to generate
      relative imports *)
end

(** Environment for generating structs for the different messages exchanged
    during a protocol. Every unique message label will correspond to a
    different struct *)
module MessagesEnv : sig
  type t

  val create : ProtocolName.t -> t

  val add_message_enum : t -> LabelName.t -> t
  (** Add enum for message label if it doesn't already exist. *)

  val add_invitation_enum : t -> ProtocolName.t -> RoleName.t list -> t
  (** Add enum for protocol call if it doesn't already exist *)

  val get_message_enum : t -> LabelName.t -> EnumName.t
  (** Get enum value for message label *)

  val get_enum_type_name : t -> EnumTypeName.t
  (** Get name of enum type *)

  val get_invitation_enum :
    t -> ProtocolName.t -> RoleName.t list -> EnumName.t
  (** Get enum value for protocol call *)

  val generate_messages_file : t -> string
  (** Generate Go source file containing the message structs defined in the
      environment *)
end

(** Environment used to keep track of all the enums which have been created
    within the 'callbacks' package in order to ensure that the generated enum
    names are unique *)
module EnumNamesEnv : sig
  type t

  val create : unit -> t

  val new_enum :
       t
    -> LocalProtocolName.t
    -> LabelName.t list
    -> t * EnumTypeName.t * EnumName.t list
  (** Generate unique names for the enum type and enum values based on the
      local protocol and the labels of the first interactions in the choice *)
end

(** Environment to generate the channel struct for a role in a protocol. The
    environment keeps track of all the field names and types in order to
    generate the struct declaration *)
module ChannelEnv : sig
  type t

  val get_or_add_channel :
       t
    -> RoleName.t * PayloadTypeName.t option * bool
    -> t * (ChannelName.t * string)
  (** Generate new struct field for a channel to send or receive a payload or
      message label *)

  val gen_channel_struct : t -> string
  (** Generate role channel struct declaration *)

  val get_channel_imports : t -> ImportsEnv.t
  (** Return the imports environment with the imports needed for the channels
      file which have been aggregated *)

  val struct_name : t -> ChannelStructName.t
  (** Returns the name of the channel struct *)

  val create :
    RoleName.t -> ProtocolName.t -> ProtocolName.t -> ImportsEnv.t -> t
end

(** Environment to generate the invitation channel struct for a role in a
    protocol. The environment keeps track of all the field names and types in
    order to generate the struct declaration *)
module InviteEnv : sig
  type t

  type role_channel_field =
    InviteChannelName.t * (PackageName.t * ChannelStructName.t)

  type role_invite_channel_field =
    InviteChannelName.t * InviteChannelStructName.t

  val get_or_add_send_invitation_channels :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * role_channel_field * role_invite_channel_field
  (** Generate, if they don't exist already, two new struct field for sending
      the invitations to a role (potentially itself). Invitations consist of
      the channel struct and invitation struct that the role will need to
      communicate in the nested protocol *)

  val get_or_add_recv_invitation_channels :
       t
    -> RoleName.t
    -> LocalProtocolName.t
    -> RoleName.t
    -> ProtocolName.t
    -> t * role_channel_field * role_invite_channel_field
  (** Generate, if they don't exist already, two new struct field for
      receiving the invitations from the role which is initiating a protocol
      call - this should not be called if the role itself is initiating the
      protocol call. Invitations consist of the channel struct and invitation
      struct that the role will need to communicate in the nested protocol *)

  val gen_invite_channel_struct : t -> string
  (** Generate invitation channel struct *)

  val get_invite_imports : t -> ImportsEnv.t
  (** Return imports environment with all the imports for the invitations
      source file aggregated so far *)

  val struct_name : t -> InviteChannelStructName.t
  (** Return name of the invitation channel struct *)

  val create : LocalProtocolName.t -> ImportsEnv.t -> t
end

(** Environment used to keep track of the structs containing the channels
    which will be used during the setup of each protocol *)
module ProtocolSetupEnv : sig
  type t

  val get_setup_channel_struct :
       t
    -> ProtocolName.t
    -> InviteChannelStructName.t * InviteChannelName.t list
  (** Return the struct name and fields of the channel setup struct for the
      given protocol *)

  val get_setup_invite_struct :
       t
    -> ProtocolName.t
    -> InviteChannelStructName.t * InviteChannelName.t list
  (** Return the struct name and fields of the invitation setup struct for
      the given protocol *)

  val get_setup_function_name : t -> ProtocolName.t -> FunctionName.t
  (** Generate name for a protocol's setup function *)

  val gen_setup_channel_struct :
    t -> PackageName.t -> ProtocolName.t -> string
  (** Generate setup channel struct declaration for the given protocol *)

  val gen_setup_invite_struct : t -> ProtocolName.t -> string
  (** Generate setup invitation struct declaration for the given protocol *)

  val create : local_proto_name_lookup -> local_t -> global_t -> t
  (** Create new environment with all the information about the structs *)
end

(** Environment for creating all of the channel variables and structs during
    the setup of a protocol *)
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
  (** Generate the assignments for all of the channels needed in a protocol
      and create all the channel and invitation structs for each role with
      them. Return the names of the variables which contain the channel and
      invitation structs for each role as well as the setup implementation. *)
end

(** Environment used to generate the callbacks interface for a role. It keeps
    track of all the callbacks that a role's implemenatation needs and
    generates the callbacks source file for that role, ensuring that the
    names of the generated callbacks are unique*)
module CallbacksEnv : sig
  type t

  val new_send_callback :
    t -> LabelName.t -> payload list -> RoleName.t -> t * CallbackName.t
  (** Add callback for returning the payloads for the message which will be
      sent in a labelled message exchange *)

  val new_recv_callback :
    t -> LabelName.t -> payload list -> RoleName.t -> t * CallbackName.t
  (** Add callback which takes in the message which was received in labelled
      message exchange *)

  val new_protocol_setup_callback : t -> ProtocolName.t -> t * CallbackName.t
  (** Add callback for setting up a protocol call *)

  val new_convert_env_callback :
    t -> LocalProtocolName.t -> t * CallbackName.t
  (** Add callback for creating the environment for the new role that the
      participant will carry out in a protocol call *)

  val new_choice_callback :
       t
    -> RoleName.t
    -> LabelName.t list
    -> LocalProtocolName.t
    -> t * CallbackName.t * EnumName.t list
  (** Add callback for returning the choice that the role will make *)

  val new_protocol_result_callback :
       t
    -> LocalProtocolName.t
    -> ProtocolName.t
    -> RoleName.t
    -> t * CallbackName.t
  (** Add callback for receiving the result which was returned after
      finishing a protocol call *)

  val add_done_callback :
    t -> ProtocolName.t -> RoleName.t -> bool -> t * CallbackName.t
  (** Add callback for finishing the protocol's execution: Done(). This
      callback is only added once, and depending on whether the role is a
      dynamic participant or not, the function will be void or return a
      result *)

  val get_enum_names_env : t -> EnumNamesEnv.t
  (** Return the enum names environment which stores all the choice enums
      generated in the callbacks package so far *)

  val gen_callbacks_file : t -> LocalProtocolName.t -> bool -> string
  (** Generate callbacks source file for the role *)

  val create : RootDirName.t -> EnumNamesEnv.t -> t
end

(* Information aggregated throughout the code generation of all the
   projections of a global protocol *)
type protocol_env =
  { channel_imports: ImportsEnv.t
  ; invite_imports: ImportsEnv.t
  ; callback_enum_names: EnumNamesEnv.t }

(** Wrapper around all the different environments used during the code
    generation *)
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

  (* ChannelEnv *)
  val get_or_add_channel :
    t -> RoleName.t * PayloadTypeName.t option * bool -> t * ChannelName.t

  val gen_channel_struct : t -> string

  (* InviteEnv *)
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

  (* CallbacksEnv *)
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

  val gen_callbacks_file : t -> LocalProtocolName.t -> bool -> string

  val gen_result_struct : RoleName.t -> string
  (** Generate result struct declaration for a role *)

  (* ImportsEnv *)
  val import_messages : t -> t * PackageName.t

  val import_channels : t -> ProtocolName.t -> t * PackageName.t

  val import_results : t -> ProtocolName.t -> t * PackageName.t

  val import_invitations : t -> t * PackageName.t

  val import_callbacks : t -> t * PackageName.t

  val import_sync : t -> t * PackageName.t

  val generate_role_imports : t -> string

  val get_protocol_env : t -> protocol_env
end
