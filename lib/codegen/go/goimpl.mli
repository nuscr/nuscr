open Names
open! Gonames

val gen_import : string -> string

val gen_aliased_import : string -> string -> string

val messages_import_path : RootDirName.t -> PackageName.t -> string

val channels_import_path : RootDirName.t -> PackageName.t -> string

val invitations_import_path : RootDirName.t -> PackageName.t -> string

val callbacks_import_path : RootDirName.t -> PackageName.t -> string

val results_import_path : RootDirName.t -> PackageName.t -> string

val roles_import_path : RootDirName.t -> PackageName.t -> string

val sync_import_path : RootDirName.t -> PackageName.t -> string

val package_stmt : PackageName.t -> string

val gen_enum : EnumTypeName.t * EnumName.t list -> string

val pkg_enum_type_access : PackageName.t -> EnumTypeName.t -> string

val pkg_enum_access : PackageName.t -> EnumName.t -> string

val pkg_var_access : PackageName.t -> VariableName.t -> string

val pkg_function : PackageName.t -> FunctionName.t -> FunctionName.t

val chan_type : string -> string

val int_type : string

val pointer_type : string -> string

val role_chan_field_decl :
  InviteChannelName.t * (PackageName.t * ChannelStructName.t) -> string

val invite_chan_field_decl :
  InviteChannelName.t * InviteChannelStructName.t -> string

val invite_channel_struct_field_access :
  VariableName.t -> InviteChannelName.t -> string

val protocol_channel_access : PackageName.t -> ChannelStructName.t -> string

val protocol_result_access : PackageName.t -> ResultName.t -> string

val protocol_invite_channel_access :
  PackageName.t -> InviteChannelStructName.t -> string

val invitation_pkg_access :
  PackageName.t -> InviteChannelStructName.t -> string

val callbacks_pkg_env : PackageName.t -> CallbacksEnvName.t -> string

val callbacks_env_interface :
     CallbacksEnvName.t
  -> ( CallbackName.t
     * [< `Payloads of (ParameterName.t * PayloadTypeName.t) list
       | `Result of ParameterName.t * (PackageName.t * ResultName.t) ]
       option
     * [< `Enum of EnumTypeName.t
       | `Env of CallbacksEnvName.t
       | `Payloads of PayloadTypeName.t list
       | `Result of PackageName.t * ResultName.t ]
       option )
     list
  -> string

val ignore_recv_msg : VariableName.t -> ChannelName.t -> string

val recv_from_invite_chan :
  VariableName.t -> VariableName.t -> InviteChannelName.t -> string

val recv_from_msg_chan :
  VariableName.t -> VariableName.t -> ChannelName.t -> string

val send_msg_over_channel :
  VariableName.t -> ChannelName.t -> VariableName.t -> string

val send_value_over_channel :
  VariableName.t -> ChannelName.t -> string -> string

val send_invite_over_channel :
  VariableName.t -> InviteChannelName.t * VariableName.t -> string

val function_decl :
     FunctionName.t
  -> (VariableName.t * string) list
  -> string option
  -> string
  -> string

val struct_field_decl : VariableName.t -> string -> string

val struct_decl : string -> string list -> string

val struct_literal : string -> string list -> string list -> string -> string

val call_method : VariableName.t -> FunctionName.t -> string list -> string

val call_function : FunctionName.t -> VariableName.t list -> string

val goroutine_call : string -> string

val defer_call : string -> string

val gen_switch_stmt :
     VariableName.t
  -> PackageName.t
  -> EnumName.t list
  -> string list
  -> string
  -> string option
  -> string

val gen_interface :
  InterfaceName.t -> (FunctionName.t * string list * string) list -> string

val return_stmt : string -> string

val continue_stmt : TypeVariableName.t -> string

val recursion_label : TypeVariableName.t -> string

val recursion_loop : string -> string -> string

val recursion_impl : string -> string -> string

val new_variable :
     Namegen.Make(VariableName).t
  -> VariableName.t
  -> Namegen.Make(VariableName).t * VariableName.t

val reference_var : VariableName.t -> VariableName.t

val var_type_decl : VariableName.t -> string -> string

val new_var_decl : VariableName.t -> string -> string

val new_var_assignment : VariableName.t -> string -> string

val multiple_var_assignment : string list -> string -> string

val new_chan_var_assignment : VariableName.t * string -> string

val panic_with_msg : string -> string

val join_non_empty_lines : ?sep:string -> string list -> string

val incr_indent : string -> string

val indent_line : string -> string -> string

type goExpr =
  | GoRecv of goExpr
  | GoVar of VariableName.t
  | GoAssert of goExpr * VariableName.t
  | GoCall of FunctionName.t * goExpr list
  | GoMCall of VariableName.t * FunctionName.t * goExpr list
  | GoTypeOf of goExpr

and goStmt =
  | GoAssign of VariableName.t * goExpr
  | GoSend of goExpr * goExpr
  | GoSeq of goStmt list
  | GoExpr of goExpr
  | GoReturn of goExpr
  | GoSwitch of goStmt * (goExpr * goStmt) list
  | GoLabel of LabelName.t
  | GoFor of goStmt
  | GoContinue of LabelName.t option
  | GoSpawn of goExpr

and goType =
  | GoTyVar of VariableName.t
  | GoChan of goType
  | GoPtr of goType
  | GoFunTy of (VariableName.t list * goType) list * goType

and goTyDecl = GoSyn of goType | GoStruct of (VariableName.t * goType) list

and goDecl =
  | GoFunc of
      FunctionName.t
      * (VariableName.t list * goType) list
      * goType option
      * goStmt
  | GoTyDecl of string * goTyDecl

val ppr_prog : goDecl list -> string
