open Names
open! Base
open Ltype
open Printf
open Gtype

module UniqueNameGen : sig
  type t

  val create : unit -> t

  val unique_name : t -> string -> t * string
end = struct
  type t = (string, int, String.comparator_witness) Map.t

  let create () = Map.empty (module String)

  let rec unique_name uids name =
    match Map.find uids name with
    | None -> (Map.add_exn uids ~key:name ~data:1, name)
    | Some curr_id -> (
        let uid = curr_id + 1 in
        let new_name = sprintf "%s_%d" name uid in
        match Map.find uids new_name with
        | None ->
            let uids = Map.add_exn uids ~key:new_name ~data:1 in
            (Map.update uids name ~f:(fun _ -> uid), new_name)
        | Some _ -> unique_name (Map.update uids name ~f:(fun _ -> uid)) name
        )
end

(* module type S = sig type t [@@deriving show {with_path= false}, sexp_of]

   val new_channel : t -> RoleName.t -> LabelName.t -> t * string

   val new_send_invite_chan : t -> RoleName.t -> string -> ProtocolName.t ->
   t * string

   val new_recv_invite_chan : t -> RoleName.t -> string -> ProtocolName.t ->
   t * string

   val new_send_callback : t -> LabelName.t -> RoleName.t -> t * string

   val new_recv_callback : t -> LabelName.t -> RoleName.t -> t * string

   (* Global protocol name *)

   val new_protocol_setup_callback : t -> ProtocolName.t -> t * string

   (* Local protocol name *)

   val new_create_protocol_env_callback : t -> ProtocolName.t -> t * string

   val new_protocol_result_callback : t -> ProtocolName.t -> t * string

   val gen_channel_struct : t -> string

   val gen_invite_channel_struct : t -> string

   val gen_setup_channel_struct : t -> string

   val gen_setup_invite_channel_struct : t -> string

   (* Generates whole file, including imports *)

   val gen_callbacks : t -> string

   val gen_result_struct : t -> string

   val create : RoleName.t -> bool -> t end *)

let msg_type_name msg = String.capitalize (LabelName.user msg)

(* let lowercase_role_name role = String.lowercase @@ RoleName.user role *)

let capitalize_role_name role = String.capitalize @@ RoleName.user role

let chan_struct_field_name role msg_type =
  sprintf "%s_%s" (capitalize_role_name role) (msg_type_name msg_type)

let chan_struct_name role_name =
  sprintf "%s_Chan" (capitalize_role_name role_name)

(* let send_invite_chan_name role_name local_protocol = sprintf
   "Invite_%s_To_%s" (capitalize_role_name role_name) local_protocol *)

(* let recv_invite_chan_name role_name local_protocol = sprintf
   "%s_Invite_To_%s" (capitalize_role_name role_name) local_protocol *)

(* let protocol_pkg_name protocol = String.lowercase @@ ProtocolName.user
   protocol *)

let struct_decl struct_name field_decls =
  let field_decls = List.sort field_decls ~compare:String.compare in
  let field_decls_str = String.concat ~sep:"\n" field_decls in
  sprintf "type %s struct {\n%s\n}" struct_name field_decls_str

(* Callbacks, channel struct, invite channel struct, result struct +
   implementation *)

module ImportsEnv : sig
  type t

  val import_messages : t -> ProtocolName.t -> t * string

  val import_channels : t -> ProtocolName.t -> t * string

  val import_result : t -> ProtocolName.t -> t * string

  (* These last 3 might be overkill *)
  val import_invitations : t -> t * string

  val import_callbacks : t -> t * string

  val import_roles : t -> t * string

  val generate_imports : t -> string
end = struct
  type aliases = (string, string, String.comparator_witness) Map.t

  type import_aliases =
    { messages: aliases
    ; channels: aliases
    ; result: aliases
    ; invitations: aliases
    ; callbacks: aliases
    ; roles: aliases }

  type t = UniqueNameGen.t * ProtocolName.t * import_aliases

  let import_messages env _ = (env, "")

  let import_channels env _ = (env, "")

  let import_invitations env = (env, "")

  let import_callbacks env = (env, "")

  let import_result env _ = (env, "")

  let import_roles env = (env, "")

  let generate_imports (_ : t) = ""
end

module ChannelEnv : sig
  type t

  val new_channel : t -> RoleName.t -> LabelName.t -> t * string

  val gen_channel_struct : t -> RoleName.t -> string

  val create : ProtocolName.t -> t
end = struct
  type channel_fields = (string * LabelName.t) list

  type t = UniqueNameGen.t * ProtocolName.t * channel_fields

  let new_channel (name_gen, protocol, channel_fields) role msg_label =
    let channel_name = chan_struct_field_name role msg_label in
    let name_gen, channel_name =
      UniqueNameGen.unique_name name_gen channel_name
    in
    let env =
      (name_gen, protocol, (channel_name, msg_label) :: channel_fields)
    in
    (env, channel_name)

  let gen_channel_struct ((_, protocol, channel_fields) : t) role =
    let gen_chan_field_decl (chan_name, msg_label) =
      sprintf "\t%s chan %s.%s" chan_name
        (ProtocolName.user protocol)
        (msg_type_name msg_label)
    in
    let chan_decls = List.map ~f:gen_chan_field_decl channel_fields in
    struct_decl (chan_struct_name role) chan_decls

  let create protocol = (UniqueNameGen.create (), protocol, [])
end

(* module InviteEnv : sig type t

   val new_send_invite_channel : t -> RoleName.t -> ProtocolName.t ->
   RoleName.t -> ProtocolName.t -> t * string

   val new_recv_invite_channel : t -> RoleName.t -> string -> RoleName.t ->
   ProtocolName.t -> t * string

   val gen_channel_struct : t -> string

   val create : string -> t end = struct type invite_channel_fields = (string
   * string * ProtocolName.t * RoleName.t) list

   type t = UniqueNameGen.t * string * invite_channel_fields

   (* let new_send_invite_chan env role local_protocol protocol_name = *)

   let new_send_invite_channel (name_gen, local_proto, channel_fields)
   participant local_protocol role protocol = let channel_name =
   send_invite_chan_name participant local_protocol in let name_gen,
   channel_name = UniqueNameGen.unique_name name_gen channel_name in let env
   = ( name_gen , local_protocol , (channel_name, local_proto, protocol,
   role) :: channel_fields ) in (env, channel_name)

   (* let new_recv_invite_chan env role local_protocol protocol_name = *)

   let new_recv_invite_channel (name_gen, local_proto, channel_fields) caller
   local_protocol role protocol = let channel_name = recv_invite_chan_name
   caller local_protocol in let name_gen, channel_name =
   UniqueNameGen.unique_name name_gen channel_name in let env = ( name_gen ,
   local_proto , (channel_name, local_protocol, protocol, role) ::
   channel_fields ) in (env, channel_name)

   (* let gen_channel_struct (_, local_protocol, channel_fields) = *) let
   gen_channel_struct (_, local_protocol, channel_fields) = let
   gen_chan_field_decl (chan_name, local_proto, protocol, role) = let
   role_chan_decl = sprintf "\t%s chan %s.%s" chan_name (protocol_pkg_name
   protocol) (chan_struct_name role) in let invite_chan_decl = sprintf "\t%s
   chan %s" () in ""

   let create local_protocol = (UniqueNameGen.create (), local_protocol, [])
   end *)

module CallbacksEnv : sig
  type t

  val new_send_callback : t -> LabelName.t -> RoleName.t -> t * string

  val new_recv_callback : t -> LabelName.t -> RoleName.t -> t * string

  (* Global protocol name *)

  val new_protocol_setup_callback : t -> ProtocolName.t -> t * string

  (* Local protocol name *)

  val new_create_protocol_env_callback :
    t -> string -> ProtocolName.t -> t * string

  val new_protocol_result_callback :
    t -> string -> ProtocolName.t -> RoleName.t -> t * string
end = struct
  type t = int

  let new_send_callback env _ _ = (env, "")

  let new_recv_callback env _ _ = (env, "")

  let new_protocol_setup_callback env _ = (env, "")

  let new_create_protocol_env_callback env _ _ = (env, "")

  let new_protocol_result_callback env _ _ _ = (env, "")
end

(* module GoCodeGenEnv = struct (* chan name, msg label *)

   type channel_fields = (string * LabelName.t) list

   (* chan name, local_protocol, global_protocol, role in protocol*)

   type invite_channel_fields = (string * ProtocolName.t * ProtocolName.t *
   RoleName.t) list

   type callback_param = ProtocolName.t * LabelName.t

   type callback_return = ProtocolName.t * LabelName.t

   type callback_sigs = string * callback_param list * callback_return option

   type callback_imports = { messages_import: bool ; result_imports: (string,
   String.comparator_witness) Set.t }

   type t = { role: RoleName.t ; protocol: ProtocolName.t ; channel_env:
   UniqueNameGen.t * channel_fields ; invite_env: UniqueNameGen.t *
   invite_channel_fields ; callbacks_env: UniqueNameGen.t * callback_sigs ;
   callback_imports_env: UniqueNameGen.t * callback_imports }

   let new_channel env role msg_label = let name_gen, channel_fields =
   env.channel_env in let channel_name = chan_struct_field_name role
   msg_label in let name_gen, channel_name = UniqueNameGen.unique_name
   name_gen channel_name in let env = { env with channel_env= (name_gen,
   (channel_name, msg_label) :: channel_fields) } in (env, channel_name)

   (* let new_send_invite_chan env role local_protocol protocol_name = *)

   let new_send_invite_chan env _ _ _ = (env, "")

   (* let new_recv_invite_chan env role local_protocol protocol_name = *)

   let new_recv_invite_chan env _ _ _ = (env, "")

   let new_send_callback env _ _ = (env, "")

   let new_recv_callback env _ _ = (env, "") end *)

(* LTYPE CODEGEN *)
module LTypeCodeGenEnv : sig
  type t

  val create : ProtocolName.t -> RoleName.t -> t

  val new_channel : t -> RoleName.t -> LabelName.t -> t * string

  val gen_channel_struct : t -> RoleName.t -> string
end = struct
  type t =
    {protocol: ProtocolName.t; role: RoleName.t; channel_env: ChannelEnv.t}

  let create protocol role =
    let channel_env = ChannelEnv.create protocol in
    {protocol; role; channel_env}

  let new_channel ({channel_env; _} as t) role msg_label =
    let channel_env, channel_name =
      ChannelEnv.new_channel channel_env role msg_label
    in
    ({t with channel_env}, channel_name)

  let gen_channel_struct {channel_env; _} role =
    ChannelEnv.gen_channel_struct channel_env role
end

(* gen_role_implementation protocol role local_protocol ltype -> env * string*)
(* gen_impl env ltype indent -> env string *)
(* TODO: Are recursion labels unique? *)
(* TODO code gen of ltype *)
let gen_role_implementation protocol role local_proto_name ltype =
  let incr_indent curr_indent = sprintf "\t%s" curr_indent in
  let indent_line line indent = sprintf "%s%s" indent line in
  let rec gen_implementation indent env ltype =
    match ltype with
    | EndL -> (env, indent_line "return env.Done()" indent)
    | TVarL var ->
        ( env
        , indent_line
            (sprintf "continue %s" (TypeVariableName.user var))
            indent )
    | MuL (_, ltype') ->
        let env, impl = gen_implementation (incr_indent indent) env ltype' in
        (env, impl)
    | InviteCreateL (_, _, _, ltype') -> gen_implementation indent env ltype'
    | AcceptL (_, _, _, _, _, ltype') -> gen_implementation indent env ltype'
    | ChoiceL (_, ltys) ->
        let env, impls =
          List.fold_map ~init:env
            ~f:(gen_implementation (incr_indent indent))
            ltys
        in
        (env, String.concat ~sep:"\n\n" impls)
    | RecvL ({label; _}, r, ltype') | SendL ({label; _}, r, ltype') ->
        (* TODO: split cases *)
        let env, channel_name = LTypeCodeGenEnv.new_channel env r label in
        let env, impl = gen_implementation indent env ltype' in
        (env, sprintf "%s\n%s" (indent_line channel_name indent) impl)
  in
  let env = LTypeCodeGenEnv.create protocol role in
  let env, impl = gen_implementation (incr_indent "") env ltype in
  let impl = sprintf "fun %s() {\n%s\n}" local_proto_name impl in
  (env, impl)

let gen_code (global_t : global_t) (local_t : local_t) =
  (* TODO: Move build_local_proto_name_lookup to codegen *)
  let protocol_lookup = build_local_proto_name_lookup local_t in
  let gen_protocol_role_implementation ~key ~data chan_structs =
    let (roles, new_roles), _, _ = data in
    let protocol = key in
    let role_chan_structs =
      List.map
        ~f:(fun role ->
          let local_protocol_id = LocalProtocolId.create protocol role in
          let _, ltype = Map.find_exn local_t local_protocol_id in
          let local_protocol_name =
            Map.find_exn protocol_lookup local_protocol_id
          in
          let local_protocol_name_str =
            ProtocolName.user local_protocol_name
          in
          let env, _ =
            gen_role_implementation protocol role local_protocol_name_str
              ltype
          in
          LTypeCodeGenEnv.gen_channel_struct env role)
        (roles @ new_roles)
    in
    let chan_structs_str = String.concat ~sep:"\n\n" role_chan_structs in
    Map.add_exn chan_structs ~key:protocol ~data:chan_structs_str
  in
  Map.fold
    ~init:(Map.empty (module ProtocolName))
    ~f:gen_protocol_role_implementation global_t
