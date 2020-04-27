open! Base
open Gtype
open Ltype
open Printf
open Names

(* TODO: Remove *)
(* let show_uids uids = Stdio.print_endline "Uids" ; Map.iteri uids ~f:(fun
   ~key ~data -> Stdio.print_endline (sprintf "%s: %d" key data)) *)

let rec make_unique_id uids name =
  match Map.find uids name with
  | None -> (Map.add_exn uids ~key:name ~data:1, name)
  | Some curr_id -> (
      let uid = curr_id + 1 in
      let new_name = sprintf "%s_%d" name uid in
      match Map.find uids new_name with
      | None ->
          let uids = Map.add_exn uids ~key:new_name ~data:1 in
          (Map.update uids name ~f:(fun _ -> uid), new_name)
      | Some _ ->
          make_unique_id (Map.update uids name ~f:(fun _ -> uid)) name )

(* PKG MESSAGES *)
let msg_field_name field = String.capitalize (VariableName.user field)

let msg_field_name_from_type t = String.capitalize @@ PayloadTypeName.user t

let msg_type_name msg = String.capitalize (LabelName.user msg)

(* --------- *)
let protocol_pkg_name protocol =
  String.lowercase @@ ProtocolName.user protocol

let lowercase_role_name role = String.lowercase @@ RoleName.user role

let capitalize_role_name role = String.capitalize @@ RoleName.user role

(* --------- *)

(* PKG CHANNELS *)
let chan_struct_field_name role msg_type =
  sprintf "%s_%s" (capitalize_role_name role) (msg_type_name msg_type)

let build_chan_name chan_uids role msg_type =
  let chan_name = chan_struct_field_name role msg_type in
  make_unique_id chan_uids chan_name

let chan_struct_name role_name =
  sprintf "%sChan" (capitalize_role_name role_name)

(* let pkg_access pkg_name field = sprintf "%s.%s" pkg_name field

   let chan_field_decl field_name chan_type = sprintf "\t%s chan %s"
   field_name chan_type *)

let struct_decl struct_name field_decls =
  let field_decls = List.sort field_decls ~compare:String.compare in
  let field_decls_str = String.concat ~sep:"\n" field_decls in
  sprintf "type %s struct {\n%s\n}" struct_name field_decls_str

let struct_field_decl field_name field_type =
  sprintf "\t%s %s" field_name field_type

(* let protocol_name protocol = sprintf "Protocol_%s" (ProtocolName.user
   protocol) *)

(* TODO: Channels, invitations, callbacks, ... *)

(* Extracts all the different messages which appear in a protocol, and
   processes them so they can be used to generate a Go struct *)
let get_msg_types gtype =
  (* Ensure labels within a protocol are used consistently with the same
     payload *)
  let rec extract_message_types msgs gtype =
    match gtype with
    | EndG | TVarG _ -> msgs
    | MuG (_, g) | CallG (_, _, _, g) -> extract_message_types msgs g
    | ChoiceG (_, gtypes) ->
        List.fold ~init:msgs ~f:extract_message_types gtypes
    | MessageG ({label; payload}, _, _, g) -> (
      match Map.find msgs label with
      | None ->
          extract_message_types (Map.add_exn msgs ~key:label ~data:payload) g
      | Some _ -> extract_message_types msgs g )
  in
  (* Assumes payloads have been validated for codegen *)
  let process_msg_payloads ~key ~data msgs =
    let get_payload_field_name field_uids payload =
      match payload with
      | PValue (Some name, _) ->
          let name_str = msg_field_name name in
          Map.add_exn field_uids ~key:name_str ~data:1
      | _ -> field_uids
    in
    (* Generate default field names for unnamed fields *)
    (* FIXME: Currently assumes type is valid identifier, so types like maps
       or channels won't work for unnamed fields *)
    let generate_payload_field_name uids payload =
      match payload with
      | PValue (None, payload_type) ->
          let field_name = msg_field_name_from_type payload_type in
          let uids, field_name = make_unique_id uids field_name in
          ( uids
          , PValue (Some (VariableName.of_string field_name), payload_type)
          )
      | PValue (Some name, payload_type) ->
          ( uids
          , PValue
              ( Some (VariableName.rename name (msg_field_name name))
              , payload_type ) )
      | _ -> failwith "PDelegate should not reach payload field renaming"
    in
    let field_uids =
      List.fold
        ~init:(Map.empty (module String))
        ~f:get_payload_field_name data
    in
    let _, payload =
      List.fold_map ~init:field_uids ~f:generate_payload_field_name data
    in
    (key, payload) :: msgs
  in
  let msgs = extract_message_types (Map.empty (module LabelName)) gtype in
  Map.fold ~init:[] ~f:process_msg_payloads msgs

let gen_msgs gtype =
  let gen_msg_struct (label, msg_payload) =
    (* FIXME: Currently assumes that the given type is a go type *)
    let gen_field_declaration payload =
      match payload with
      | PValue (field_name, field_type) ->
          let field = Option.value_exn field_name in
          struct_field_decl (VariableName.user field)
            (PayloadTypeName.user field_type)
      | PDelegate _ ->
          failwith "PDelegate should never reach struct field generation"
    in
    let field_decls = List.map ~f:gen_field_declaration msg_payload in
    struct_decl (msg_type_name label) field_decls
  in
  let msg_types = get_msg_types gtype in
  let msg_structs = List.map ~f:gen_msg_struct msg_types in
  let msg_structs_str = String.concat ~sep:"\n\n" msg_structs in
  msg_structs_str

let gen_protocol_msgs (global_t : Gtype.global_t) =
  Map.fold
    ~init:(Map.empty (module ProtocolName))
    ~f:(fun ~key ~data msgs ->
      let _, _, gtype = data in
      Map.add_exn msgs ~key ~data:(gen_msgs gtype))
    global_t

(* Gen code for a local protocol *)
(* - local protocol name *)
(* - Map<string -> uid> channel name uids (name = <role>_<msg_labell>) *)
(* - Map<(chan_name, msg_label)> *)
(* - ltype *)
(* let gen_channel_structs channel_uids channels = function | EndL | TVarL _
   -> channels | MuL _ *)
let gen_channel_struct protocol role ltype =
  let rec get_channels (channel_uids, channel_fields) ltype =
    match ltype with
    | EndL | TVarL _ -> (channel_uids, channel_fields)
    | MuL (_, ltype') -> get_channels (channel_uids, channel_fields) ltype'
    | InviteCreateL (_, _, _, ltype') ->
        get_channels (channel_uids, channel_fields) ltype'
    | AcceptL (_, _, _, _, _, ltype') ->
        get_channels (channel_uids, channel_fields) ltype'
    | ChoiceL (_, ltys) ->
        List.fold ~init:(channel_uids, channel_fields) ~f:get_channels ltys
    | RecvL ({label; _}, r, ltype') | SendL ({label; _}, r, ltype') ->
        let channel_uids, channel_name =
          build_chan_name channel_uids r label
        in
        get_channels
          (channel_uids, (channel_name, label) :: channel_fields)
          ltype'
  in
  let gen_chan_field_decl protocol (chan_name, msg_label) =
    sprintf "\t%s chan %s.%s" chan_name
      (ProtocolName.user protocol)
      (msg_type_name msg_label)
  in
  let _, channel_fields =
    get_channels (Map.empty (module String), []) ltype
  in
  (*  *)
  (* Stdio.print_endline (sprintf "%s %s" (ProtocolName.user protocol)
     (RoleName.user role)) ; List.iter channel_fields ~f:(fun (chan_name, _)
     -> Stdio.print_endline chan_name) ; *)
  (*  *)
  let chan_decls =
    List.map ~f:(gen_chan_field_decl protocol) channel_fields
  in
  struct_decl (chan_struct_name role) chan_decls

let gen_protocol_channels (global_t : global_t) (local_t : local_t) =
  let gen_protocol_role_channel_structs ~key ~data chan_structs =
    let (roles, new_roles), _, _ = data in
    let protocol = key in
    let role_chan_structs =
      List.map
        ~f:(fun role ->
          let _, ltype =
            Map.find_exn local_t (LocalProtocolId.create protocol role)
          in
          gen_channel_struct protocol role ltype)
        (roles @ new_roles)
    in
    let chan_structs_str = String.concat ~sep:"\n\n" role_chan_structs in
    Map.add_exn chan_structs ~key:protocol ~data:chan_structs_str
  in
  Map.fold
    ~init:(Map.empty (module ProtocolName))
    ~f:gen_protocol_role_channel_structs global_t

(* FIXME: This is probably too restrictive/checks are fully correct *)

(** For current code generation scheme:

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
  (* Ensure message labels are unique when lowercased *)
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
