open! Base
open Gtype
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
      let new_name = name ^ "_" ^ Int.to_string uid in
      match Map.find uids new_name with
      | None ->
          let uids = Map.add_exn uids ~key:new_name ~data:1 in
          (Map.update uids name ~f:(fun _ -> uid), new_name)
      | Some _ ->
          make_unique_id (Map.update uids name ~f:(fun _ -> uid)) name )

let msg_field_name field = sprintf "F_%s" (VariableName.user field)

let msg_type_name msg = sprintf "Msg_%s" (LabelName.user msg)

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
      | Some payload' ->
          if List.equal equal_pvalue_payload payload payload' then
            extract_message_types msgs g
          else
            raise
              (Err.Violation
                 "Within a protocol, messages with the same label should  \
                  have the same payloads") )
  in
  (* Ensure there are no duplicate field names *)
  let process_msg_payloads ~key ~data msgs =
    let get_payload_field_name field_uids payload =
      match payload with
      | PValue (Some name, _) ->
          let name_str = VariableName.user name in
          if Map.mem field_uids name_str then
            Err.uerr (Err.DuplicatePayloadField (key, name)) ;
          Map.add_exn field_uids ~key:name_str ~data:1
      | PValue _ -> field_uids
      | PDelegate _ -> raise (Err.Violation "Delegation is not supported")
    in
    (* Generate default field names for unnamed fields *)
    (* FIXME: Currently assumes type is valid identifier, so types like maps
       or channels won't work for unnamed fields *)
    let generate_payload_field_name uids payload =
      match payload with
      | PValue (Some _, _) -> (uids, payload)
      | PValue (None, payload_type) ->
          let field_name = PayloadTypeName.user payload_type in
          let uids, field_name = make_unique_id uids field_name in
          ( uids
          , PValue (Some (VariableName.of_string field_name), payload_type)
          )
      | PDelegate _ -> raise (Err.Violation "Delegation is not supported")
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

let gen_protocol_msgs gtype =
  let gen_msg_struct (label, msg_payload) =
    (* FIXME: Currently assumes that the given type is a go type *)
    let gen_field_declaration payload =
      match payload with
      | PValue (field_name, field_type) ->
          let field = Option.value_exn field_name in
          sprintf "\t%s %s" (msg_field_name field)
            (PayloadTypeName.user field_type)
      | PDelegate _ -> failwith "PDelegate should never reach here"
    in
    let field_decls = List.map ~f:gen_field_declaration msg_payload in
    let field_decls_str = String.concat ~sep:"\n" field_decls in
    sprintf "type %s struct {\n%s\n}" (msg_type_name label) field_decls_str
  in
  let msg_types = get_msg_types gtype in
  let msg_structs = List.map ~f:gen_msg_struct msg_types in
  (* Stdio.print_endline (String.concat ~sep:"\n\n" msg_structs) *)
  msg_structs

let gen_msgs (global_t : Gtype.global_t) =
  Map.fold
    ~init:(Map.empty (module ProtocolName))
    ~f:(fun ~key ~data msgs ->
      let _, _, gtype = data in
      Map.add_exn msgs ~key ~data:(gen_protocol_msgs gtype))
    global_t

(* Map.iteri global_t ~f:(fun ~key ~data -> Stdio.print_endline (sprintf
   "Protocol %s messages:\n" (ProtocolName.user key)) ; let _, _, gtype =
   data in gen_protocol_msgs gtype) *)
