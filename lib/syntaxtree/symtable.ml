open! Base
open Syntax
open Loc
open Err
open Names

(** Record containing a protocol's signature *)
type protocol_decl =
  { proto_name: ProtocolName.t
  ; all_roles: RoleName.t list
  ; split_decl: RoleName.t list * RoleName.t list
  ; loc: Loc.t }

let show_roles split_decl =
  let list_to_str l =
    String.concat ~sep:", " (List.map ~f:RoleName.user l)
  in
  let roles, new_roles = split_decl in
  let roles_str = list_to_str roles in
  let new_roles_str =
    match new_roles with [] -> "" | _ -> "; new " ^ list_to_str new_roles
  in
  roles_str ^ new_roles_str

(* let show_protocol_decl (proto_decl : protocol_decl) = String.concat
   ~sep:"" [ ProtocolName.user proto_decl.proto_name ; "(" ; show_roles
   proto_decl.split_decl ; ")" ] *)

(** Symbol table type *)
type t =
  { (* protocol: ProtocolName.t *)
    table: protocol_decl Map.M(ProtocolName).t
        (* [@printer fun fmt tbl -> let nested_protos = String.concat ~sep:",
           " (List.map ~f:show_protocol_decl (Map.data tbl)) in fprintf fmt
           "{%s}" nested_protos] *)
  ; parent: t option }

(** Generate nested protocol names based on the name of the parent protocol*)
let name_with_prefix prefix proto_name =
  match ProtocolName.user prefix with
  | "" -> proto_name
  | _ ->
      ProtocolName.rename proto_name
        (ProtocolName.user prefix ^ "_" ^ ProtocolName.user proto_name)

(** Generate a protocol's declaration *)
let decl_from_protocol prefix (protocol : global_protocol) =
  let extract_rolenames roles =
    let num_roles = List.length roles in
    if Set.length (Set.of_list (module RoleName) roles) <> num_roles then
      uerr (DuplicateRoleParams protocol.value.name) ;
    roles
  in
  let proto_name = name_with_prefix prefix protocol.value.name in
  let all_roles = extract_rolenames protocol.value.roles in
  let rs, new_rs = protocol.value.split_roles in
  let roles = extract_rolenames rs in
  let dynamic_roles = extract_rolenames new_rs in
  let split_decl = (roles, dynamic_roles) in
  let loc = protocol.loc in
  {proto_name; all_roles; split_decl; loc}

(** Build symbol table for a given protocol *)
let create (prefix : ProtocolName.t) (protocols : global_protocol list)
    (parent : t option) =
  let add_proto_decl acc (protocol : global_protocol) =
    let decl = decl_from_protocol prefix protocol in
    let name = protocol.value.name in
    match Map.add acc ~key:name ~data:decl with
    | `Ok new_acc -> new_acc
    | `Duplicate ->
        let proto_sig = Map.find_exn acc name in
        uerr
          (RedefinedProtocol
             (protocol.value.name, protocol.loc, proto_sig.loc) )
  in
  let table =
    List.fold
      ~init:(Map.empty (module ProtocolName))
      ~f:add_proto_decl protocols
  in
  {(* protocol= prefix; *) table; parent}

(** Recursively look up a protocol in the symbol table hierarchy *)
let rec lookup_protocol (symbol_table : t) protocol =
  match Map.find symbol_table.table protocol with
  | Some decl -> decl
  | None -> (
    match symbol_table.parent with
    | Some parent_table -> lookup_protocol parent_table protocol
    | None -> uerr (UnboundProtocol protocol) )
