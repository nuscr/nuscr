open! Base
open Syntax
open Loc
open Err

(** Record containing a protocol's signature *)
type protocol_decl =
  { proto_name: string
  ; all_roles: string list
  ; split_decl: string list * string list
  ; loc: source_loc }

let show_roles split_decl =
  let list_to_str l = String.concat ~sep:", " l in
  let roles, new_roles = split_decl in
  let roles_str = list_to_str roles in
  let new_roles_str =
    match new_roles with [] -> "" | _ -> "; new " ^ list_to_str new_roles
  in
  roles_str ^ new_roles_str

let show_protocol_decl (proto_decl : protocol_decl) =
  String.concat ~sep:""
    [proto_decl.proto_name; "("; show_roles proto_decl.split_decl; ")"]

(** Symbol table type *)
type symbol_table =
  { protocol: string
  ; table: protocol_decl Map.M(String).t
        [@printer
          fun fmt tbl ->
            let nested_protos =
              String.concat ~sep:", "
                (List.map ~f:show_protocol_decl (Map.data tbl))
            in
            fprintf fmt "{%s}" nested_protos]
  ; parent: symbol_table option }
[@@deriving show]

(** Generate nested protocol names based on the name of the parent protocol*)
let name_with_prefix prefix proto_name =
  match prefix with "" -> proto_name | _ -> prefix ^ "_" ^ proto_name

(** Generate a protocol's declaration *)
let decl_from_protocol prefix (protocol : global_protocol) =
  let extract_rolenames roles =
    let num_roles = List.length roles in
    let str_roles = List.map ~f:N.user roles in
    if Set.length (Set.of_list (module String) str_roles) <> num_roles then
      uerr
        (DuplicateRoleParams (Names.ProtocolName.of_name protocol.value.name)) ;
    str_roles
  in
  let proto_name = name_with_prefix prefix (N.user protocol.value.name) in
  let all_roles = extract_rolenames protocol.value.roles in
  let rs, new_rs = protocol.value.split_roles in
  let roles = extract_rolenames rs in
  let dynamic_roles = extract_rolenames new_rs in
  let split_decl = (roles, dynamic_roles) in
  let loc = protocol.loc in
  {proto_name; all_roles; split_decl; loc}

(** Build symbol table for a given protocol *)
let build_symbol_table (prefix : string) (protocols : global_protocol list)
    (parent : symbol_table option) =
  let add_proto_decl acc (protocol : global_protocol) =
    let decl = decl_from_protocol prefix protocol in
    let name = N.user protocol.value.name in
    match Map.add acc ~key:name ~data:decl with
    | `Ok new_acc -> new_acc
    | `Duplicate ->
        let proto_sig = Map.find_exn acc name in
        uerr
          (RedefinedProtocol
             ( Names.ProtocolName.of_name protocol.value.name
             , protocol.loc
             , proto_sig.loc ) )
  in
  let table =
    List.fold ~init:(Map.empty (module String)) ~f:add_proto_decl protocols
  in
  {protocol= prefix; table; parent}

(** Recursively look up a protocol in the symbol table hierarchy *)
let rec lookup_protocol (symbol_table : symbol_table) protocol =
  let proto_name = N.user protocol in
  match Map.find symbol_table.table proto_name with
  | Some decl -> decl
  | None -> (
    match symbol_table.parent with
    | Some parent_table -> lookup_protocol parent_table protocol
    | None -> uerr (UnboundProtocol (Names.ProtocolName.of_name protocol)) )
