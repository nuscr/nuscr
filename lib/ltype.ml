open! Base
open Printf
open Gtype
open Err
open Names

type t =
  | RecvL of Gtype.message * RoleName.t * t
  | SendL of Gtype.message * RoleName.t * t
  | ChoiceL of RoleName.t * t list
  | TVarL of TypeVariableName.t
  | MuL of TypeVariableName.t * t
  | EndL
  | InviteCreateL of RoleName.t list * RoleName.t list * ProtocolName.t * t
  | AcceptL of
      RoleName.t
      * RoleName.t
      * ProtocolName.t
      * RoleName.t list
      * RoleName.t list
      * t
[@@deriving sexp_of, eq]

let roles_to_string roles =
  let str_roles = List.map ~f:RoleName.user roles in
  String.concat ~sep:", " str_roles

let show =
  let indent_here indent = String.make (indent * 2) ' ' in
  let rec show_local_type_internal indent =
    let current_indent = indent_here indent in
    function
    | RecvL (m, r, l) ->
        sprintf "%s%s from %s;\n%s" current_indent (show_message m)
          (RoleName.user r)
          (show_local_type_internal indent l)
    | SendL (m, r, l) ->
        sprintf "%s%s to %s;\n%s" current_indent (show_message m)
          (RoleName.user r)
          (show_local_type_internal indent l)
    | MuL (n, l) ->
        sprintf "%srec %s {\n%s%s}\n" current_indent
          (TypeVariableName.user n)
          (show_local_type_internal (indent + 1) l)
          current_indent
    | TVarL n ->
        sprintf "%scontinue %s;\n" current_indent (TypeVariableName.user n)
    | EndL -> sprintf "%send\n" current_indent
    | ChoiceL (r, ls) ->
        let pre =
          sprintf "%schoice at %s {\n" current_indent (RoleName.user r)
        in
        let intermission = sprintf "%s} or {\n" current_indent in
        let post = sprintf "%s}\n" current_indent in
        let choices =
          List.map ~f:(show_local_type_internal (indent + 1)) ls
        in
        let ls = String.concat ~sep:intermission choices in
        pre ^ ls ^ post
    | InviteCreateL (invite_roles, create_roles, protocol, l) ->
        let name_str = ProtocolName.user protocol in
        let invite =
          sprintf "%sinvite(%s) to %s;\n" current_indent
            (roles_to_string invite_roles)
            name_str
        in
        let create =
          if List.length create_roles = 0 then ""
          else
            sprintf "%screate(%s) to %s;\n" current_indent
              (roles_to_string create_roles)
              name_str
        in
        let l_str = show_local_type_internal indent l in
        invite ^ create ^ l_str
    | AcceptL (role, caller, protocol, roles, new_roles, l) ->
        let roles_str = List.map ~f:RoleName.user roles in
        let new_roles_str = List.map ~f:RoleName.user new_roles in
        sprintf "%saccept %s from %s in %s(%s);\n%s" current_indent
          (RoleName.user role) (RoleName.user caller)
          (ProtocolName.user protocol)
          (Symtable.show_roles (roles_str, new_roles_str))
          (show_local_type_internal (indent + 1) l)
  in
  show_local_type_internal 0

exception Unmergable of t * t [@@deriving sexp_of]

let rec merge projected_role lty1 lty2 =
  try
    let fail () = raise (Unmergable (lty1, lty2)) in
    let merge_recv r recvs =
      let rec aux (acc : (LabelName.t * t) list) = function
        | RecvL (m, _, lty) as l -> (
            let {label; _} = m in
            match List.Assoc.find acc ~equal:LabelName.equal label with
            | None -> (label, l) :: acc
            | Some (RecvL (m_, r, l_))
              when List.equal equal_payload m.Gtype.payload m_.Gtype.payload
              ->
                List.Assoc.add acc ~equal:LabelName.equal label
                  (RecvL (m, r, merge projected_role lty l_))
            | Some (RecvL _) -> fail ()
            | _ ->
                raise
                  (Violation
                     "Merge receive must be merging receive local types") )
        | _ ->
            raise
              (Violation "Merge receive must be merging receive local types")
      in
      let conts = List.fold ~f:aux ~init:[] recvs in
      match conts with
      | [] -> EndL
      | [(_, lty)] -> lty
      | conts -> ChoiceL (r, List.map ~f:snd conts)
    in
    match (lty1, lty2) with
    | RecvL (_, r1, _), RecvL (_, r2, _) ->
        if not @@ RoleName.equal r1 r2 then fail () ;
        merge_recv r1 [lty1; lty2]
    | ChoiceL (r1, ltys1), RecvL (_, r2, _) when RoleName.equal r1 r2 ->
        (* Choice is a set of receive *)
        merge_recv r1 (lty2 :: ltys1)
    | RecvL (_, r2, _), ChoiceL (r1, ltys2) when RoleName.equal r1 r2 ->
        merge_recv r1 (lty1 :: ltys2)
    | ChoiceL (r1, ltys1), ChoiceL (r2, ltys2)
      when RoleName.equal r1 r2 && not (RoleName.equal r1 projected_role) ->
        merge_recv r1 (ltys1 @ ltys2)
    | _ -> if equal lty1 lty2 then lty1 else fail ()
  with Unmergable (l1, l2) ->
    let error = show l1 ^ " " ^ show l2 in
    uerr @@ Err.UnableToMerge error

(* Check whether the first message in a g choice is from choice_r to recv_r,
   if recv_r is Some; return receive role *)
let check_consistent_gchoice choice_r recv_r = function
  (* TODO: add Invitation *)
  | MessageG (_, send_r, recv_r_, _) -> (
      if not @@ RoleName.equal send_r choice_r then
        uerr (RoleMismatch (choice_r, send_r)) ;
      match recv_r with
      | None -> Some recv_r_
      | Some recv_r ->
          if not @@ RoleName.equal recv_r recv_r_ then
            uerr (RoleMismatch (recv_r, recv_r_)) ;
          Some recv_r )
  | _ ->
      raise
        (Violation
           "Normalised global type always has a message in choice branches")

let rec project (projected_role : RoleName.t) = function
  | EndG -> EndL
  | TVarG name -> TVarL name
  | MuG (name, g_type) -> (
    match project projected_role g_type with
    | TVarL _ | EndL -> EndL
    | lType -> MuL (name, lType) )
  | MessageG (m, send_r, recv_r, g_type) -> (
      let next = project projected_role g_type in
      match projected_role with
      | _ when RoleName.equal projected_role send_r -> SendL (m, recv_r, next)
      | _ when RoleName.equal projected_role recv_r -> RecvL (m, send_r, next)
      | _ -> next )
  | ChoiceG (choice_r, g_types) -> (
      (* TODO Add label for invitation *)
      let check_distinct_prefix gtys =
        let rec aux acc = function
          | [] -> ()
          | MessageG (m, _, _, _) :: rest ->
              let l = m.label in
              if Set.mem acc l (* FIXME: Use 2 labels for location *) then
                uerr (DuplicateLabel l)
              else aux (Set.add acc l) rest
          | _ -> (* FIXME: raise Violation *) assert false
        in
        aux (Set.empty (module LabelName)) gtys
      in
      check_distinct_prefix g_types ;
      let recv_r =
        List.fold ~f:(check_consistent_gchoice choice_r) ~init:None g_types
      in
      let recv_r = Option.value_exn recv_r in
      let l_types = List.map ~f:(project projected_role) g_types in
      let l_types =
        List.filter ~f:(function EndL -> false | _ -> true) l_types
      in
      match projected_role with
      | _
        when RoleName.equal projected_role choice_r
             || RoleName.equal projected_role recv_r ->
          ChoiceL (choice_r, l_types)
      | _ -> (
        match List.reduce ~f:(merge projected_role) l_types with
        | Some l -> l
        | None -> EndL ) )
  | NestedG _ -> unimpl "TODO: implement nested protocols in local types"
  | CallG _ -> unimpl "TODO: implement calls in local types"

module type S = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  include Comparable.S with type t := t
end

module LocalProtocolId = struct
  module T = struct
    type t = ProtocolName.t * RoleName.t
    [@@deriving show {with_path= false}, sexp_of]

    let compare (p1, r1) (p2, r2) =
      let cmp_protocol_name = ProtocolName.compare p1 p2 in
      if cmp_protocol_name <> 0 then cmp_protocol_name
      else RoleName.compare r1 r2
  end

  include T
  include Comparable.Make (T)
end

let build_local_proto_lookup_table (global_t : global_t) =
  let rec unique_name key uids =
    let uid = match Map.find uids key with Some n -> n | None -> 0 in
    let protocol_name =
      if uid = 0 then key else key ^ "_" ^ Int.to_string (uid + 1)
    in
    let new_uids = Map.update uids key ~f:(fun _ -> uid + 1) in
    if uid = 0 then (new_uids, protocol_name)
    else if Map.mem new_uids protocol_name then unique_name key new_uids
    else (Map.add_exn new_uids ~key:protocol_name ~data:1, protocol_name)
  in
  let local_protocol_unique_names proto_name roles new_roles uids
      all_local_protocols =
    let add_local_proto_name (local_proto_names, uids) role =
      let proto_and_role =
        ProtocolName.user proto_name ^ "_" ^ RoleName.user role
      in
      let new_uids, local_proto_name = unique_name proto_and_role uids in
      ( Map.add_exn local_proto_names ~key:(proto_name, role)
          ~data:local_proto_name
      , new_uids )
    in
    let all_local_protocols, uids =
      List.fold
        ~init:(all_local_protocols, uids)
        ~f:add_local_proto_name (roles @ new_roles)
    in
    (uids, all_local_protocols)
  in
  let rec proto_to_table g uids acc =
    match g with
    | NestedG (proto, roles, new_roles, g_, g_') ->
        let uids, acc =
          local_protocol_unique_names proto roles new_roles uids acc
        in
        let uids, acc = proto_to_table g_ uids acc in
        proto_to_table g_' uids acc
    | EndG | TVarG _ | MuG _ | MessageG _ | ChoiceG _ | CallG _ -> (uids, acc)
  in
  let _, all_local_protocols =
    Map.fold global_t
      ~init:(Map.empty (module String), Map.empty (module LocalProtocolId))
      ~f:(fun ~key ~data acc ->
        let uids, all_names = acc in
        let (roles, new_roles), g = data in
        let uids, all_names =
          local_protocol_unique_names key roles new_roles uids all_names
        in
        proto_to_table g uids all_names)
  in
  all_local_protocols

let show_lookup_table table =
  let show_key (protocol, role) =
    sprintf "%s@%s" (ProtocolName.user protocol) (RoleName.user role)
  in
  let show_aux ~key ~data acc =
    sprintf "%s%s: %s\n" acc (show_key key) data
  in
  Map.fold table ~init:"" ~f:show_aux
