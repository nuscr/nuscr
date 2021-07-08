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
      * ProtocolName.t
      * RoleName.t list
      * RoleName.t list
      * RoleName.t
      * t
  | SilentL of VariableName.t * Expr.payload_type * t
[@@deriving sexp_of, eq]

module type S = sig
  type t [@@deriving show {with_path= false}, sexp_of]

  val create : ProtocolName.t -> RoleName.t -> t

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

    let create protocol role = (protocol, role)
  end

  include T
  include Comparable.Make (T)
end

type local_t = (RoleName.t list * t) Map.M(LocalProtocolId).t

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
            let str_roles =
              List.map
                ~f:(fun r -> sprintf "role %s" (RoleName.user r))
                create_roles
            in
            sprintf "%screate(%s) in %s;\n" current_indent
              (String.concat ~sep:", " str_roles)
              name_str
        in
        let l_str = show_local_type_internal indent l in
        invite ^ create ^ l_str
    | AcceptL (role, protocol, roles, new_roles, caller, l) ->
        let roles_str = List.map ~f:RoleName.user roles in
        let new_roles_str = List.map ~f:RoleName.user new_roles in
        sprintf "%saccept %s@%s(%s) from %s;\n%s" current_indent
          (RoleName.user role)
          (ProtocolName.user protocol)
          (Symtable.show_roles (roles_str, new_roles_str))
          (RoleName.user caller)
          (show_local_type_internal indent l)
    | SilentL (var, ty, l) ->
        sprintf "%s(silent) %s(%s);\n%s" current_indent
          (VariableName.user var)
          (Expr.show_payload_type ty)
          (show_local_type_internal indent l)
  in
  show_local_type_internal 0

let show_local_t (local_t : local_t) =
  let show_local_protocol ((protocol, role), (roles, ltype)) =
    let roles_str = List.map ~f:RoleName.user roles in
    sprintf "%s@%s(%s) {\n\n%s\n}"
      (ProtocolName.user protocol)
      (RoleName.user role)
      (Symtable.show_roles (roles_str, []))
      (show ltype)
  in
  Map.to_alist local_t
  |> List.map ~f:show_local_protocol
  |> String.concat ~sep:"\n\n"

type local_proto_name_lookup = LocalProtocolName.t Map.M(LocalProtocolId).t

let build_local_proto_name_lookup (local_t : local_t) :
    local_proto_name_lookup =
  let gen_unique_name ~key:(protocol, role) ~data:_
      (local_protocol_names, name_gen) =
    let protocol_name =
      sprintf "%s_%s" (ProtocolName.user protocol) (RoleName.user role)
    in
    let name_gen, protocol_name =
      Namegen.unique_name name_gen protocol_name
    in
    let local_protocol_name = LocalProtocolName.of_string protocol_name in
    ( Map.add_exn local_protocol_names ~key:(protocol, role)
        ~data:local_protocol_name
    , name_gen )
  in
  let local_protocol_names, _ =
    Map.fold
      ~init:(Map.empty (module LocalProtocolId), Namegen.create ())
      ~f:gen_unique_name local_t
  in
  local_protocol_names

let lookup_protocol_id (lookup_table : local_proto_name_lookup)
    local_protocol_id =
  Map.find_exn lookup_table local_protocol_id

let lookup_local_protocol (lookup_table : local_proto_name_lookup) protocol
    role =
  let local_protocol_id = LocalProtocolId.create protocol role in
  Map.find_exn lookup_table local_protocol_id

let show_lookup_table table =
  let show_key (protocol, role) =
    sprintf "%s@%s" (RoleName.user role) (ProtocolName.user protocol)
  in
  let show_aux ~key ~data acc =
    sprintf "%s%s: %s\n" acc (show_key key) (LocalProtocolName.user data)
  in
  Map.fold table ~init:"" ~f:show_aux

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
        | AcceptL (role', protocol, roles, new_roles, caller, lty) as l -> (
            let label = call_label caller protocol roles in
            match List.Assoc.find acc ~equal:LabelName.equal label with
            | None -> (label, l) :: acc
            | Some (AcceptL (_, _, _, _, _, lty_)) ->
                List.Assoc.add acc ~equal:LabelName.equal label
                  (AcceptL
                     ( role'
                     , protocol
                     , roles
                     , new_roles
                     , caller
                     , merge projected_role lty lty_ ))
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
    | AcceptL (_, _, _, _, caller, _), RecvL (_, r2, _) ->
        if not @@ RoleName.equal caller r2 then fail () ;
        merge_recv r2 [lty1; lty2]
    | RecvL (_, r1, _), AcceptL (_, _, _, _, caller, _) ->
        if not @@ RoleName.equal caller r1 then fail () ;
        merge_recv r1 [lty1; lty2]
    | ChoiceL (r1, ltys1), RecvL (_, r2, _) when RoleName.equal r1 r2 ->
        (* Choice is a set of receive *)
        merge_recv r1 (lty2 :: ltys1)
    | RecvL (_, r2, _), ChoiceL (r1, ltys2) when RoleName.equal r1 r2 ->
        merge_recv r1 (lty1 :: ltys2)
    | ChoiceL (r1, ltys1), ChoiceL (r2, ltys2)
      when RoleName.equal r1 r2 && not (RoleName.equal r1 projected_role) ->
        merge_recv r1 (ltys1 @ ltys2)
    | AcceptL (_, _, _, _, caller1, _), AcceptL (_, _, _, _, caller2, _) ->
        if not @@ RoleName.equal caller1 caller2 then fail () ;
        merge_recv caller1 [lty1; lty2]
    | ChoiceL (r1, ltys1), AcceptL (_, _, _, _, caller, _)
      when RoleName.equal r1 caller && not (RoleName.equal r1 projected_role)
      ->
        merge_recv r1 (lty2 :: ltys1)
    | AcceptL (_, _, _, _, caller, _), ChoiceL (r2, ltys2)
      when RoleName.equal r2 caller && not (RoleName.equal r2 projected_role)
      ->
        merge_recv r2 (lty1 :: ltys2)
    | _ -> if equal lty1 lty2 then lty1 else fail ()
  with Unmergable (l1, l2) ->
    let error = show l1 ^ " " ^ show l2 in
    uerr @@ Err.UnableToMerge error

(* Check whether the first message in a g choice is from choice_r to recv_r,
   if recv_r is Some; return receive role *)
(* In nested protocols, calls will send invitation messages all participants,
   so need to check if any of the roles in a call is the receiver *)
let rec check_consistent_gchoice choice_r possible_roles = function
  | MessageG (_, send_r, recv_r_, _) ->
      if not @@ RoleName.equal send_r choice_r then
        uerr (RoleMismatch (choice_r, send_r)) ;
      if
        (not @@ Set.is_empty possible_roles)
        && (not @@ Set.mem possible_roles recv_r_)
      then uerr (RoleMismatch (Set.choose_exn possible_roles, recv_r_)) ;
      Set.singleton (module RoleName) recv_r_
  | CallG (caller_r, protocol, roles, _) ->
      if not @@ RoleName.equal caller_r choice_r then
        uerr (RoleMismatch (choice_r, caller_r)) ;
      let roles_set = Set.of_list (module RoleName) roles in
      if Set.is_empty possible_roles then roles_set
      else
        let intersection = Set.inter possible_roles roles_set in
        if Set.is_empty intersection then
          uerr (ChoiceCallRoleMismatch protocol) ;
        intersection
  | ChoiceG (new_choice_r, gs) ->
      if not @@ RoleName.equal choice_r new_choice_r then
        uerr (RoleMismatch (choice_r, new_choice_r)) ;
      Set.union_list
        (module RoleName)
        (List.map ~f:(check_consistent_gchoice choice_r possible_roles) gs)
  | MuG (_, _, g) -> check_consistent_gchoice choice_r possible_roles g
  | TVarG (_, _, g) ->
      check_consistent_gchoice choice_r possible_roles (Lazy.force g)
  | g ->
      raise
        (Violation
           ( "Normalised global type always has a message in choice branches\n"
           ^ Gtype.show g ))

let rec project' (global_t : global_t) (projected_role : RoleName.t) =
  function
  | EndG -> EndL
  | TVarG (name, _, _) -> TVarL name
  | MuG (name, _, g_type) -> (
    match project' global_t projected_role g_type with
    | TVarL _ | EndL -> EndL
    | lType -> MuL (name, lType) )
  | MessageG (m, send_r, recv_r, g_type) -> (
      let next = project' global_t projected_role g_type in
      match projected_role with
      | _ when RoleName.equal projected_role send_r -> SendL (m, recv_r, next)
      | _ when RoleName.equal projected_role recv_r -> RecvL (m, send_r, next)
      | _ ->
          let named_payloads =
            List.rev_filter_map
              ~f:(function
                | PValue (Some var, t) -> Some (var, t) | _ -> None)
              m.payload
          in
          if
            List.is_empty named_payloads
            || not !Config.refinement_type_enabled
          then next
          else
            List.fold ~init:next
              ~f:(fun acc (var, t) -> SilentL (var, t, acc))
              named_payloads )
  | ChoiceG (choice_r, g_types) -> (
      let check_distinct_prefix gtys =
        let rec aux acc = function
          | [] -> ()
          | MessageG (m, _, _, _) :: rest ->
              let l = m.label in
              if Set.mem acc l (* FIXME: Use 2 labels for location *) then
                uerr (DuplicateLabel l)
              else aux (Set.add acc l) rest
          | CallG (caller, protocol, roles, _) :: rest ->
              let l = call_label caller protocol roles in
              if Set.mem acc l then uerr (DuplicateLabel l)
              else aux (Set.add acc l) rest
          | ChoiceG (_, gs) :: rest -> aux acc (gs @ rest)
          | MuG (_, _, g) :: rest -> aux acc (g :: rest)
          | TVarG (_, _, g) :: rest -> aux acc (Lazy.force g :: rest)
          | _ ->
              raise
                (Violation
                   "Normalised global type always has a message in choice \
                    branches")
        in
        aux (Set.empty (module LabelName)) gtys
      in
      check_distinct_prefix g_types ;
      let possible_roles =
        List.fold
          ~f:(check_consistent_gchoice choice_r)
          ~init:(Set.empty (module RoleName))
          g_types
      in
      let recv_r = Set.choose_exn possible_roles in
      let l_types = List.map ~f:(project' global_t projected_role) g_types in
      match projected_role with
      | _
        when RoleName.equal projected_role choice_r
             || RoleName.equal projected_role recv_r ->
          ChoiceL (choice_r, l_types)
      | _ -> (
        match List.reduce ~f:(merge projected_role) l_types with
        | Some l -> l
        | None -> EndL ) )
  | CallG (caller, protocol, roles, g_type) -> (
      let next = project' global_t projected_role g_type in
      let (protocol_roles, new_protocol_roles), _, _ =
        Map.find_exn global_t protocol
      in
      let gen_acceptl next =
        let role_elem =
          List.findi roles ~f:(fun _ r' -> RoleName.equal projected_role r')
        in
        let idx, _ = Option.value_exn role_elem in
        let role_in_proto = List.nth_exn protocol_roles idx in
        AcceptL
          (role_in_proto, protocol, roles, new_protocol_roles, caller, next)
      in
      let gen_invitecreatel next =
        InviteCreateL (roles, new_protocol_roles, protocol, next)
      in
      let is_caller = RoleName.equal caller projected_role in
      let is_participant =
        List.mem roles projected_role ~equal:RoleName.equal
      in
      match projected_role with
      | _ when is_caller && is_participant ->
          let acceptl = gen_acceptl next in
          gen_invitecreatel acceptl
      | _ when is_caller -> gen_invitecreatel next
      | _ when is_participant -> gen_acceptl next
      | _ -> next )

let project projected_role g =
  project' (Map.empty (module ProtocolName)) projected_role g

let project_global_t (global_t : global_t) =
  let project_role protocol_name all_roles gtype local_protocols
      projected_role =
    let ltype = project' global_t projected_role gtype in
    Map.add_exn local_protocols
      ~key:(protocol_name, projected_role)
      ~data:(all_roles, ltype)
  in
  Map.fold
    ~init:(Map.empty (module LocalProtocolId))
    ~f:(fun ~key ~data local_protocols ->
      let (roles, new_roles), _, gtype = data in
      let all_roles = roles @ new_roles in
      List.fold ~init:local_protocols
        ~f:(project_role key all_roles gtype)
        all_roles)
    global_t
