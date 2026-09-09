open! Base
open Printf
open Gtype
open Err
open Names
open Message

type t =
  | RecvL of message * RoleName.t * t
  | SendL of message * RoleName.t * t
  | ChoiceL of RoleName.t * t list
  | TVarL of TypeVariableName.t * Expr.t list
  | MuL of TypeVariableName.t * (bool * Gtype.rec_var) list * t
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

module LocalProtocolId = struct
  module T = struct
    type t = ProtocolName.t * RoleName.t
    [@@deriving show {with_path= false}, sexp_of, ord]

    let create protocol role = (protocol, role)

    let get_role (_, role) = role

    let get_protocol (protocol, _) = protocol
  end

  include T
  include Comparable.Make (T)
end

type nested_t = (RoleName.t list * t) Map.M(LocalProtocolId).t

module Formatting = struct
  open! Stdlib.Format

  let rec pp_roles ppf = function
    | [] -> ()
    | r :: roles ->
        pp_print_string ppf (RoleName.user r) ;
        pp_print_string ppf ", " ;
        pp_roles ppf roles

  let rec pp ppf = function
    | RecvL (m, r, l) ->
        pp_print_string ppf (show_message m) ;
        pp_print_string ppf " from " ;
        pp_print_string ppf (RoleName.user r) ;
        pp_print_string ppf ";" ;
        pp_force_newline ppf () ;
        pp ppf l
    | SendL (m, r, l) ->
        pp_print_string ppf (show_message m) ;
        pp_print_string ppf " to " ;
        pp_print_string ppf (RoleName.user r) ;
        pp_print_string ppf ";" ;
        pp_force_newline ppf () ;
        pp ppf l
    | InviteCreateL (invite_roles, create_roles, protocol, l) ->
        let protocol = ProtocolName.user protocol in
        pp_print_string ppf "invite(" ;
        pp_roles ppf invite_roles ;
        pp_print_string ppf ") to " ;
        pp_print_string ppf protocol ;
        pp_print_string ppf ";" ;
        pp_force_newline ppf () ;
        if not (List.is_empty create_roles) then (
          let rec pp_create_roles = function
            | [] -> ()
            | r :: roles ->
                pp_print_string ppf "role " ;
                pp_print_string ppf (RoleName.user r) ;
                if not (List.is_empty roles) then pp_print_string ppf "," ;
                pp_create_roles roles
          in
          pp_print_string ppf "create(" ;
          pp_create_roles create_roles ;
          pp_print_string ppf ") in " ;
          pp_print_string ppf protocol ;
          pp_print_string ppf ";" ;
          pp_force_newline ppf () ) ;
        pp ppf l
    | AcceptL (role, protocol, roles, new_roles, caller, l) ->
        pp_print_string ppf "accept " ;
        pp_print_string ppf (RoleName.user role) ;
        pp_print_string ppf "@" ;
        pp_print_string ppf (ProtocolName.user protocol) ;
        pp_print_string ppf "(" ;
        pp_print_string ppf (Symtable.show_roles (roles, new_roles)) ;
        pp_print_string ppf ") from " ;
        pp_print_string ppf (RoleName.user caller) ;
        pp_print_string ppf ";" ;
        pp_force_newline ppf () ;
        pp ppf l
    | SilentL (var, ty, l) ->
        pp_print_string ppf "(silent) " ;
        pp_print_string ppf (VariableName.user var) ;
        pp_print_string ppf "(" ;
        pp_print_string ppf (Expr.show_payload_type ty) ;
        pp_print_string ppf ");" ;
        pp_force_newline ppf () ;
        pp ppf l
    | MuL (n, rec_vars, l) ->
        pp_print_string ppf "rec " ;
        pp_print_string ppf (TypeVariableName.user n) ;
        pp_print_string ppf " " ;
        if not (List.is_empty rec_vars) then (
          let rec pp_recvars = function
            | [] -> ()
            | (is_silent, recvar) :: recvars ->
                if is_silent then fprintf ppf "(silent) " ;
                pp_rec_var ppf recvar ;
                if not (List.is_empty recvars) then pp_print_string ppf ", " ;
                pp_recvars recvars
          in
          pp_print_string ppf "[" ;
          pp_open_box ppf 2 ;
          pp_recvars rec_vars ;
          pp_close_box ppf () ;
          pp_print_string ppf "] " ) ;
        pp_print_string ppf "{" ;
        pp_force_newline ppf () ;
        pp_open_box ppf 2 ;
        pp_print_string ppf "  " ;
        pp ppf l ;
        pp_close_box ppf () ;
        pp_force_newline ppf () ;
        pp_print_string ppf "}"
    | TVarL (n, rec_exprs) ->
        let rec_exprs_s =
          if List.is_empty rec_exprs then ""
          else
            " ["
            ^ String.concat ~sep:", " (List.map ~f:Expr.show rec_exprs)
            ^ "]"
        in
        pp_print_string ppf "continue " ;
        pp_print_string ppf (TypeVariableName.user n) ;
        pp_print_string ppf rec_exprs_s ;
        pp_print_string ppf ";"
    | EndL -> pp_print_string ppf "(end)"
    | ChoiceL (r, ls) ->
        pp_print_string ppf "choice at " ;
        pp_print_string ppf (RoleName.user r) ;
        pp_print_string ppf " {" ;
        pp_force_newline ppf () ;
        pp_open_box ppf 2 ;
        pp_print_string ppf "  " ;
        let rec pp_choices = function
          | [] ->
              Err.violation ~here:[%here] "Choice branches must not be empty"
          | [l] ->
              pp ppf l ;
              pp_close_box ppf () ;
              pp_force_newline ppf () ;
              pp_print_string ppf "}"
          | l :: ls ->
              pp ppf l ;
              pp_close_box ppf () ;
              pp_force_newline ppf () ;
              pp_print_string ppf "} or {" ;
              pp_force_newline ppf () ;
              pp_open_box ppf 2 ;
              pp_print_string ppf "  " ;
              pp_choices ls
        in
        pp_choices ls

  let show ltype =
    let buffer = Buffer.create 1024 in
    let ppf = formatter_of_buffer buffer in
    fprintf ppf "%a@?" pp ltype ;
    Buffer.contents buffer
end

include Formatting

let show_nested_t (nested_t : nested_t) =
  let show_local_protocol ((protocol, role), (roles, ltype)) =
    sprintf "%s@%s(%s) {\n\n%s\n}" (RoleName.user role)
      (ProtocolName.user protocol)
      (Symtable.show_roles (roles, []))
      (show ltype)
  in
  Map.to_alist nested_t
  |> List.map ~f:show_local_protocol
  |> String.concat ~sep:"\n\n"

type local_proto_name_lookup = LocalProtocolName.t Map.M(LocalProtocolId).t

let build_local_proto_name_lookup (nested_t : nested_t) :
    local_proto_name_lookup =
  let module Namegen = Namegen.Make (ProtocolName) in
  let gen_unique_name ~key:(protocol, role) ~data:_
      (local_protocol_names, name_gen) =
    let protocol_name =
      ProtocolName.rename protocol
        (sprintf "%s_%s" (ProtocolName.user protocol) (RoleName.user role))
    in
    let name_gen, protocol_name =
      Namegen.unique_name name_gen protocol_name
    in
    let local_protocol_name =
      LocalProtocolName.of_other_name (module ProtocolName) protocol_name
    in
    ( Map.add_exn local_protocol_names ~key:(protocol, role)
        ~data:local_protocol_name
    , name_gen )
  in
  let local_protocol_names, _ =
    Map.fold
      ~init:(Map.empty (module LocalProtocolId), Namegen.create ())
      ~f:gen_unique_name nested_t
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

(* Remove silent prefixes in a silent type, and return a list of silent
   prefixes and a non-silent type (that is not a choice) *)
let un_silent lty =
  let rec aux acc = function
    | [] -> acc
    | (curr, lty) :: rest -> (
      match lty with
      | SilentL (v, ty, lty) -> aux acc (((v, ty) :: curr, lty) :: rest)
      | ChoiceL (_, ltys) ->
          aux acc (List.map ~f:(fun lty -> (curr, lty)) ltys @ rest)
      | _ -> aux ((curr, lty) :: acc) rest )
  in
  aux [] [([], lty)]

let rec re_silent (vars, lty) =
  match vars with
  | [] -> lty
  | (v, ty) :: rest -> re_silent (rest, SilentL (v, ty, lty))

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
              when List.equal equal_payload m.payload m_.payload ->
                List.Assoc.add acc ~equal:LabelName.equal label
                  (RecvL (m, r, merge projected_role lty l_))
            | Some (RecvL _) -> fail ()
            | _ ->
                violation ~here:[%here]
                  "Merge receive must be merging receive local types" )
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
                     , merge projected_role lty lty_ ) )
            | _ ->
                violation ~here:[%here]
                  "Merge receive must be merging receive local types" )
        | _ ->
            violation ~here:[%here]
              "Merge receive must be merging receive local types"
      in
      let conts = List.fold ~f:aux ~init:[] recvs in
      match conts with
      | [] -> EndL
      | [(_, lty)] -> lty
      | conts -> ChoiceL (r, List.map ~f:snd conts)
    in
    let merge_silent_prefix lty1 lty2 =
      let unsilent1 = un_silent lty1 in
      let unsilent2 = un_silent lty2 in
      let var_lty_pairs = unsilent1 @ unsilent2 in
      (* var_lty_pairs should have length >= 2 *)
      let rec try_merge env ltys =
        match (env, ltys) with
        | `Init, (_, EndL) :: rest -> try_merge `End rest
        | `Init, (_, (TVarL _ as tv)) :: rest -> try_merge (`TVar tv) rest
        | `Init, (vars, (RecvL (m, r, _) as lty)) :: rest ->
            try_merge
              (`Recv
                 (Set.singleton (module LabelName) m.label, r, [(vars, lty)])
              )
              rest
        | `End, [] -> EndL
        | `End, (_, EndL) :: rest -> try_merge `End rest
        | `TVar tv, [] -> tv
        | `TVar tv, (_, (TVarL _ as tv')) :: rest when equal tv tv' ->
            try_merge (`TVar tv) rest
        | `Recv (_, recv_role, acc), [] ->
            ChoiceL (recv_role, List.rev_map ~f:re_silent acc)
        | ( `Recv (seen, recv_role, acc)
          , (vars, (RecvL (m, r, _) as lty)) :: rest )
          when RoleName.equal recv_role r && not (Set.mem seen m.label) ->
            try_merge
              (`Recv (Set.add seen m.label, recv_role, (vars, lty) :: acc))
              rest
        | _ -> fail ()
      in
      try_merge `Init var_lty_pairs
    in
    match (lty1, lty2) with
    | RecvL (_, r1, _), RecvL (_, r2, _) ->
        if not @@ RoleName.equal r1 r2 then fail () ;
        merge_recv r1 [lty1; lty2]
    | SendL (m1, r1, lty1), SendL (m2, r2, lty2)
      when RoleName.equal r1 r2 && equal_message m1 m2 ->
        SendL (m1, r2, merge projected_role lty1 lty2)
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
    | SilentL _, _ | _, SilentL _ -> merge_silent_prefix lty1 lty2
    | _ -> if equal lty1 lty2 then lty1 else fail ()
  with Unmergable (l1, l2) ->
    let error = show l1 ^ "\nand\n" ^ show l2 in
    uerr @@ Err.UnableToMerge (String.strip error, Some projected_role)

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
      violation ~here:[%here]
        ( "Normalised global type always has a message in choice branches\n"
        ^ Gtype.show g )

(* Various infomation needed during projection *)
type project_env =
  { penv: Gtype.nested_t (* Info for nested protocols *)
  ; rvenv: (bool * rec_var) list Map.M(TypeVariableName).t
        (* Info for refinement variables *)
  ; silent_vars: Set.M(VariableName).t
        (* Info for silent variables (refinement types ) *)
  ; unguarded_tv: Set.M(TypeVariableName).t (* Unguarded type variables *) }

let new_project_env =
  { penv= Map.empty (module ProtocolName)
  ; rvenv= Map.empty (module TypeVariableName)
  ; silent_vars= Set.empty (module VariableName)
  ; unguarded_tv= Set.empty (module TypeVariableName) }

(* Projection keeps uninvolved choices explicit until all recursion binders
   have been constructed. This representation never escapes this module. *)
module Pending = struct
  type t =
    | RecvL of message * RoleName.t * t
    | SendL of message * RoleName.t * t
    | ChoiceL of RoleName.t * t list
    | TVarL of TypeVariableName.t * Expr.t list
    | MuL of TypeVariableName.t * (bool * Gtype.rec_var) list * t
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
    | MergeL of t list
end

let rec project' env (projected_role : RoleName.t) =
  let check_expr silent_vars e =
    let free_vars = Expr.free_var e in
    let unknown_vars = Set.inter free_vars silent_vars in
    if not @@ Set.is_empty unknown_vars then
      uerr
        (UnknownVariableValue (projected_role, Set.choose_exn unknown_vars))
  in
  function
  | EndG -> Pending.EndL
  | TVarG (name, _, _) when Set.mem env.unguarded_tv name ->
      (* Type variable unguarded *)
      Pending.EndL
  | TVarG (name, rec_exprs, _) ->
      let {rvenv; silent_vars; _} = env in
      let rec_expr_filter = Map.find_exn rvenv name in
      let rec_exprs =
        List.map2_exn
          ~f:(fun (x, _) y -> if not x then Some y else None)
          rec_expr_filter rec_exprs
      in
      let rec_exprs = List.filter_opt rec_exprs in
      List.iter ~f:(check_expr silent_vars) rec_exprs ;
      Pending.TVarL (name, rec_exprs)
  | MuG (name, rec_exprs, g_type) -> (
      let rec_exprs =
        List.map
          ~f:(fun ({rv_roles; _} as rec_expr) ->
            ( not @@ List.mem ~equal:RoleName.equal rv_roles projected_role
            , rec_expr ) )
          rec_exprs
      in
      let {rvenv; silent_vars; unguarded_tv; _} = env in
      let silent_vars =
        List.fold ~init:silent_vars
          ~f:(fun acc (is_silent, {rv_name; _}) ->
            if is_silent then Set.add acc rv_name else acc )
          rec_exprs
      in
      (* FIXME: This breaks when there are shadowed type variables *)
      let rvenv =
        match Map.add ~key:name ~data:rec_exprs rvenv with
        | `Ok rvenv -> rvenv
        | `Duplicate -> rvenv
      in
      let env =
        {env with rvenv; silent_vars; unguarded_tv= Set.add unguarded_tv name}
      in
      match project' env projected_role g_type with
      | Pending.TVarL _ | Pending.EndL -> Pending.EndL
      | lType -> Pending.MuL (name, rec_exprs, lType) )
  | MessageG (m, send_r, recv_r, g_type) -> (
      let next env = project' env projected_role g_type in
      match projected_role with
      (* When projected role is involved in an interaction, reset
         unguarded_tv *)
      | _ when RoleName.equal projected_role send_r ->
          Pending.SendL
            ( m
            , recv_r
            , next
                {env with unguarded_tv= Set.empty (module TypeVariableName)}
            )
      | _ when RoleName.equal projected_role recv_r ->
          Pending.RecvL
            ( m
            , send_r
            , next
                {env with unguarded_tv= Set.empty (module TypeVariableName)}
            )
      (* When projected role is not involved in an interaction, do not reset
       * unguarded_tv *)
      | _ ->
          let named_payloads =
            List.rev_filter_map
              ~f:(function
                | PValue (Some var, t) -> Some (var, t) | _ -> None )
              m.payload
          in
          if
            List.is_empty named_payloads
            || (not @@ Pragma.refinement_type_enabled ())
          then next env
          else
            let {silent_vars; _} = env in
            let silent_vars =
              List.fold ~init:silent_vars
                ~f:(fun acc (var, _) -> Set.add acc var)
                named_payloads
            in
            let env = {env with silent_vars} in
            List.fold ~init:(next env)
              ~f:(fun acc (var, t) -> Pending.SilentL (var, t, acc))
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
              violation ~here:[%here]
                "Normalised global type always has a message in choice \
                 branches"
        in
        aux (Set.empty (module LabelName)) gtys
      in
      let check_guarded_prefix gtys =
        let rec aux acc = function
          | [] -> ()
          | MessageG (m, _, _, _) :: rest ->
              let l = m.label in
              let existing = Map.find acc l |> Option.value ~default:[] in
              List.iter existing ~f:(fun prev_m ->
                  if
                    not
                      (Message.payloads_compatible prev_m.payload m.payload)
                  then uerr (GuardedChoiceError (l, IncompatiblePayloads))
                  else
                    match
                      ( Message.extract_message_guard prev_m
                      , Message.extract_message_guard m )
                    with
                    | Some g1, Some g2 ->
                        if not (Message.guards_disjoint m.payload g1 g2) then
                          uerr (GuardedChoiceError (l, OverlappingGuards))
                    | _ -> uerr (GuardedChoiceError (l, MissingGuard)) ) ;
              aux (Map.add_multi acc ~key:l ~data:m) rest
          | CallG (caller, protocol, roles, _) :: rest ->
              let l = call_label caller protocol roles in
              if Map.mem acc l then uerr (DuplicateLabel l) else aux acc rest
          | ChoiceG (_, gs) :: rest -> aux acc (gs @ rest)
          | MuG (_, _, g) :: rest -> aux acc (g :: rest)
          | TVarG (_, _, g) :: rest -> aux acc (Lazy.force g :: rest)
          | _ ->
              violation ~here:[%here]
                "Normalised global type always has a message in choice \
                 branches"
        in
        aux (Map.empty (module LabelName)) gtys
      in
      if Pragma.guarded_uniqueness () then check_guarded_prefix g_types
      else check_distinct_prefix g_types ;
      let possible_roles =
        List.fold
          ~f:(check_consistent_gchoice choice_r)
          ~init:(Set.empty (module RoleName))
          g_types
      in
      let recv_r = Set.choose_exn possible_roles in
      let l_types = List.map ~f:(project' env projected_role) g_types in
      match projected_role with
      | _
        when RoleName.equal projected_role choice_r
             || RoleName.equal projected_role recv_r -> (
        match l_types with
        | [ltype] -> ltype
        | _ -> Pending.ChoiceL (choice_r, l_types) )
      | _ -> Pending.MergeL l_types )
  | CallG (caller, protocol, roles, g_type) -> (
      (* Reset unguarded_tv *)
      let next =
        project'
          {env with unguarded_tv= Set.empty (module TypeVariableName)}
          projected_role g_type
      in
      let {penv; _} = env in
      let {static_roles; dynamic_roles; _} = Map.find_exn penv protocol in
      let gen_acceptl next =
        let role_elem =
          List.findi roles ~f:(fun _ r' -> RoleName.equal projected_role r')
        in
        let idx, _ = Option.value_exn role_elem in
        let role_in_proto = List.nth_exn static_roles idx in
        Pending.AcceptL
          (role_in_proto, protocol, roles, dynamic_roles, caller, next)
      in
      let gen_invitecreatel next =
        Pending.InviteCreateL (roles, dynamic_roles, protocol, next)
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

let rec finish_projection projected_role (pending : Pending.t) : t =
  let next = finish_projection projected_role in
  match pending with
  | Pending.RecvL (m, r, l) -> RecvL (m, r, next l)
  | Pending.SendL (m, r, l) -> SendL (m, r, next l)
  | Pending.ChoiceL (r, ls) -> ChoiceL (r, List.map ~f:next ls)
  | Pending.MergeL ls ->
      List.map ~f:next ls
      |> List.reduce ~f:(merge projected_role)
      |> Option.value ~default:EndL
  | Pending.MuL (n, rvs, l) -> (
    match next l with TVarL _ | EndL -> EndL | body -> MuL (n, rvs, body) )
  | Pending.TVarL (n, es) -> TVarL (n, es)
  | Pending.EndL -> EndL
  | Pending.SilentL (v, ty, l) -> SilentL (v, ty, next l)
  | Pending.InviteCreateL (rs, ns, p, l) -> InviteCreateL (rs, ns, p, next l)
  | Pending.AcceptL (r, p, rs, ns, caller, l) ->
      AcceptL (r, p, rs, ns, caller, next l)

module Recursive_merge = struct
  (* IDs, rather than OCaml object addresses or unfolded syntax, give each
     recursive obligation a stable identity. The input graph is immutable
     once construction finishes. *)
  type node =
    | Receive of message * RoleName.t * int
    | Send of message * RoleName.t * int
    | Choice of RoleName.t * int list
    | Merge of int list
    | Alias of int
    | End

  exception Unsupported

  module State = struct
    type t = int list [@@deriving sexp_of, ord]

    let hash = Hashtbl.hash
  end

  let project projected_role pending =
    let input = Hashtbl.create (module Int) in
    let fresh table node =
      let id = Hashtbl.length table in
      Hashtbl.add_exn table ~key:id ~data:node ;
      id
    in
    let rec build env (pending : Pending.t) =
      let next = build env in
      match pending with
      | Pending.RecvL (m, r, l) -> fresh input (Receive (m, r, next l))
      | Pending.SendL (m, r, l) -> fresh input (Send (m, r, next l))
      | Pending.ChoiceL (r, ls) ->
          fresh input (Choice (r, List.map ~f:next ls))
      | Pending.MergeL ls -> fresh input (Merge (List.map ~f:next ls))
      | Pending.MuL (name, [], l) ->
          let id = fresh input End in
          let body = build (Map.set env ~key:name ~data:id) l in
          Hashtbl.set input ~key:id ~data:(Alias body) ;
          id
      | Pending.TVarL (name, []) -> Map.find_exn env name
      | Pending.EndL -> fresh input End
      (* Unfolding parameterised recursion requires substitution. Silent
         knowledge and invitations also have their own merge rules; retain
         the existing projection behavior for these extensions. *)
      | Pending.MuL _ | Pending.TVarL _ | Pending.SilentL _
       |Pending.InviteCreateL _ | Pending.AcceptL _ ->
          raise Unsupported
    in
    let root = build (Map.empty (module TypeVariableName)) pending in
    let output = Hashtbl.create (module Int) in
    let memo = Hashtbl.create (module State) in
    (* Resolve only epsilon edges. A cycle is not evidence that actions
       match: every resulting state must still pass the checks below. *)
    let frontier ids =
      let rec visit seen acc = function
        | [] -> List.dedup_and_sort ~compare:Int.compare acc
        | id :: rest when Set.mem seen id -> visit seen acc rest
        | id :: rest -> (
            let seen = Set.add seen id in
            match Hashtbl.find_exn input id with
            | Alias next -> visit seen acc (next :: rest)
            | Merge next -> visit seen acc (next @ rest)
            | Choice (r, next) when not (RoleName.equal r projected_role) ->
                visit seen acc (next @ rest)
            | _ -> visit seen (id :: acc) rest )
      in
      visit (Set.empty (module Int)) [] ids
    in
    let rec solve ids =
      let ids = frontier ids in
      match Hashtbl.find memo ids with
      | Some id -> id
      | None ->
          let id = fresh output End in
          (* Reserve the result before solving any continuation. A repeated
             obligation points here, not to either of its operands. *)
          Hashtbl.add_exn memo ~key:ids ~data:id ;
          let nodes = List.map ids ~f:(Hashtbl.find_exn input) in
          let result =
            match nodes with
            | [] -> raise Unsupported
            | End :: rest ->
                if
                  not
                    (List.for_all rest ~f:(function
                      | End -> true
                      | _ -> false ))
                then raise Unsupported ;
                End
            | Receive (_, sender, _) :: _ -> (
                let groups =
                  List.fold nodes
                    ~init:(Map.empty (module LabelName))
                    ~f:(fun groups -> function
                      | Receive (m, r, next) when RoleName.equal sender r
                        -> (
                        match Map.find groups m.label with
                        | None ->
                            Map.set groups ~key:m.label ~data:(m, [next])
                        | Some (previous, nexts) ->
                            if
                              not
                                (List.equal equal_payload m.payload
                                   previous.payload )
                            then raise Unsupported ;
                            Map.set groups ~key:m.label
                              ~data:(previous, next :: nexts) )
                      | _ -> raise Unsupported )
                in
                let branches =
                  Map.data groups
                  |> List.map ~f:(fun (m, nexts) ->
                      Receive (m, sender, solve nexts) )
                in
                match branches with
                | [branch] -> branch
                | _ -> Choice (sender, List.map branches ~f:(fresh output)) )
            | Send (m, receiver, _) :: _ ->
                let nexts =
                  List.map nodes ~f:(function
                    | Send (m', r, next)
                      when RoleName.equal receiver r && equal_message m m' ->
                        next
                    | _ -> raise Unsupported )
                in
                Send (m, receiver, solve nexts)
            | Choice (r, branches) :: rest ->
                (* Internal choices must agree; only external receives can
                   acquire extra alternatives through merge. *)
                let alternatives =
                  List.map rest ~f:(function
                    | Choice (r', bs)
                      when RoleName.equal r r'
                           && List.length bs = List.length branches ->
                        bs
                    | _ -> raise Unsupported )
                in
                Choice
                  ( r
                  , List.mapi branches ~f:(fun i branch ->
                        solve
                          ( branch
                          :: List.map alternatives ~f:(fun bs ->
                              List.nth_exn bs i ) ) ) )
            | Alias _ :: _ | Merge _ :: _ -> assert false
          in
          Hashtbl.set output ~key:id ~data:result ;
          id
    in
    let root = solve [root] in
    (* Reify with a path-local binder environment. Shared acyclic states may
       be duplicated, but every back-edge refers to an enclosing binder. *)
    let used = Hash_set.create (module Int) in
    let rec reify active id =
      let name = TypeVariableName.of_string (sprintf "Merge%d" id) in
      if Set.mem active id then (
        Hash_set.add used id ;
        TVarL (name, []) )
      else
        let next = reify (Set.add active id) in
        let body =
          match Hashtbl.find_exn output id with
          | Receive (m, r, l) -> RecvL (m, r, next l)
          | Send (m, r, l) -> SendL (m, r, next l)
          | Choice (r, ls) -> ChoiceL (r, List.map ~f:next ls)
          | End -> EndL
          | Alias _ | Merge _ -> assert false
        in
        if Hash_set.mem used id then MuL (name, [], body) else body
    in
    reify (Set.empty (module Int)) root
end

let project_with_env env projected_role g =
  let pending = project' env projected_role g in
  (* Keep established output and extension-specific merge behavior whenever
     structural projection succeeds. Only a failed merge needs the graph
     solver; other projection errors must propagate unchanged. *)
  try finish_projection projected_role pending
  with UserError (UnableToMerge _ as error) -> (
    try Recursive_merge.project projected_role pending
    with Recursive_merge.Unsupported -> uerr error )

let project projected_role g =
  project_with_env new_project_env projected_role g

let project_nested_t (nested_t : Gtype.nested_t) =
  let project_role protocol_name all_roles gtype local_protocols
      projected_role =
    let ltype =
      project_with_env
        {new_project_env with penv= nested_t}
        projected_role gtype
    in
    (* TODO: Fix make unique tvars *)
    Map.add_exn local_protocols
      ~key:(protocol_name, projected_role)
      ~data:(all_roles, ltype)
  in
  Map.fold
    ~init:(Map.empty (module LocalProtocolId))
    ~f:(fun ~key ~data local_protocols ->
      let {static_roles; dynamic_roles; gtype; _} = data in
      let all_roles = static_roles @ dynamic_roles in
      List.fold ~init:local_protocols
        ~f:(project_role key all_roles gtype)
        all_roles )
    nested_t

let make_unique_tvars ltype =
  (* TODO: Handle expressions in recursion and recursive variables *)
  let module Namegen = Namegen.Make (TypeVariableName) in
  let rec rename_tvars tvar_mapping namegen = function
    | RecvL (msg, sender, l) ->
        let namegen, l = rename_tvars tvar_mapping namegen l in
        (namegen, RecvL (msg, sender, l))
    | SendL (msg, recv, l) ->
        let namegen, l = rename_tvars tvar_mapping namegen l in
        (namegen, SendL (msg, recv, l))
    | MuL (tvar, rec_vars, l) ->
        let namegen, new_tvar = Namegen.unique_name namegen tvar in
        let tvar_mapping =
          Map.update tvar_mapping tvar ~f:(fun _ -> new_tvar)
        in
        let namegen, l = rename_tvars tvar_mapping namegen l in
        (namegen, MuL (new_tvar, rec_vars, l))
    | TVarL (tvar, rec_exprs) ->
        (namegen, TVarL (Map.find_exn tvar_mapping tvar, rec_exprs))
    | EndL as g -> (namegen, g)
    | ChoiceL (r, ls) ->
        let namegen, ls =
          List.fold_map ls ~init:namegen ~f:(rename_tvars tvar_mapping)
        in
        (namegen, ChoiceL (r, ls))
    | InviteCreateL (invite_roles, create_roles, protocol, l) ->
        let namegen, l = rename_tvars tvar_mapping namegen l in
        (namegen, InviteCreateL (invite_roles, create_roles, protocol, l))
    | AcceptL (role, protocol, roles, new_roles, caller, l) ->
        let namegen, l = rename_tvars tvar_mapping namegen l in
        (namegen, AcceptL (role, protocol, roles, new_roles, caller, l))
    | SilentL _ ->
        Err.unimpl ~here:[%here]
          "renaming recursive variables with refinements"
  in
  let namegen = Namegen.create () in
  let _, ltype =
    rename_tvars (Map.empty (module TypeVariableName)) namegen ltype
  in
  ltype

let ensure_unique_tvars nested_t : nested_t =
  Map.map nested_t ~f:(fun (roles, ltype) ->
      (roles, make_unique_tvars ltype) )
