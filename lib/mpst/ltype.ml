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
  | CallL of
      RoleName.t * ProtocolName.t * RoleName.t list * RoleName.t list * t
  | ICallL of
      (* Inl[t] is a local type that occurs in an "inlined" protocol call *)
      RoleName.t
      * ProtocolName.t
      * RoleName.t list
      * RoleName.t list
      * t
  | CombineL of t * t
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
    | CallL (role, protocol, roles, new_roles, l) ->
        pp_print_string ppf "call " ;
        pp_print_string ppf (RoleName.user role) ;
        pp_print_string ppf "@" ;
        pp_print_string ppf (ProtocolName.user protocol) ;
        pp_print_string ppf "(" ;
        pp_print_string ppf (Symtable.show_roles (roles, new_roles)) ;
        pp_print_string ppf ");" ;
        pp_force_newline ppf () ;
        pp ppf l
    | ICallL (role, protocol, roles, new_roles, l) ->
        pp_print_string ppf "{{call" ;
        pp_print_string ppf (RoleName.user role) ;
        pp_print_string ppf "@" ;
        pp_print_string ppf (ProtocolName.user protocol) ;
        pp_print_string ppf "(" ;
        pp_print_string ppf (Symtable.show_roles (roles, new_roles)) ;
        pp_print_string ppf ")}}{" ;
        pp_force_newline ppf () ;
        pp_open_box ppf 2 ;
        pp_print_string ppf "  " ;
        pp ppf l ;
        pp_close_box ppf () ;
        pp_force_newline ppf () ;
        pp_print_string ppf "}"
    | CombineL (l1, l2) ->
        pp_print_string ppf "[" ;
        pp ppf l1 ;
        pp_print_string ppf "] {" ;
        pp_force_newline ppf () ;
        pp_open_box ppf 2 ;
        pp_print_string ppf "  " ;
        pp ppf l2 ;
        pp_close_box ppf () ;
        pp_force_newline ppf () ;
        pp_print_string ppf "}"

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
(* Check whether the first message in a g choice is from choice_r to recv_r,
   if recv_r is Some; return receive role *)
(* In nested protocols, calls will send invitation messages all participants,
   so need to check if any of the roles in a call is the receiver *)
let rec check_consistent_directed_gchoice choice_r possible_roles = function
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
        (List.map
           ~f:(check_consistent_directed_gchoice choice_r possible_roles)
           gs )
  | MuG (_, _, g) ->
      check_consistent_directed_gchoice choice_r possible_roles g
  | TVarG (_, _, g) ->
      check_consistent_directed_gchoice choice_r possible_roles
        (Lazy.force g)
  | g ->
      violation ~here:[%here]
        ( "Normalised global type always has a message in choice branches\n"
        ^ Gtype.show g )

let rec check_consistent_gchoice choice_r = function
  | MessageG (_, send_r, _, _) ->
      if not @@ RoleName.equal send_r choice_r then
        uerr (RoleMismatch (choice_r, send_r)) ;
      ()
  | CallG (caller_r, _, _, _) ->
      if not @@ RoleName.equal caller_r choice_r then
        uerr (RoleMismatch (choice_r, caller_r)) ;
      ()
  | ChoiceG (new_choice_r, gs) ->
      if not @@ RoleName.equal choice_r new_choice_r then
        uerr (RoleMismatch (choice_r, new_choice_r)) ;
      List.iter ~f:(check_consistent_gchoice choice_r) gs
  | MuG (_, _, g) -> check_consistent_gchoice choice_r g
  | TVarG (_, _, g) -> check_consistent_gchoice choice_r (Lazy.force g)
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

let rec project' env (projected_role : RoleName.t) =
  let check_expr silent_vars e =
    let free_vars = Expr.free_var e in
    let unknown_vars = Set.inter free_vars silent_vars in
    if not @@ Set.is_empty unknown_vars then
      uerr
        (UnknownVariableValue (projected_role, Set.choose_exn unknown_vars))
  in
  function
  | EndG -> EndL
  | TVarG (name, _, _) when Set.mem env.unguarded_tv name ->
      (* Type variable unguarded *)
      EndL
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
      TVarL (name, rec_exprs)
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
      | TVarL _ | EndL -> EndL
      | lType -> MuL (name, rec_exprs, lType) )
  | MessageG (m, send_r, recv_r, g_type) -> (
      let next env = project' env projected_role g_type in
      match projected_role with
      (* When projected role is involved in an interaction, reset unguarded_tv
       * *)
      | _ when RoleName.equal projected_role send_r ->
          SendL
            ( m
            , recv_r
            , next
                {env with unguarded_tv= Set.empty (module TypeVariableName)}
            )
      | _ when RoleName.equal projected_role recv_r ->
          RecvL
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
              violation ~here:[%here]
                "Normalised global type always has a message in choice \
                 branches"
        in
        aux (Set.empty (module LabelName)) gtys
      in
      check_distinct_prefix g_types ;
      if Pragma.nested_protocol_enabled () then
        List.iter ~f:(check_consistent_gchoice choice_r) g_types ;
      let recv_r =
        if Pragma.nested_protocol_enabled () then None
        else
          let possible_roles =
            List.fold
              ~f:(check_consistent_directed_gchoice choice_r)
              ~init:(Set.empty (module RoleName))
              g_types
          in
          Some (Set.choose_exn possible_roles)
      in
      let l_types = List.map ~f:(project' env projected_role) g_types in
      match projected_role with
      | _
        when RoleName.equal projected_role choice_r
             || Option.is_some recv_r
                && RoleName.equal projected_role (Option.value_exn recv_r)
        -> (
        match l_types with
        | [ltype] -> ltype
        | _ -> ChoiceL (choice_r, l_types) )
      | _ -> (
        match List.reduce ~f:(merge projected_role) l_types with
        | Some l -> l
        | None -> EndL ) )
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
        AcceptL (role_in_proto, protocol, roles, dynamic_roles, caller, next)
      in
      let gen_calll next =
        let role_elem =
          List.findi roles ~f:(fun _ r' -> RoleName.equal projected_role r')
        in
        let idx, _ = Option.value_exn role_elem in
        let role_in_proto = List.nth_exn static_roles idx in
        CallL (role_in_proto, protocol, roles, dynamic_roles, next)
      in
      let gen_invitecreatel next =
        InviteCreateL (roles, dynamic_roles, protocol, next)
      in
      let is_caller = RoleName.equal caller projected_role in
      let is_participant =
        List.mem roles projected_role ~equal:RoleName.equal
      in
      match projected_role with
      | _ when is_caller && is_participant ->
          let call_l = gen_calll next in
          let acceptl = gen_acceptl call_l in
          gen_invitecreatel acceptl
      | _ when is_caller -> gen_invitecreatel next
      | _ when is_participant ->
          let call_l = gen_calll next in
          gen_acceptl call_l
      | _ -> next )
  | CombineG (g1, g2) ->
      CombineL
        (project' env projected_role g1, project' env projected_role g2)

let project projected_role g = project' new_project_env projected_role g

let project_nested_t (nested_t : Gtype.nested_t) =
  let project_role protocol_name all_roles gtype local_protocols
      projected_role =
    let ltype =
      project' {new_project_env with penv= nested_t} projected_role gtype
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
    | CallL (role, protocol, roles, new_roles, l) ->
        let namegen, l = rename_tvars tvar_mapping namegen l in
        (namegen, CallL (role, protocol, roles, new_roles, l))
    | ICallL (role, protocol, roles, new_roles, l) ->
        let namegen, l = rename_tvars tvar_mapping namegen l in
        (namegen, ICallL (role, protocol, roles, new_roles, l))
    | CombineL (l1, l2) ->
        let namegen, l1 = rename_tvars tvar_mapping namegen l1 in
        let namegen, l2 = rename_tvars tvar_mapping namegen l2 in
        (namegen, CombineL (l1, l2))
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

(* UPDATABLE RECURSION *)

let rec erase_upd x = function
  | RecvL (m, f, k) -> RecvL (m, f, erase_upd x k)
  | SendL (m, f, k) -> SendL (m, f, erase_upd x k)
  | ChoiceL (f, ks) -> ChoiceL (f, List.map ~f:(erase_upd x) ks)
  | TVarL (v, e) -> TVarL (v, e)
  | MuL (v, vs, k) -> MuL (v, vs, erase_upd x k)
  | EndL -> EndL
  | InviteCreateL (irs, nrs, proto, k) ->
      InviteCreateL (irs, nrs, proto, erase_upd x k)
  | AcceptL (who, proto, irs, nrs, from, k) ->
      AcceptL (who, proto, irs, nrs, from, erase_upd x k)
  | CallL (who, proto, irs, nrs, k) ->
      CallL (who, proto, irs, nrs, erase_upd x k)
  | ICallL (who, proto, irs, nrs, k) ->
      ICallL (who, proto, irs, nrs, erase_upd x k)
  | CombineL (l, r) -> (
    match l with
    | TVarL (v', _) ->
        if TypeVariableName.equal v' x then l else CombineL (l, r)
    | _ -> CombineL (l, r) )
  | SilentL (v, e, k) -> SilentL (v, e, erase_upd x k)

let rec is_free v = function
  | RecvL (_, _, k)
   |SendL (_, _, k)
   |InviteCreateL (_, _, _, k)
   |CallL (_, _, _, _, k)
   |ICallL (_, _, _, _, k)
   |AcceptL (_, _, _, _, _, k)
   |SilentL (_, _, k) ->
      is_free v k
  | ChoiceL (_, ks) ->
      List.fold_left ~f:(fun a b -> a || is_free v b) ~init:false ks
  | TVarL (v2, _) -> TypeVariableName.equal v v2
  | EndL -> false
  | MuL (v2, _, k) ->
      if TypeVariableName.equal v v2 then false else is_free v k
  | CombineL (l, r) -> is_free v l || is_free v r

let rec is_updatable v = function
  | RecvL (_, _, k)
   |SendL (_, _, k)
   |InviteCreateL (_, _, _, k)
   |CallL (_, _, _, _, k)
   |ICallL (_, _, _, _, k)
   |AcceptL (_, _, _, _, _, k)
   |SilentL (_, _, k) ->
      is_updatable v k
  | ChoiceL (_, ks) ->
      List.fold_left ~f:(fun a b -> a || is_updatable v b) ~init:false ks
  | TVarL (_, _) | EndL -> false
  | MuL (v2, _, k) ->
      if TypeVariableName.equal v v2 then false else is_updatable v k
  | CombineL (l, _) -> is_free v l

let rec lookup_var v = function
  | [] -> None
  | (v', k) :: ks ->
      if TypeVariableName.equal v v' then Some k else lookup_var v ks

let rec ends_with v = function
  | AcceptL (_, _, _, _, _, k)
   |CallL (_, _, _, _, k)
   |ICallL (_, _, _, _, k)
   |SilentL (_, _, k)
   |InviteCreateL (_, _, _, k)
   |RecvL (_, _, k)
   |SendL (_, _, k) ->
      ends_with v k
  | ChoiceL (_, ks) ->
      List.fold_left ~f:(fun a b -> a && ends_with v b) ~init:true ks
  | TVarL (v', _) -> (
    match v with Some vv -> TypeVariableName.equal v' vv | None -> false )
  | MuL (v', _, k) -> (
    match v with
    (* FIXME: list of vars *)
    | None -> ends_with None k
    | Some vv ->
        if TypeVariableName.equal vv v' then false else ends_with v k )
  | CombineL (l, _) -> ends_with v l
  | EndL -> ( match v with None -> true | _ -> false )

let rec combine_branch v l r =
  match l with
  | RecvL (m, f, k) -> RecvL (m, f, combine_branch v k r)
  | SendL (m, f, k) -> SendL (m, f, combine_branch v k r)
  | ChoiceL (f, ks) ->
      ChoiceL (f, List.map ~f:(fun k -> combine_branch v k r) ks)
  | TVarL (vv, e) ->
      if TypeVariableName.equal v vv then
        if ends_with (Some v) r then r
        else Err.uerr (Err.Uncategorised "cannot combine branches")
      else TVarL (vv, e)
  | MuL _ | ICallL _ | CombineL _ ->
      Err.uerr
        (Err.Uncategorised
           "in combine_branch: \n\
           \        combining recursion/inlined calls/combine" )
  | EndL ->
      if ends_with None r then r
      else Err.uerr (Err.Uncategorised "cannot combine branches")
  | InviteCreateL (irs, nrs, proto, k) ->
      InviteCreateL (irs, nrs, proto, combine_branch v k r)
  | AcceptL (who, proto, irs, nrs, from, k) ->
      AcceptL (who, proto, irs, nrs, from, combine_branch v k r)
  | CallL (who, proto, irs, nrs, k) ->
      CallL (who, proto, irs, nrs, combine_branch v k r)
  | SilentL (vs, e, k) -> SilentL (vs, e, combine_branch v k r)

let rec insert_prefixes l r =
  match r with
  | RecvL (m, f, k) -> RecvL (m, f, insert_prefixes l k)
  | SendL (m, f, k) -> SendL (m, f, insert_prefixes l k)
  | ChoiceL (f, ks) ->
      ChoiceL (f, List.map ~f:(fun k -> insert_prefixes l k) ks)
  | TVarL _ | EndL -> l
  | MuL _ | CombineL _ ->
      Err.uerr
        (Err.Uncategorised
           "in combine_branch: \n\
           \        combining recursion/inlined calls/combine" )
  | InviteCreateL (irs, nrs, proto, k) ->
      InviteCreateL (irs, nrs, proto, insert_prefixes l k)
  | AcceptL (who, proto, irs, nrs, from, k) ->
      AcceptL (who, proto, irs, nrs, from, insert_prefixes l k)
  | CallL (who, proto, irs, nrs, k) ->
      (* FIXME *)
      ICallL (who, proto, irs, nrs, insert_prefixes l k)
  | ICallL (who, proto, irs, nrs, k) ->
      (* FIXME *)
      ICallL (who, proto, irs, nrs, insert_prefixes l k)
  | SilentL (vs, e, k) -> SilentL (vs, e, insert_prefixes l k)

let rec redo_inline_call l r =
  match l with
  | RecvL (m, f, k) -> RecvL (m, f, redo_inline_call k r)
  | SendL (m, f, k) -> SendL (m, f, redo_inline_call k r)
  | ChoiceL (f, ks) ->
      ChoiceL (f, List.map ~f:(fun k -> redo_inline_call k r) ks)
  | TVarL _ | EndL -> insert_prefixes l r
  | MuL _ | CombineL _ ->
      Err.uerr
        (Err.Uncategorised
           "in redo_inline_call: \n\
           \        combining recursion/inlined calls/combine" )
  | InviteCreateL (irs, nrs, proto, k) ->
      InviteCreateL (irs, nrs, proto, redo_inline_call k r)
  | AcceptL (who, proto, irs, nrs, from, k) ->
      AcceptL (who, proto, irs, nrs, from, redo_inline_call k r)
  | CallL (who, proto, irs, nrs, k) ->
      CallL (who, proto, irs, nrs, redo_inline_call k r)
  | ICallL (who, proto, irs, nrs, k) ->
      ICallL (who, proto, irs, nrs, redo_inline_call k r)
  | SilentL (vs, e, k) -> SilentL (vs, e, redo_inline_call k r)

let combine_seq wrap v l r =
  match l with
  | TVarL (vv, _) ->
      if TypeVariableName.equal vv v then wrap r
      else combine_branch v l (wrap r)
  | ICallL (_, _, _, _, TVarL (vv, _)) ->
      if TypeVariableName.equal vv v then wrap r
      else combine_branch v l (wrap r)
  | _ -> combine_branch v l (wrap r)

let rec combine_branches wrap v l r =
  match (l, r) with
  | [], [] -> []
  | h :: t, r :: s -> combine_seq wrap v h r :: combine_branches wrap v t s
  | _, _ ->
      Err.uerr (Err.Uncategorised "combining distinct number of branches")

let combine_choices wrap v l r =
  match (l, r) with
  | ChoiceL (f1, k1), ChoiceL (f2, k2) ->
      if RoleName.equal f1 f2 then ChoiceL (f1, combine_branches wrap v k1 k2)
      else Err.uerr (Err.Uncategorised "Combining distinct choices")
  | _, _ -> combine_seq wrap v l r

let rec do_combine env (ty : t) = function
  | RecvL (m, f, k) -> RecvL (m, f, do_combine env ty k)
  | SendL (m, f, k) -> SendL (m, f, do_combine env ty k)
  | ChoiceL _ | TVarL _ | MuL _ | ICallL _ | CombineL _ ->
      Err.unimpl ~here:[%here]
        "Only send/recv/invite/accept allowed in RHS of updatable rec"
  | EndL -> ty
  | InviteCreateL (irs, nrs, proto, k) ->
      InviteCreateL (irs, nrs, proto, do_combine env ty k)
  | AcceptL (who, proto, irs, nrs, from, k) ->
      AcceptL (who, proto, irs, nrs, from, do_combine env ty k)
  | CallL (who, proto, irs, nrs, k) -> (
    match k with
    | EndL -> (
        let local_protocol_id = LocalProtocolId.create proto who in
        let decl = Map.find_exn env local_protocol_id in
        let body = unfold_upd env [] (snd decl) in
        (*
         * let body = snd decl in 
         *)
        let wrap k = ICallL (who, proto, irs, nrs, k) in
        match (ty, body) with
        | MuL (v1, vs, k1), MuL (v2, _, k2) ->
            if TypeVariableName.equal v1 v2 then
              MuL (v1, vs, combine_choices wrap v1 k1 k2)
            else
              Err.uerr
                (Err.Uncategorised
                   "combining distinct recursion variables is not possible"
                )
        (* FIXME: hack!! rework combining recursive protocols with
         * recursive protocol calls
         * *)
        | MuL (v1, vs, k1), _ ->
            let wrap k = ICallL (who, proto, irs, nrs, k) in
            MuL (v1, vs, redo_inline_call k1 (wrap body))
        | _, _ ->
            Err.uerr
              (Err.Uncategorised
                 ( "combining distinct non-recursive protocols\n" ^ show ty
                 ^ "\n"
                 ^ show (wrap body) ) ) )
    | _ ->
        Err.unimpl ~here:[%here]
          "Protocol call must be last in updatable recursion" )
  | SilentL (v, e, k) -> SilentL (v, e, do_combine env ty k)

and unfold_upd env x = function
  | RecvL (m, f, k) -> RecvL (m, f, unfold_upd env x k)
  | SendL (m, f, k) -> SendL (m, f, unfold_upd env x k)
  | ChoiceL (f, ks) ->
      let res = List.map ~f:(unfold_upd env x) ks in
      ChoiceL (f, res)
  | TVarL (v, e) -> (
    match lookup_var v x with Some k -> k | None -> TVarL (v, e) )
  | MuL (v, vs, k) ->
      if is_updatable v k then
        match k with
        | CombineL (TVarL (_, _), r) | CombineL (EndL, r) ->
            unfold_upd env x r
        | _ -> unfold_upd env ((v, MuL (v, vs, erase_upd v k)) :: x) k
      else
        let res = unfold_upd env x k in
        MuL (v, vs, res)
  | EndL -> EndL
  | InviteCreateL (irs, nrs, proto, k) ->
      let res = unfold_upd env x k in
      InviteCreateL (irs, nrs, proto, res)
  | AcceptL (who, proto, irs, nrs, from, k) ->
      let res = unfold_upd env x k in
      AcceptL (who, proto, irs, nrs, from, res)
  | CallL (who, proto, irs, nrs, k) ->
      let res = unfold_upd env x k in
      CallL (who, proto, irs, nrs, res)
  | ICallL (who, proto, irs, nrs, k) ->
      let res = unfold_upd env x k in
      ICallL (who, proto, irs, nrs, res)
  | CombineL (l, r) ->
      let l' = unfold_upd env x l in
      do_combine env l' r
  | SilentL (v, e, k) ->
      let res = unfold_upd env x k in
      SilentL (v, e, res)

let unfold_decl env (lty : RoleName.t list * t) =
  let res = unfold_upd env [] (snd lty) in
  (fst lty, res)

let rec cleanup_upd x = function
  | RecvL (m, f, k) -> RecvL (m, f, cleanup_upd x k)
  | SendL (m, f, k) -> SendL (m, f, cleanup_upd x k)
  | ChoiceL (f, ks) -> ChoiceL (f, List.map ~f:(cleanup_upd x) ks)
  | TVarL (v, e) -> TVarL (v, e)
  | MuL (v, vs, k) -> MuL (v, vs, cleanup_upd x k)
  | EndL -> EndL
  | InviteCreateL (irs, nrs, proto, k) ->
      InviteCreateL (irs, nrs, proto, cleanup_upd x k)
  | AcceptL (who, proto, irs, nrs, from, k) ->
      AcceptL (who, proto, irs, nrs, from, cleanup_upd x k)
  | CallL (who, proto, irs, nrs, k) ->
      CallL (who, proto, irs, nrs, cleanup_upd x k)
  | ICallL (who, proto, irs, nrs, k) ->
      ICallL (who, proto, irs, nrs, cleanup_upd x k)
  | CombineL (l, r) -> ( match r with EndL -> l | _ -> CombineL (l, r) )
  | SilentL (v, e, k) -> SilentL (v, e, cleanup_upd x k)

let cleanup_combine (lty : RoleName.t list * t) =
  (fst lty, cleanup_upd [] (snd lty))

let unfold_combine (ltype : nested_t) =
  let ltype = Map.map ltype ~f:cleanup_combine in
  let ltype = Map.map ltype ~f:(unfold_decl ltype) in
  ltype
