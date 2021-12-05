open! Base
open Printf
open Gtype
open Err
open Names
open Syntaxtree

type t =
  | RecvL of Gtype.message * RoleName.t * t
  | SendL of Gtype.message * RoleName.t * t
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
  | StateL of StateNameSet.t * (t Lazy.t[@equal Base.phys_equal])
  | EpsilonL of t list
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

let roles_to_string roles =
  let str_roles = List.map ~f:RoleName.user roles in
  String.concat ~sep:", " str_roles

let show =
  let indent_here indent = String.make (indent * 2) ' ' in
  let rec show_nested_type_internal indent =
    let current_indent = indent_here indent in
    function
    | RecvL (m, r, l) ->
        sprintf "%s%s from %s;\n%s" current_indent (show_message m)
          (RoleName.user r)
          (show_nested_type_internal indent l)
    | SendL (m, r, l) ->
        sprintf "%s%s to %s;\n%s" current_indent (show_message m)
          (RoleName.user r)
          (show_nested_type_internal indent l)
    | MuL (n, rec_vars, l) ->
        let rec_vars_s =
          if List.is_empty rec_vars then ""
          else
            "["
            ^ String.concat ~sep:", "
                (List.map
                   ~f:(fun (is_silent, rv) ->
                     let prefix = if is_silent then "(silent) " else "" in
                     prefix ^ Gtype.show_rec_var rv )
                   rec_vars )
            ^ "] "
        in
        sprintf "%srec %s %s{\n%s%s}\n" current_indent
          (TypeVariableName.user n) rec_vars_s
          (show_nested_type_internal (indent + 1) l)
          current_indent
    | TVarL (n, rec_exprs) ->
        let rec_exprs_s =
          if List.is_empty rec_exprs then ""
          else
            " ["
            ^ String.concat ~sep:", " (List.map ~f:Expr.show rec_exprs)
            ^ "]"
        in
        sprintf "%scontinue %s%s;\n" current_indent (TypeVariableName.user n)
          rec_exprs_s
    | EndL -> sprintf "%send\n" current_indent
    | ChoiceL (r, ls) ->
        let pre =
          sprintf "%schoice at %s {\n" current_indent (RoleName.user r)
        in
        let intermission = sprintf "%s} or {\n" current_indent in
        let post = sprintf "%s}\n" current_indent in
        let choices =
          List.map ~f:(show_nested_type_internal (indent + 1)) ls
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
        let l_str = show_nested_type_internal indent l in
        invite ^ create ^ l_str
    | AcceptL (role, protocol, roles, new_roles, caller, l) ->
        let roles_str = List.map ~f:RoleName.user roles in
        let new_roles_str = List.map ~f:RoleName.user new_roles in
        sprintf "%saccept %s@%s(%s) from %s;\n%s" current_indent
          (RoleName.user role)
          (ProtocolName.user protocol)
          (Symtable.show_roles (roles_str, new_roles_str))
          (RoleName.user caller)
          (show_nested_type_internal indent l)
    | SilentL (var, ty, l) ->
        sprintf "%s(silent) %s(%s);\n%s" current_indent
          (VariableName.user var)
          (Expr.show_payload_type ty)
          (show_nested_type_internal indent l)
    | EpsilonL ls ->
        let pre = sprintf "%s(nondet) {\n" current_indent in
        let intermission = sprintf "%s} or {\n" current_indent in
        let post = sprintf "%s}\n" current_indent in
        let choices =
          List.map ~f:(show_nested_type_internal (indent + 1)) ls
        in
        let ls = String.concat ~sep:intermission choices in
        pre ^ ls ^ post
    | StateL (n, l) ->
        sprintf "%s(recState) %s {\n%s%s}\n" current_indent
          (StateNameSet.user n)
          (show_nested_type_internal (indent + 1) (Lazy.force l))
          current_indent
  in
  show_nested_type_internal 0

let show_nested_t (nested_t : nested_t) =
  let show_local_protocol ((protocol, role), (roles, ltype)) =
    let roles_str = List.map ~f:RoleName.user roles in
    sprintf "%s@%s(%s) {\n\n%s\n}" (RoleName.user role)
      (ProtocolName.user protocol)
      (Symtable.show_roles (roles_str, []))
      (show ltype)
  in
  Map.to_alist nested_t
  |> List.map ~f:show_local_protocol
  |> String.concat ~sep:"\n\n"

type local_proto_name_lookup = LocalProtocolName.t Map.M(LocalProtocolId).t

let build_local_proto_name_lookup (nested_t : nested_t) :
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
                violation "Merge receive must be merging receive local types"
            )
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
                violation "Merge receive must be merging receive local types"
            )
        | _ -> violation "Merge receive must be merging receive local types"
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
      violation
        ( "Normalised global type always has a message in choice branches\n"
        ^ Gtype.show g )

(* project a 'nondeterministic' local type in a straightforward manner. All
   SendL/RecvL's are prefixed with fresh state identifier (StateL). Mergings
   are deferred using EpsilonL constructor, and branches in internal choices
   could have MuL's and TVarL's. *)
let rec project_nondet (projected_role : RoleName.t) (g : Gtype.t) =
  let state l = StateL (StateNameSet.fresh_singleton (), l) in
  match g with
  | MessageG (m, send_r, recv_r, g_type) ->
      if RoleName.equal projected_role send_r then
        state
        @@ Lazy.from_val
             (SendL (m, recv_r, project_nondet projected_role g_type))
      else if RoleName.equal projected_role recv_r then
        state
        @@ Lazy.from_val
             (RecvL (m, send_r, project_nondet projected_role g_type))
      else project_nondet projected_role g_type
  | ChoiceG (choice_r, g_types) ->
      if RoleName.equal projected_role choice_r then
        (* internal choice *)
        ChoiceL
          (choice_r, List.map ~f:(project_nondet projected_role) g_types)
      else
        (* non-deterministic choices -- defer merging using EpsilonL *)
        EpsilonL (List.map ~f:(project_nondet projected_role) g_types)
  | EndG -> EndL
  | MuG (name, _, g_type) -> (
    match project_nondet projected_role g_type with
    | TVarL (name', _) when TypeVariableName.equal name name' -> EndL
    | EndL -> EndL
    | lType -> MuL (name, [], lType) )
  | TVarG (name, _, _) -> TVarL (name, [])
  | CallG (_, _, _, _) -> (* TODO *) assert false

let rec unfold (tv, body) (l : t) =
  match l with
  | RecvL (m, send_r, cont) -> RecvL (m, send_r, unfold (tv, body) cont)
  | SendL (m, recv_r, cont) -> SendL (m, recv_r, unfold (tv, body) cont)
  | ChoiceL (choice_r, conts) ->
      ChoiceL (choice_r, List.map ~f:(unfold (tv, body)) conts)
  | TVarL (tv', _) when TypeVariableName.equal tv tv' -> MuL (tv, [], body)
  | TVarL (_, _) -> l
  | MuL (tv, rec_var, cont) -> MuL (tv, rec_var, unfold (tv, body) cont)
  | EndL -> l
  | StateL (state_id, cont) ->
      StateL (state_id, lazy (unfold (tv, body) (Lazy.force cont)))
  | EpsilonL conts -> EpsilonL (List.map ~f:(unfold (tv, body)) conts)
  | InviteCreateL (_, _, _, _) -> assert false
  | AcceptL (_, _, _, _, _, _) -> assert false
  | SilentL (_, _, _) -> assert false

let end_state = StateNameSet.singleton_of_string "state_end"

(* Compute the powerset state by removing epsilons and expanding mu's.

   Returns Either.First [(state_id0, SendL/RecvL/ChoiceL); (state_id1, ...);
   ...] if the state is successfully computed.

   Otherwise, Either.Second [tvar0; tvar1; ...] denotes a 'backward' epsilon
   transition which could be eliminated at the caller's site.

   Parameters: (1) ~visited records visited recursion variables and detects
   backward epsilon links. (2) ~binding tracks mu bindings outside the
   type. *)
let rec compute_state ~visited ~binding (l : t) =
  let is_visited tv = List.mem ~equal:TypeVariableName.equal visited tv in
  match l with
  | MuL (tv, _, _) when is_visited tv ->
      (* a backward epsilon link is found as a result of unfolding (see
         next). *)
      Either.Second [tv]
  | MuL (tv, _, cont) -> (
      (* visited a recursion variable for the first time. unfold it. *)
      let cont = unfold (tv, cont) cont in
      match compute_state ~visited:(tv :: visited) ~binding cont with
      | Either.Second tvars ->
          (* only backward epsilon links from here *)
          let tvars =
            (* filter out transitions pointing to this state *)
            List.filter
              ~f:(fun tv' -> not @@ TypeVariableName.equal tv tv')
              tvars
          in
          if List.length tvars > 0 then
            (* if some backward links remain, return them *)
            Either.Second tvars
          else
            (* all links points to myself -- this must be an `end` state! *)
            Either.First [(end_state, EndL)]
      | sts -> sts )
  | TVarL (tv, _) when is_visited tv ->
      (* another form of a backward epsilon transition. This 'bare' recursion
         variable occurs when a mu is stripped off in `determinise`. *)
      Either.Second [tv]
  | TVarL (tv, _) ->
      (* just expand it if it is for the first time. *)
      let t = Map.find_exn binding tv in
      compute_state ~visited:(tv :: visited) ~binding t
  | EndL -> Either.First [(end_state, EndL)]
  | StateL (state_id, l) -> (
    match Lazy.force l with
    | RecvL (m, send_r, cont) ->
        Either.First [(state_id, RecvL (m, send_r, cont))]
    | SendL (m, recv_r, cont) ->
        Either.First [(state_id, SendL (m, recv_r, cont))]
    | _ ->
        (* impossible: only send/recv shuold be annotated with state names *)
        assert false )
  | ChoiceL (choice_r, conts) ->
      (* internal choice. all branches must be distinct; hence a non-epsilon
         loop to this state is prohibited. *)
      let states, vars =
        List.partition_map conts ~f:(compute_state ~visited ~binding)
      in
      let vars = List.concat vars in
      if List.length vars > 0 then
        (* epsilon loops found *)
        violation "internal choice expands infinitely"
      else
        let states = List.concat states in
        let state_ids, conts = List.unzip states in
        (* FIXME check conts are deterministic *)
        Either.First
          [(StateNameSet.union_list state_ids, ChoiceL (choice_r, conts))]
  | EpsilonL conts ->
      (* epsilon transitions -- compute non-epsilon transitions *)
      let states, vars =
        List.partition_map conts ~f:(compute_state ~visited ~binding)
      in
      let states = List.concat states in
      if List.length states > 0 then
        (* just return concrete transitions -- the backward epsilon links can
           safely be ignored *)
        Either.First states
      else
        (* no concrete transitions -- return all target states (variables).
           they are later filtered (and possibly replaced with EndL's) at
           binding site *)
        Either.Second (List.concat vars)
  | RecvL (_, _, _) | SendL (_, _, _) ->
      (* impossible: RecvL and SendL are always prefixd by StateL *)
      assert false
  | SilentL (_, _, _) -> (* TODO *) assert false
  | InviteCreateL (_, _, _, _) -> (* TODO *) assert false
  | AcceptL (_, _, _, _, _, _) -> (* TODO *) assert false

(* Deferred merging. it computes merging for the first actions, and for
   overlapping input labels, merging is deferred using EpsilonL *)
let merge_deferred projected_role types =
  let rec aux (acc : (LabelName.t * t) list) = function
    | RecvL (m, _, lty) as l -> (
        let {label; _} = m in
        match List.Assoc.find acc ~equal:LabelName.equal label with
        | None -> (label, l) :: acc
        | Some (RecvL (m_, r, l_))
          when List.equal equal_payload m.Gtype.payload m_.Gtype.payload ->
            (* input labels overlap. defer merging of the continuation using
               EpsilonL *)
            List.Assoc.add acc ~equal:LabelName.equal label
              (RecvL (m, r, EpsilonL [lty; l_]))
        | Some (RecvL _) -> violation "Payload type mismatch"
        | _ -> violation "Merge receive must be merging receive local types"
        )
    | _ -> violation "Merge receive must be merging receive local types"
  in
  match types with
  | (RecvL (_, sender_r, _) :: _ | ChoiceL (sender_r, _) :: _)
    when not @@ RoleName.equal projected_role sender_r -> (
      let recvs =
        List.concat_map
          ~f:(function
            | RecvL (_, sender_r, _) as l -> [(sender_r, l)]
            | ChoiceL (sender_r, ls)
              when not @@ RoleName.equal projected_role sender_r ->
                List.map ~f:(fun l -> (sender_r, l)) ls
            | t ->
                violation @@ "Merge should be receive local types: " ^ show t
            )
          types
      in
      let senders, recvs = List.unzip recvs in
      let sender =
        match senders with
        | [r] -> r
        | r :: rs when List.for_all ~f:(RoleName.equal r) rs -> r
        | _ -> violation "Merge sender must be identical"
      in
      let conts = List.fold ~f:aux ~init:[] recvs in
      match conts with
      | [] -> EndL
      | [(_, lty)] -> lty
      | conts -> ChoiceL (sender, List.map ~f:snd conts) )
  | EndL :: ls when List.for_all ls ~f:(equal EndL) -> EndL
  | [(SendL (_, _, _) as l)] -> l
  | [(ChoiceL (sender_r, _) as l)]
    when RoleName.equal projected_role sender_r ->
      l
  | ts ->
      violation @@ "Can't merge. projected role:"
      ^ RoleName.user projected_role
      ^ " protocol: \n"
      ^ String.concat ~sep:"\n\n and \n"
      @@ List.map ~f:show ts

(* unfold all mu's at the head *)
let rec unwind ?(bound = Set.empty (module TypeVariableName)) (l : t) =
  match l with
  | MuL (tv, _, cont) ->
      let binding, cont = unwind ~bound:(Set.add bound tv) cont in
      ((tv, cont) :: binding, cont)
  | TVarL (tv, _) when Set.mem bound tv -> ([], EndL)
  | l -> ([], l)

(* determinise the non-deterministic local types using powerset construction.

   the returned RecvL/SendL/ChoiceL are prefixed by state id (StateL).

   to avoid indefinite expansion, calculation of the next state is deferred
   using StateL constructor. the loop is resolved using additional mu binders
   introduced by `remove_states`. *)
let rec determinise projected_role binding0 (l : t) =
  (* peeling off mu's *)
  let binding, l = unwind l in
  (* and record them as visited *)
  let visited = List.rev_map ~f:fst binding in
  let binding =
    List.fold_left
      ~f:(fun binding (key, data) -> Map.add_exn binding ~key ~data)
      ~init:binding0 binding
  in
  (* compute the current states *)
  let states =
    match compute_state ~visited ~binding l with
    | Either.First states -> states
    | Either.Second _ -> [(end_state, EndL)]
  in
  let state_ids, typs = List.unzip states in
  let new_state_id =
    (* powerset construction *)
    StateNameSet.union_list state_ids
  in
  let merged =
    lazy
      ( determinise_following projected_role binding
      @@ merge_deferred projected_role typs )
  in
  let t =
    (* prefix it by the powerset state id *)
    StateL (new_state_id, merged)
  in
  (* and wrap it by the mu binders which we peeled off at the beginning *)
  List.fold_left ~f:(fun t tv -> MuL (tv, [], t)) ~init:t visited

and determinise_following projected_role binding (l : t) =
  let next = function
    | RecvL (m, send_r, cont) ->
        RecvL (m, send_r, determinise projected_role binding cont)
    | SendL (m, recv_r, cont) ->
        SendL (m, recv_r, determinise projected_role binding cont)
    | _ -> (* TODO *) assert false
  in
  match l with
  | TVarL (tvar, _) ->
      determinise projected_role binding (Map.find_exn binding tvar)
  | EndL -> EndL
  | (RecvL (_, _, _) | SendL (_, _, _)) as l -> next l
  | ChoiceL (r, conts) -> ChoiceL (r, List.map ~f:next conts)
  | _ -> assert false

type state_env =
  { visited: Set.M(StateNameSet).t
  ; current_var: TypeVariableName.t option
  ; rename_var: TypeVariableName.t Map.M(TypeVariableName).t
  ; state_to_var: TypeVariableName.t Map.M(StateNameSet).t }

let new_state_env =
  { visited= Set.empty (module StateNameSet)
  ; current_var= None
  ; rename_var= Map.empty (module TypeVariableName)
  ; state_to_var= Map.empty (module StateNameSet) }

(* remove StateL constructors by detecting loops. returns the used (powerset)
   state id in the first component of the pair *)
let rec remove_states env (l : t) =
  let empty = Set.empty (module StateNameSet) in
  let next ?(nextenv = {env with current_var= None}) f t =
    let used, t = remove_states nextenv t in
    (used, f t)
  in
  let make_tvar state_ids =
    let tvar =
      String.concat ~sep:"_"
        (List.map ~f:StateName.user @@ Set.to_list state_ids)
    in
    TypeVariableName.create tvar Loc.ghost_loc
  in
  match l with
  | RecvL (m, send_r, cont) -> next (fun t -> RecvL (m, send_r, t)) cont
  | SendL (m, recv_r, cont) -> next (fun t -> SendL (m, recv_r, t)) cont
  | ChoiceL (choose_r, conts) ->
      let us, conts =
        List.unzip
        @@ List.map ~f:(remove_states {env with current_var= None}) conts
      in
      let used = Set.union_list (module StateNameSet) us in
      (used, ChoiceL (choose_r, conts))
  | MuL (tvar, rec_vars, cont) when Option.is_none env.current_var ->
      (* associate the recursion variable with the powerset state *)
      let nextenv = {env with current_var= Some tvar} in
      next ~nextenv (fun t -> MuL (tvar, rec_vars, t)) cont
  | MuL (tvar, rec_vars, cont) ->
      (* if mu's are nested, rename the inner ones *)
      let nextenv =
        { env with
          rename_var=
            Map.add_exn env.rename_var ~key:tvar
              ~data:(Option.value_exn env.current_var) }
      in
      next ~nextenv (fun t -> MuL (tvar, rec_vars, t)) cont
  | TVarL (tvar, expr) when Map.mem env.rename_var tvar ->
      (* rename variables bound by the inner binding *)
      let tvar = Map.find_exn env.rename_var tvar in
      (empty, TVarL (tvar, expr))
  | TVarL (tvar, expr) -> (empty, TVarL (tvar, expr))
  | EndL -> (empty, EndL)
  | StateL (state_ids, _) when Set.mem env.visited state_ids ->
      (* already visited; put typevar here *)
      if Map.mem env.state_to_var state_ids then
        (empty, TVarL (Map.find_exn env.state_to_var state_ids, []))
      else
        ( Set.singleton (module StateNameSet) state_ids
        , TVarL (make_tvar state_ids, []) )
  | StateL (state_ids, cont) when Option.is_none env.current_var ->
      (* first time, and the state is not bound *)
      let visited = Set.add env.visited state_ids in
      let used, cont = remove_states {env with visited} (Lazy.force cont) in
      if Set.mem used state_ids then
        (Set.remove used state_ids, MuL (make_tvar state_ids, [], cont))
      else (Set.remove used state_ids, cont)
  | StateL (tvars, cont) ->
      (* first time, but state is already bound by a mu *)
      let visited = Set.add env.visited tvars in
      let state_to_var =
        Map.add_exn env.state_to_var ~key:tvars
          ~data:(Option.value_exn env.current_var)
      in
      let used, cont =
        remove_states {env with state_to_var; visited} (Lazy.force cont)
      in
      (Set.remove used tvars, cont)
  | EpsilonL ls ->
      violation @@ "unexpected epsilon:\n"
      ^ String.concat ~sep:"\nand\n"
      @@ List.map ~f:show ls
  | InviteCreateL (_, _, _, _)
   |AcceptL (_, _, _, _, _, _)
   |SilentL (_, _, _) ->
      assert false

let rec project projected_role g =
  let l = project_nondet projected_role g in
  let l =
    determinise projected_role (Map.empty (module TypeVariableName)) l
  in
  let _, l = remove_states new_state_env l in
  l

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
      let env =
        { env with
          rvenv= Map.add_exn ~key:name ~data:rec_exprs rvenv
        ; silent_vars
        ; unguarded_tv= Set.add unguarded_tv name }
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
              violation
                "Normalised global type always has a message in choice \
                 branches"
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
      let l_types = List.map ~f:(project' env projected_role) g_types in
      match projected_role with
      | _
        when RoleName.equal projected_role choice_r
             || RoleName.equal projected_role recv_r -> (
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
      let gen_invitecreatel next =
        InviteCreateL (roles, dynamic_roles, protocol, next)
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

let _project projected_role g = project' new_project_env projected_role g

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
  let rec rename_tvars tvar_mapping namegen = function
    | RecvL (msg, sender, l) ->
        let namegen, l = rename_tvars tvar_mapping namegen l in
        (namegen, RecvL (msg, sender, l))
    | SendL (msg, recv, l) ->
        let namegen, l = rename_tvars tvar_mapping namegen l in
        (namegen, SendL (msg, recv, l))
    | MuL (tvar, rec_vars, l) ->
        let namegen, new_tvar_str =
          Namegen.unique_name namegen (TypeVariableName.user tvar)
        in
        let new_tvar = TypeVariableName.of_string @@ new_tvar_str in
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
    | SilentL _ -> Err.unimpl "renaming recursive variables with refinements"
    | EpsilonL ls ->
        let namegen, ls =
          List.fold_map ls ~init:namegen ~f:(rename_tvars tvar_mapping)
        in
        (namegen, EpsilonL ls)
    | StateL (_, _) ->
        Err.unimpl
          "renaming recursive variables with intermediate state annotations"
  in
  let namegen = Namegen.create () in
  let _, ltype =
    rename_tvars (Map.empty (module TypeVariableName)) namegen ltype
  in
  ltype

let ensure_unique_tvars nested_t : nested_t =
  Map.map nested_t ~f:(fun (roles, ltype) ->
      (roles, make_unique_tvars ltype) )
