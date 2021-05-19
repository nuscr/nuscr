open! Base
open Printf
open Loc
open Err
open Names
open Syntaxtree

type payload =
  | PValue of VariableName.t option * Expr.payload_type
  | PDelegate of ProtocolName.t * RoleName.t
[@@deriving sexp_of]

(* Ignoring variable names for now *)
let equal_payload p1 p2 =
  match (p1, p2) with
  | PValue (_, n1), PValue (_, n2) -> Expr.equal_payload_type n1 n2
  | PDelegate (pn1, rn1), PDelegate (pn2, rn2) ->
      ProtocolName.equal pn1 pn2 && RoleName.equal rn1 rn2
  | _, _ -> false

let equal_pvalue_payload p1 p2 =
  let var_name_equal = Option.equal VariableName.equal in
  match (p1, p2) with
  | PValue (v1, n1), PValue (v2, n2) ->
      var_name_equal v1 v2 && Expr.equal_payload_type n1 n2
  | _ -> equal_payload p1 p2

let compare_payload p1 p2 =
  match (p1, p2) with
  | PValue (_, ptn1), PValue (_, ptn2) -> Expr.compare_payload_type ptn1 ptn2
  | PValue _, PDelegate _ -> -1
  | PDelegate _, PValue _ -> 1
  | PDelegate (pn1, rn1), PDelegate (pn2, rn2) ->
      let comp_fst = ProtocolName.compare pn1 pn2 in
      if comp_fst = 0 then RoleName.compare rn1 rn2 else comp_fst

let show_payload = function
  | PValue (var, ty) ->
      let var =
        match var with
        | Some var -> VariableName.user var ^ ": "
        | None -> ""
      in
      sprintf "%s%s" var (Expr.show_payload_type ty)
  | PDelegate (proto, role) ->
      sprintf "%s @ %s" (ProtocolName.user proto) (RoleName.user role)

let pp_payload fmt p = Caml.Format.fprintf fmt "%s" (show_payload p)

let parse_typename name =
  match Name.Name.user name with
  | "int" -> Expr.PTInt
  | "string" -> Expr.PTString
  | "bool" -> Expr.PTBool
  | "unit" -> Expr.PTUnit
  | _ -> Expr.PTAbstract (PayloadTypeName.of_name name)

let of_syntax_payload (payload : Syntax.payloadt) =
  let open Syntax in
  match payload with
  | PayloadName n -> PValue (None, parse_typename n)
  | PayloadDel (p, r) ->
      PDelegate (ProtocolName.of_name p, RoleName.of_name r)
  | PayloadBnd (var, n) ->
      PValue (Some (VariableName.of_name var), parse_typename n)
  | PayloadRTy (Simple n) -> PValue (None, parse_typename n)
  | PayloadRTy (Refined (v, t, e)) ->
      if Pragma.refinement_type_enabled () then
        PValue
          ( Some (VariableName.of_name v)
          , Expr.PTRefined
              ( VariableName.of_name v
              , parse_typename t
              , Expr.of_syntax_expr e ) )
      else
        uerr
          (PragmaNotSet
             ( Pragma.show_pragma Pragma.RefinementTypes
             , "Refinement Types require RefinementTypes pramga to be set."
             ) )

let typename_of_payload = function
  | PValue (_, ty) -> Expr.payload_typename_of_payload_type ty
  | PDelegate _ -> Err.unimpl "delegation for code generation"

type message = {label: LabelName.t; payload: payload list}
[@@deriving eq, sexp_of, ord]

let show_message {label; payload} =
  sprintf "%s(%s)" (LabelName.user label)
    (String.concat ~sep:", " (List.map ~f:show_payload payload))

let pp_message fmt m = Caml.Format.fprintf fmt "%s" (show_message m)

let of_syntax_message (message : Syntax.message) =
  let open Syntax in
  match message with
  | Message {name; payload} ->
      { label= LabelName.of_name name
      ; payload= List.map ~f:of_syntax_payload payload }
  | MessageName name -> {label= LabelName.of_name name; payload= []}

type rec_var =
  { rv_name: VariableName.t
  ; rv_roles: RoleName.t list
  ; rv_ty: Expr.payload_type
  ; rv_init_expr: Expr.t }
[@@deriving sexp_of, eq]

let show_rec_var {rv_name; rv_roles; rv_ty; rv_init_expr} =
  sprintf "%s<%s>: %s = %s"
    (VariableName.user rv_name)
    (String.concat ~sep:", " (List.map ~f:RoleName.user rv_roles))
    (Expr.show_payload_type rv_ty)
    (Expr.show rv_init_expr)

type t =
  | MessageG of message * RoleName.t * RoleName.t * t
  | MuG of TypeVariableName.t * rec_var list * t
  | TVarG of TypeVariableName.t * Expr.t list * t Lazy.t
  | ChoiceG of RoleName.t * t list
  | EndG
  | CallG of RoleName.t * ProtocolName.t * RoleName.t list * t
[@@deriving sexp_of]

type global_t =
  ((RoleName.t list * RoleName.t list) * ProtocolName.t list * t)
  Map.M(ProtocolName).t

let show =
  let indent_here indent = String.make (indent * 2) ' ' in
  let rec show_global_type_internal indent =
    let current_indent = indent_here indent in
    function
    | MessageG (m, r1, r2, g) ->
        sprintf "%s%s from %s to %s;\n%s" current_indent (show_message m)
          (RoleName.user r1) (RoleName.user r2)
          (show_global_type_internal indent g)
    | MuG (n, rec_vars, g) ->
        let rec_vars_s =
          if List.is_empty rec_vars then ""
          else
            "["
            ^ String.concat ~sep:", " (List.map ~f:show_rec_var rec_vars)
            ^ "] "
        in
        sprintf "%srec %s %s{\n%s%s}\n" current_indent
          (TypeVariableName.user n) rec_vars_s
          (show_global_type_internal (indent + 1) g)
          current_indent
    | TVarG (n, rec_exprs, _) ->
        let rec_exprs_s =
          if List.is_empty rec_exprs then ""
          else
            " ["
            ^ String.concat ~sep:", " (List.map ~f:Expr.show rec_exprs)
            ^ "]"
        in
        sprintf "%scontinue %s%s;\n" current_indent (TypeVariableName.user n)
          rec_exprs_s
    | EndG -> sprintf "%send\n" current_indent
    | ChoiceG (r, gs) ->
        let pre =
          sprintf "%schoice at %s {\n" current_indent (RoleName.user r)
        in
        let intermission = sprintf "%s} or {\n" current_indent in
        let post = sprintf "%s}\n" current_indent in
        let choices =
          List.map ~f:(show_global_type_internal (indent + 1)) gs
        in
        let gs = String.concat ~sep:intermission choices in
        pre ^ gs ^ post
    | CallG (caller, proto_name, roles, g) ->
        sprintf "%s%s calls %s(%s);\n%s" current_indent
          (RoleName.user caller)
          (ProtocolName.user proto_name)
          (String.concat ~sep:", " (List.map ~f:RoleName.user roles))
          (show_global_type_internal indent g)
  in
  show_global_type_internal 0

let call_label caller protocol roles =
  let str_roles = List.map ~f:RoleName.user roles in
  let roles_str = String.concat ~sep:"," str_roles in
  (* Current label is a bit arbitrary - find better one? *)
  let label_str =
    sprintf "call(%s, %s(%s))" (RoleName.user caller)
      (ProtocolName.user protocol)
      roles_str
  in
  LabelName.create label_str (ProtocolName.where protocol)

let show_global_t (g : global_t) =
  let show_aux ~key ~data acc =
    let (roles, new_roles), nested_protocols, g_ = data in
    let roles_str = List.map ~f:RoleName.user roles in
    let new_roles_str = List.map ~f:RoleName.user new_roles in
    let str_proto_names = List.map ~f:ProtocolName.user nested_protocols in
    let names_str = String.concat ~sep:", " str_proto_names in
    let proto_str =
      sprintf "protocol %s(%s) {\n\nNested Protocols: %s\n\n%s\n}"
        (ProtocolName.user key)
        (Symtable.show_roles (roles_str, new_roles_str))
        (if String.length names_str = 0 then "-" else names_str)
        (show g_)
    in
    proto_str :: acc
  in
  String.concat ~sep:"\n\n" (List.rev (Map.fold ~init:[] ~f:show_aux g))

let rec_var_of_syntax_rec_var rec_var =
  let open Syntax in
  let {var; roles; ty; init} = rec_var in
  let rv_ty =
    match of_syntax_payload ty with
    | PValue (_, ty) -> ty
    | _ -> assert false
  in
  { rv_name= VariableName.of_name var
  ; rv_roles= List.map ~f:RoleName.of_name roles
  ; rv_ty
  ; rv_init_expr= Expr.of_syntax_expr init }

let of_protocol (global_protocol : Syntax.global_protocol) =
  let open Syntax in
  let {Loc.value= {roles; interactions; _}; _} = global_protocol in
  let roles = List.map ~f:RoleName.of_name roles in
  let assert_empty l =
    if not @@ List.is_empty l then unimpl "Non tail-recursive protocol"
  in
  let check_role r =
    if not @@ List.mem roles r ~equal:RoleName.equal then
      uerr @@ UnboundRole r
  in
  let rec conv_interactions free_names lazy_conts
      (interactions : global_interaction list) =
    match interactions with
    | [] -> (EndG, free_names)
    | {value; _} :: rest -> (
      match value with
      | MessageTransfer {message; from_role; to_roles; _} ->
          let from_role = RoleName.of_name from_role in
          let to_roles = List.map ~f:RoleName.of_name to_roles in
          check_role from_role ;
          let init, free_names =
            conv_interactions free_names lazy_conts rest
          in
          let f to_role acc =
            check_role to_role ;
            if RoleName.equal from_role to_role then
              uerr (ReflexiveMessage from_role) ;
            MessageG (of_syntax_message message, from_role, to_role, acc)
          in
          (List.fold_right ~f ~init to_roles, free_names)
      | Recursion (rname, rec_vars, interactions) ->
          let rname = TypeVariableName.of_name rname in
          if Set.mem free_names rname then
            unimpl "Alpha convert recursion names"
          else assert_empty rest ;
          let rec lazy_cont =
            lazy
              (conv_interactions free_names
                 ((rname, lazy_cont) :: lazy_conts)
                 interactions )
          in
          let rec_vars =
            if Pragma.refinement_type_enabled () then
              List.map ~f:rec_var_of_syntax_rec_var rec_vars
            else []
          in
          List.iter
            ~f:(fun {rv_roles; _} -> List.iter ~f:check_role rv_roles)
            rec_vars ;
          let cont, free_names_ = Lazy.force lazy_cont in
          (* Remove degenerate recursion here *)
          if Set.mem free_names_ rname then
            (MuG (rname, rec_vars, cont), Set.remove free_names_ rname)
          else (cont, free_names_)
      | Continue (name, rec_exprs) ->
          let name = TypeVariableName.of_name name in
          let rec_exprs =
            if Pragma.refinement_type_enabled () then
              List.map ~f:Expr.of_syntax_expr rec_exprs
            else []
          in
          let cont =
            lazy
              ( Lazy.force
                  (List.Assoc.find_exn ~equal:TypeVariableName.equal
                     lazy_conts name )
              |> fst )
          in
          assert_empty rest ;
          (TVarG (name, rec_exprs, cont), Set.add free_names name)
      | Choice (role, interactions_list) ->
          let role = RoleName.of_name role in
          assert_empty rest ;
          check_role role ;
          let conts =
            List.map
              ~f:(conv_interactions free_names lazy_conts)
              interactions_list
          in
          ( ChoiceG (role, List.map ~f:fst conts)
          , Set.union_list (module TypeVariableName) (List.map ~f:snd conts)
          )
      | Do (protocol, _, roles, _) ->
          (* Previously this would've been a violation *)
          let fst_role = RoleName.of_name @@ List.hd_exn roles in
          let role_names = List.map ~f:RoleName.of_name roles in
          let cont, free_names =
            conv_interactions free_names lazy_conts rest
          in
          ( CallG (fst_role, ProtocolName.of_name protocol, role_names, cont)
          , free_names )
      | Calls (caller, proto, _, roles, _) ->
          let caller_role = RoleName.of_name caller in
          let role_names = List.map ~f:RoleName.of_name roles in
          let cont, free_names =
            conv_interactions free_names lazy_conts rest
          in
          ( CallG (caller_role, ProtocolName.of_name proto, role_names, cont)
          , free_names ) )
  in
  let gtype, free_names =
    conv_interactions
      (Set.empty (module Names.TypeVariableName))
      [] interactions
  in
  match Set.choose free_names with
  | Some free_name -> uerr (UnboundRecursionName free_name)
  | None -> gtype

let rec flatten = function
  | ChoiceG (role, choices) ->
      let choices = List.map ~f:flatten choices in
      let lift = function
        | ChoiceG (role_, choices_) when RoleName.equal role role_ ->
            choices_
        | ChoiceG (role_, _choices) ->
            uerr (InconsistentNestedChoice (role, role_))
        | g -> [g]
      in
      ChoiceG (role, List.concat_map ~f:lift choices)
  | g -> g

let rec substitute g tvar g_sub =
  match g with
  | TVarG (tvar_, rec_exprs, _) when TypeVariableName.equal tvar tvar_ -> (
    match g_sub with
    | MuG (tvar__, rec_vars, g) ->
        let rec_vars =
          match
            List.map2
              ~f:(fun rec_var rec_expr ->
                {rec_var with rv_init_expr= rec_expr} )
              rec_vars rec_exprs
          with
          | Base.List.Or_unequal_lengths.Ok rec_vars -> rec_vars
          | _ -> unimpl "Error in substitution"
        in
        MuG (tvar__, rec_vars, g)
    | g_sub -> g_sub )
  | TVarG _ -> g
  | MuG (tvar_, _, _) when TypeVariableName.equal tvar tvar_ -> g
  | MuG (tvar_, rec_vars, g_) ->
      MuG (tvar_, rec_vars, substitute g_ tvar g_sub)
  | EndG -> EndG
  | MessageG (m, r1, r2, g_) -> MessageG (m, r1, r2, substitute g_ tvar g_sub)
  | ChoiceG (r, g_) ->
      ChoiceG (r, List.map ~f:(fun g__ -> substitute g__ tvar g_sub) g_)
  | CallG (caller, protocol, roles, g_) ->
      CallG (caller, protocol, roles, substitute g_ tvar g_sub)

let rec unfold = function
  | MuG (tvar, _, g_) as g -> substitute g_ tvar g
  | g -> g

let rec normalise = function
  | MessageG (m, r1, r2, g_) -> MessageG (m, r1, r2, normalise g_)
  | ChoiceG (r, g_) ->
      let g_ = List.map ~f:normalise g_ in
      flatten (ChoiceG (r, g_))
  | (EndG | TVarG _) as g -> g
  | MuG (tvar, rec_vars, g_) -> unfold (MuG (tvar, rec_vars, normalise g_))
  | CallG (caller, protocol, roles, g_) ->
      CallG (caller, protocol, roles, normalise g_)

let normalise_global_t (global_t : global_t) =
  let normalise_protocol ~key ~data acc =
    let all_roles, nested_protocols, g = data in
    Map.add_exn acc ~key ~data:(all_roles, nested_protocols, normalise g)
  in
  Map.fold
    ~init:(Map.empty (module ProtocolName))
    ~f:normalise_protocol global_t

let validate_refinements_exn t =
  let env =
    ( Expr.new_typing_env
    , Map.empty (module TypeVariableName)
    , Map.empty (module RoleName) )
  in
  let knowledge_add role_knowledge role variable =
    Map.update role_knowledge role ~f:(function
      | None -> Set.singleton (module VariableName) variable
      | Some s -> Set.add s variable )
  in
  let ensure_knowledge role_knowledge role e =
    let known_vars =
      Option.value
        ~default:(Set.empty (module VariableName))
        (Map.find role_knowledge role)
    in
    let free_vars = Expr.free_var e in
    let unknown_vars = Set.diff free_vars known_vars in
    if Set.is_empty unknown_vars then ()
    else uerr (UnknownVariableValue (role, Set.choose_exn unknown_vars))
  in
  let encode_progress_clause env payloads =
    let e =
      List.fold ~init:(Expr.Sexp.Atom "true")
        ~f:
          (fun e -> function
            | PValue (None, _) -> e
            | PValue (Some v, ty) ->
                let sort = Expr.smt_sort_of_type ty in
                let e =
                  match ty with
                  | Expr.PTRefined (v_, _, refinement) ->
                      if VariableName.equal v v_ then
                        Expr.Sexp.List
                          [ Expr.Sexp.Atom "and"
                          ; Expr.sexp_of_expr refinement
                          ; e ]
                      else
                        Err.violationf
                          "TODO: Handle the case where refinement and \
                           payload variables are different"
                  | _ -> e
                in
                Expr.Sexp.List
                  [ Expr.Sexp.Atom "exists"
                  ; Expr.Sexp.List
                      [ Expr.Sexp.List
                          [ Expr.Sexp.Atom (VariableName.user v)
                          ; Expr.Sexp.Atom sort ] ]
                  ; e ]
            | PDelegate _ -> (* Not supported *) e )
        payloads
    in
    let env =
      Expr.add_assert_s_expr (Expr.Sexp.List [Expr.Sexp.Atom "not"; e]) env
    in
    env
  in
  let ensure_progress env gs =
    let tyenv, _, _ = env in
    let encoded = Expr.encode_env tyenv in
    let rec gather_first_message = function
      | MessageG (m, _, _, _) -> [m.payload]
      | ChoiceG (_, gs) -> List.concat_map ~f:gather_first_message gs
      | MuG (_, _, g) -> gather_first_message g
      | TVarG (_, _, g) -> gather_first_message (Lazy.force g)
      | EndG -> []
      | CallG _ -> (* Not supported *) []
    in
    let first_messages = List.concat_map ~f:gather_first_message gs in
    let encoded =
      List.fold ~init:encoded ~f:encode_progress_clause first_messages
    in
    match Expr.check_sat encoded with
    | `Unsat -> ()
    | _ -> uerr StuckRefinement
  in
  let rec aux env =
    ( if Pragma.validate_refinement_satisfiability () then
      let tyenv, _, _ = env in
      Expr.ensure_satisfiable tyenv ) ;
    function
    | EndG -> ()
    | MessageG (m, role_send, role_recv, g) ->
        let payloads = m.payload in
        let f (tenv, rvenv, role_knowledge) = function
          | PValue (v_opt, p_type) ->
              if Expr.is_well_formed_type tenv p_type then
                match v_opt with
                | Some v ->
                    let tenv = Expr.env_append tenv v p_type in
                    let role_knowledge =
                      knowledge_add role_knowledge role_recv v
                    in
                    let role_knowledge =
                      knowledge_add role_knowledge role_send v
                    in
                    let () =
                      match p_type with
                      | Expr.PTRefined (_, _, e) ->
                          if Pragma.sender_validate_refinements () then
                            ensure_knowledge role_knowledge role_send e ;
                          if Pragma.receiver_validate_refinements () then
                            ensure_knowledge role_knowledge role_recv e
                      | _ -> ()
                    in
                    (tenv, rvenv, role_knowledge)
                | None -> (tenv, rvenv, role_knowledge)
              else uerr (IllFormedPayloadType (Expr.show_payload_type p_type))
          | PDelegate _ -> unimpl "Delegation as payload"
        in
        let env = List.fold ~init:env ~f payloads in
        aux env g
    | ChoiceG (_, gs) ->
        List.iter ~f:(aux env) gs ;
        if Pragma.validate_refinement_progress () then ensure_progress env gs
    | MuG (tvar, rec_vars, g) ->
        let f (tenv, rvenv, role_knowledge)
            {rv_name; rv_ty; rv_init_expr; rv_roles} =
          if Expr.is_well_formed_type tenv rv_ty then
            if Expr.check_type tenv rv_init_expr rv_ty then
              let tenv = Expr.env_append tenv rv_name rv_ty in
              let rvenv = Map.add_exn ~key:tvar ~data:rec_vars rvenv in
              let role_knowledge =
                List.fold ~init:role_knowledge
                  ~f:(fun acc role -> knowledge_add acc role rv_name)
                  rv_roles
              in
              (tenv, rvenv, role_knowledge)
            else
              uerr
                (TypeError
                   (Expr.show rv_init_expr, Expr.show_payload_type rv_ty) )
          else uerr (IllFormedPayloadType (Expr.show_payload_type rv_ty))
        in
        let env = List.fold ~init:env ~f rec_vars in
        aux env g
    | TVarG (tvar, rec_exprs, _) -> (
        let tenv, rvenv, role_knowledge = env in
        (* Unbound TypeVariable should not be possible, because it was
           previously validated *)
        let rec_vars = Option.value ~default:[] @@ Map.find rvenv tvar in
        match
          List.iter2
            ~f:(fun {rv_ty; rv_roles; _} rec_expr ->
              if Expr.check_type tenv rec_expr rv_ty then
                List.iter
                  ~f:(fun role ->
                    ensure_knowledge role_knowledge role rec_expr )
                  rv_roles
              else
                uerr
                  (TypeError
                     (Expr.show rec_expr, Expr.show_payload_type rv_ty) ) )
            rec_vars rec_exprs
        with
        | Base.List.Or_unequal_lengths.Ok () -> ()
        | Base.List.Or_unequal_lengths.Unequal_lengths ->
            unimpl
              "Error message for mismatched number of recursion variable \
               declaration and expressions" )
    | CallG _ -> assert false
  in
  aux env t

let add_missing_payload_field_names global_t =
  let add_missing_names namegen = function
    | PValue (None, n1) ->
        let payload_name_str =
          "p_" ^ String.uncapitalize @@ Expr.show_payload_type n1
        in
        let namegen, payload_name_str =
          Namegen.unique_name namegen payload_name_str
        in
        let payload_name = VariableName.of_string payload_name_str in
        (namegen, PValue (Some payload_name, n1))
    | PValue (Some payload_name, n1) ->
        let payload_name_str =
          String.uncapitalize @@ VariableName.user payload_name
        in
        let namegen, payload_name_str =
          Namegen.unique_name namegen payload_name_str
        in
        let payload_name =
          VariableName.rename payload_name payload_name_str
        in
        (namegen, PValue (Some payload_name, n1))
    | PDelegate _ as p -> (namegen, p)
  in
  let rec add_missing_payload_names = function
    | MessageG (m, sender, recv, g) ->
        let g = add_missing_payload_names g in
        let {payload; _} = m in
        let namegen = Namegen.create () in
        let _, payload =
          List.fold_map payload ~init:namegen ~f:add_missing_names
        in
        MessageG ({m with payload}, sender, recv, g)
    | MuG (n, rec_vars, g) -> MuG (n, rec_vars, add_missing_payload_names g)
    | (TVarG _ | EndG) as p -> p
    | ChoiceG (r, gs) -> ChoiceG (r, List.map gs ~f:add_missing_payload_names)
    | CallG (caller, proto_name, roles, g) ->
        let g = add_missing_payload_names g in
        CallG (caller, proto_name, roles, g)
  in
  Map.map global_t ~f:(fun (roles, nested_protocols, gtype) ->
      (roles, nested_protocols, add_missing_payload_names gtype) )

let global_t_of_module (scr_module : Syntax.scr_module) =
  let open Syntax in
  let scr_module = Protocol.rename_nested_protocols scr_module in
  let split_role_names (roles, new_roles) =
    let role_names = List.map ~f:RoleName.of_name roles in
    let new_role_names = List.map ~f:RoleName.of_name new_roles in
    (role_names, new_role_names)
  in
  let rec add_protocol protocols (protocol : global_protocol) =
    let nested_protocols = protocol.value.nested_protocols in
    let protocols =
      List.fold ~init:protocols ~f:add_protocol nested_protocols
    in
    let proto_name = ProtocolName.of_name protocol.value.name in
    let g = of_protocol protocol in
    let roles = split_role_names protocol.value.split_roles in
    let nested_protocol_names =
      List.map
        ~f:(fun {Loc.value= {name; _}; _} -> ProtocolName.of_name name)
        nested_protocols
    in
    Map.add_exn protocols ~key:proto_name
      ~data:(roles, nested_protocol_names, g)
  in
  let all_protocols = scr_module.protocols @ scr_module.nested_protocols in
  let global_t =
    List.fold
      ~init:(Map.empty (module ProtocolName))
      ~f:add_protocol all_protocols
  in
  normalise_global_t @@ add_missing_payload_field_names global_t
