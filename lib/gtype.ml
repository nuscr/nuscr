open! Base
open Printf
open Loc
open Err
open Names

type payload =
  | PValue of VariableName.t option * PayloadTypeName.t
  | PDelegate of ProtocolName.t * RoleName.t
[@@deriving eq, sexp_of, ord]

let show_payload = function
  | PValue (var, ty) ->
      let var =
        match var with
        | Some var -> VariableName.user var ^ ": "
        | None -> ""
      in
      sprintf "%s%s" var (PayloadTypeName.user ty)
  | PDelegate (proto, role) ->
      sprintf "%s @ %s" (ProtocolName.user proto) (RoleName.user role)

let pp_payload fmt p = Caml.Format.fprintf fmt "%s" (show_payload p)

let of_syntax_payload (payload : Syntax.payloadt) =
  let open Syntax in
  match payload with
  | PayloadName n -> PValue (None, PayloadTypeName.of_name n)
  | PayloadDel (p, r) ->
      PDelegate (ProtocolName.of_name p, RoleName.of_name r)
  | PayloadQName qn ->
      PValue (None, PayloadTypeName.of_string (qname_to_string qn))
      (* FIXME *)
  | PayloadBnd (var, n) ->
      PValue
        ( Some (VariableName.of_name var)
        , PayloadTypeName.of_string (qname_to_string n) )

(* FIXME *)

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
  | MessageQName qn ->
      {label= LabelName.of_string (qname_to_string qn); payload= []}

(* FIXME *)

type t =
  | MessageG of message * RoleName.t * RoleName.t * t
  | MuG of TypeVariableName.t * t
  | TVarG of TypeVariableName.t
  | ChoiceG of RoleName.t * t list
  | EndG

let show =
  let indent_here indent = String.make (indent * 2) ' ' in
  let rec show_global_type_internal indent =
    let current_indent = indent_here indent in
    function
    | MessageG (m, r1, r2, g) ->
        sprintf "%s%s from %s to %s;\n%s" current_indent (show_message m)
          (RoleName.user r1) (RoleName.user r2)
          (show_global_type_internal indent g)
    | MuG (n, g) ->
        sprintf "%srec %s {\n%s%s}\n" current_indent
          (TypeVariableName.user n)
          (show_global_type_internal (indent + 1) g)
          current_indent
    | TVarG n -> sprintf "%s%s\n" current_indent (TypeVariableName.user n)
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
  in
  show_global_type_internal 0

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
  let rec conv_interactions rec_names
      (interactions : global_interaction list) =
    match interactions with
    | [] -> EndG
    | {value; _} :: rest -> (
      match value with
      | MessageTransfer {message; from_role; to_roles; _} ->
          let from_role = RoleName.of_name from_role in
          let to_roles = List.map ~f:RoleName.of_name to_roles in
          check_role from_role ;
          let init = conv_interactions rec_names rest in
          let f to_role acc =
            check_role to_role ;
            if RoleName.equal from_role to_role then
              uerr (ReflexiveMessage from_role) ;
            MessageG (of_syntax_message message, from_role, to_role, acc)
          in
          List.fold_right ~f ~init to_roles
      | Recursion (rname, interactions) ->
          let rname = TypeVariableName.of_name rname in
          if List.mem rec_names rname ~equal:TypeVariableName.equal then
            unimpl "Alpha convert recursion names"
          else assert_empty rest ;
          MuG (rname, conv_interactions (rname :: rec_names) interactions)
      | Continue name ->
          let name = TypeVariableName.of_name name in
          if List.mem rec_names name ~equal:TypeVariableName.equal then (
            assert_empty rest ; TVarG name )
          else uerr (UnboundRecursionName name)
      | Choice (role, interactions_list) ->
          let role = RoleName.of_name role in
          assert_empty rest ;
          check_role role ;
          ChoiceG
            ( role
            , List.map ~f:(conv_interactions rec_names) interactions_list )
      | Do _ ->
          Violation "The do constructor should not be here. This cannot be!"
          |> raise )
  in
  conv_interactions [] interactions

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
  | TVarG tvar_ when TypeVariableName.equal tvar tvar_ -> g_sub
  | TVarG _ -> g
  | MuG (tvar_, _) when TypeVariableName.equal tvar tvar_ -> g
  | MuG (tvar_, g_) -> MuG (tvar_, substitute g_ tvar g_sub)
  | EndG -> EndG
  | MessageG (m, r1, r2, g_) -> MessageG (m, r1, r2, substitute g_ tvar g_sub)
  | ChoiceG (r, g_) ->
      ChoiceG (r, List.map ~f:(fun g__ -> substitute g__ tvar g_sub) g_)

let rec unfold = function
  | MuG (tvar, g_) as g -> substitute g_ tvar g
  | g -> g

let rec normalise = function
  | MessageG (m, r1, r2, g_) -> MessageG (m, r1, r2, normalise g_)
  | ChoiceG (r, g_) ->
      let g_ = List.map ~f:normalise g_ in
      flatten (ChoiceG (r, g_))
  | (EndG | TVarG _) as g -> g
  | MuG (tvar, g_) -> unfold (MuG (tvar, normalise g_))
