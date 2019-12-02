open! Base
open Printf
open Syntax
open Loc
open Err

type t =
  | MessageG of message * name * name * t
  | MuG of name * t
  | TVarG of name
  | ChoiceG of name * t list
  | EndG

let show =
  let indent_here indent = String.make (indent * 2) ' ' in
  let rec show_global_type_internal indent =
    let current_indent = indent_here indent in
    function
    | MessageG (m, r1, r2, g) ->
        sprintf "%s%s from %s to %s;\n%s" current_indent (show_message m)
          (Name.user r1) (Name.user r2)
          (show_global_type_internal indent g)
    | MuG (n, g) ->
        sprintf "%srec %s {\n%s%s}\n" current_indent (Name.user n)
          (show_global_type_internal (indent + 1) g)
          current_indent
    | TVarG n -> sprintf "%s%s\n" current_indent (Name.user n)
    | EndG -> sprintf "%send\n" current_indent
    | ChoiceG (r, gs) ->
        let pre =
          sprintf "%schoice at %s {\n" current_indent (Name.user r)
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

let of_protocol global_protocol =
  let {Loc.value= {roles; interactions; _}; _} = global_protocol in
  let assert_empty l =
    if not @@ List.is_empty l then unimpl "Non tail-recursive protocol"
  in
  let check_role r loc =
    if not @@ List.mem roles r ~equal:Name.equal then
      uerr @@ UnboundRole (r, loc)
  in
  let rec conv_interactions rec_names
      (interactions : global_interaction list) =
    match interactions with
    | [] -> EndG
    | {value; loc} :: rest -> (
      match value with
      | MessageTransfer {message; from_role; to_roles; _} ->
          if List.length to_roles <> 1 then
            unimpl "Sending to multiple roles" ;
          let to_role = Option.value_exn (List.hd to_roles) in
          check_role from_role loc ;
          check_role to_role loc ;
          if Name.equal from_role to_role then
            uerr (ReflexiveMessage (from_role, loc)) ;
          MessageG
            (message, from_role, to_role, conv_interactions rec_names rest)
      | Recursion (rname, interactions) ->
          if List.mem rec_names rname ~equal:Name.equal then
            unimpl "Alpha convert recursion names"
          else assert_empty rest ;
          MuG (rname, conv_interactions (rname :: rec_names) interactions)
      | Continue name ->
          if List.mem rec_names name ~equal:Name.equal then (
            assert_empty rest ; TVarG name )
          else unimpl "Error message for Unbound TVar"
      | Choice (role, interactions_list) ->
          assert_empty rest ;
          check_role role loc ;
          ChoiceG
            ( role
            , List.map ~f:(conv_interactions rec_names) interactions_list )
      | Do _ -> assert false
      | _ -> unimpl "Other Scribble constructs" )
  in
  conv_interactions [] interactions

let rec flatten = function
  | ChoiceG (role, choices) ->
      let choices = List.map ~f:flatten choices in
      let lift = function
        | ChoiceG (role_, choices_) when Name.equal role role_ -> choices_
        | ChoiceG (_role, _choices) ->
            unimpl "Error message for inconsistent choices"
        | g -> [g]
      in
      ChoiceG (role, List.concat_map ~f:lift choices)
  | g -> g

let%test "Flatten Example" =
  let nos = Name.of_string in
  let mkMsg str = Message {name= nos str; payload= []} in
  let m1 = mkMsg "m1" in
  let m2 = mkMsg "m2" in
  let m3 = mkMsg "m3" in
  let mkMG m = MessageG (m, nos "A", nos "B", EndG) in
  let before =
    ChoiceG (nos "A", [ChoiceG (nos "A", [mkMG m1; mkMG m2]); mkMG m3])
  in
  let after = ChoiceG (nos "A", [mkMG m1; mkMG m2; mkMG m3]) in
  Poly.equal (flatten before) after

let rec substitute g tvar g_sub =
  match g with
  | TVarG tvar_ when Name.equal tvar tvar_ -> g_sub
  | TVarG _ -> g
  | MuG (tvar_, _) when Name.equal tvar tvar_ -> g
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

let%test "Normal Form Example" =
  let nos = Name.of_string in
  let mkMsg name = Message {name= nos name; payload= []} in
  let m1 = mkMsg "m1" in
  let m2 = mkMsg "m2" in
  let m3 = mkMsg "m3" in
  let mkMG m c = MessageG (m, nos "A", nos "B", c) in
  let before =
    ChoiceG
      ( nos "A"
      , [ MuG
            ( nos "Loop"
            , ChoiceG (nos "A", [mkMG m1 (TVarG (nos "Loop")); mkMG m2 EndG])
            )
        ; mkMG m3 EndG ] )
  in
  let after =
    ChoiceG
      ( nos "A"
      , [ mkMG m1
            (MuG
               ( nos "Loop"
               , ChoiceG
                   (nos "A", [mkMG m1 (TVarG (nos "Loop")); mkMG m2 EndG]) ))
        ; mkMG m2 EndG
        ; mkMG m3 EndG ] )
  in
  Poly.equal (normalise before) after
