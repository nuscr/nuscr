open! Core_kernel
open Syntax

type global_type =
  | Message of message * name * name * global_type
  | Mu of name * global_type
  | TVar of name
  | ChoiceT of name * global_type list
  | End
[@@deriving show]

let global_type_of_protocol global_protocol =
  let {value= {name; roles; interactions; _}; _} = global_protocol in
  let has_global_recursion = ref false in
  let global_recursion_name = "__" ^ name in
  let assert_empty l =
    if not @@ List.is_empty l then
      failwith "TODO: Non tail-recursive protocol"
  in
  let rec conv_interactions rec_names
      (interactions : global_interaction list) =
    match interactions with
    | [] -> End
    | {value; _} :: rest -> (
      match value with
      | MessageTransfer {message; from_role; to_roles; _} ->
          assert (List.length to_roles = 1) ;
          let to_role = Option.value_exn (List.hd to_roles) in
          Message
            (message, from_role, to_role, conv_interactions rec_names rest)
      | Recursion (rname, interactions) ->
          if List.mem rec_names rname ~equal:( = ) then
            failwith "TODO: alpha convert"
          else assert_empty rest ;
          Mu (rname, conv_interactions (rname :: rec_names) interactions)
      | Continue name ->
          if List.mem rec_names name ~equal:( = ) then (
            assert_empty rest ; TVar name )
          else failwith "TODO: Unbound TVar"
      | Choice (role, interactions_list) ->
          assert_empty rest ;
          ChoiceT
            ( role
            , List.map ~f:(conv_interactions rec_names) interactions_list )
      | Do (name_, _, roles_, _)
        when name = name_ && List.equal ( = ) roles roles_ ->
          has_global_recursion := true ;
          assert_empty rest ;
          TVar global_recursion_name
      | Do _ -> failwith "TODO: handle other do"
      | _ -> failwith "TODO" )
  in
  let converted = conv_interactions [global_recursion_name] interactions in
  let converted =
    if !has_global_recursion then Mu (global_recursion_name, converted)
    else converted
  in
  converted
