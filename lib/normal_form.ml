open Core_kernel
open Syntax
open Err

(* From Featherweight Scribble *)
(* https://doi.org/10.1007/978-3-030-21485-2_14 *)

let rec flatten (protocol : global_interaction list) :
    global_interaction list =
  let flatten_single (protocol_item : global_interaction) :
      global_interaction =
    match protocol_item with
    | {value= Choice (role, choices); _} ->
        let flattened_inner = List.map ~f:flatten choices in
        let liftInner (p : global_interaction list) :
            global_interaction list list =
          match p with
          | {value= Choice (role_, choices); _} :: _ when role_ = role ->
              choices
          | p -> [p]
        in
        { protocol_item with
          value= Choice (role, List.concat_map ~f:liftInner flattened_inner)
        }
    | _ -> protocol_item
  in
  List.map ~f:flatten_single protocol

let%test "Flatten Example" =
  (* Taken from Fig. 9 *)
  let mkMsg name = Message {name; payload= []} in
  let m1 = mkMsg "m1" in
  let m2 = mkMsg "m2" in
  let m3 = mkMsg "m3" in
  let p = (Lexing.dummy_pos, Lexing.dummy_pos) in
  let mkMsgTransfer m =
    make_located ~loc:p
      (MessageTransfer
         {message= m; from_role= "A"; to_roles= ["B"]; ann= None})
  in
  let choice1 =
    make_located ~loc:p
      (Choice ("A", [[mkMsgTransfer m1]; [mkMsgTransfer m2]]))
  in
  let before =
    make_located ~loc:p (Choice ("A", [[choice1]; [mkMsgTransfer m3]]))
  in
  let after =
    make_located ~loc:p
      (Choice
         ("A", [[mkMsgTransfer m1]; [mkMsgTransfer m2]; [mkMsgTransfer m3]]))
  in
  flatten [before] = [after]

let unfold (protocol : global_interaction list) =
  let rec replace loc name protocol items =
    match items with
    | {value= Continue name_; _} :: tl when name = name_ ->
        List.append protocol (replace loc name protocol tl)
    | {value= Recursion (name_, _); loc= loc_new} :: _ when name = name_ ->
        raise (UserError (RedefinedRecursionName (name, loc, loc_new)))
    | [] -> []
    | hd :: tl -> hd :: replace loc name protocol tl
  in
  match protocol with
  | {value= Recursion (name, inner_protocol); loc} :: tl ->
      List.append (replace loc name inner_protocol inner_protocol) tl
  | _ -> protocol

let rec to_normal_form (protocol : global_interaction list) =
  let to_normal_form_single (item : global_interaction) :
      global_interaction list =
    let {loc; value} = item in
    match value with
    | Choice (name, protocols) ->
        [ { loc
          ; value=
              Choice
                ( name
                , List.map
                    ~f:(fun p -> p |> to_normal_form |> flatten)
                    protocols ) } ]
    | Recursion (name, inner_protocol) ->
        let inner_protocol = to_normal_form inner_protocol in
        unfold [{loc; value= Recursion (name, inner_protocol)}]
    | _ -> [item]
  in
  List.concat_map ~f:to_normal_form_single protocol

let%test "Normal Form example" =
  (* Taken from Fig. 10 *)
  let mkMsg name = Message {name; payload= []} in
  let m1 = mkMsg "m1" in
  let m2 = mkMsg "m2" in
  let m3 = mkMsg "m3" in
  let p = (Lexing.dummy_pos, Lexing.dummy_pos) in
  let mkMsgTransfer m =
    make_located ~loc:p
      (MessageTransfer
         {message= m; from_role= "A"; to_roles= ["B"]; ann= None})
  in
  let continue = make_located ~loc:p (Continue "Loop") in
  let choice1 =
    make_located ~loc:p
      (Choice ("A", [[mkMsgTransfer m1; continue]; [mkMsgTransfer m2]]))
  in
  let loop = make_located ~loc:p (Recursion ("Loop", [choice1])) in
  let before =
    make_located ~loc:p (Choice ("A", [[loop]; [mkMsgTransfer m3]]))
  in
  let after =
    make_located ~loc:p
      (Choice
         ( "A"
         , [[mkMsgTransfer m1; loop]; [mkMsgTransfer m2]; [mkMsgTransfer m3]]
         ))
  in
  to_normal_form [before] = [after]
