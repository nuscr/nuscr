open! Core_kernel
open Syntax
open Gtype
open Err

type local_type =
  | RecvL of message * name * local_type
  | SendL of message * name * local_type
  | ChoiceL of name * local_type list
  | TVarL of name
  | MuL of name * local_type
  | EndL
[@@deriving show]

exception Unmergable of local_type * local_type

let rec merge projected_role lty1 lty2 =
  let fail () = raise (Unmergable (lty1, lty2)) in
  let merge_recv r recvs =
    let rec aux (acc : (string * local_type) list) = function
      | RecvL (m, _, lty) -> (
          let label = message_label m in
          match List.Assoc.find acc ~equal:String.equal label with
          | None -> (label, lty) :: acc
          | Some lty_ ->
              List.Assoc.add acc ~equal:String.equal label
                (merge projected_role lty lty_) )
      | _ -> failwith "Impossible"
    in
    let conts = List.fold ~f:aux ~init:[] recvs in
    match conts with
    | [(_, lty)] -> lty
    | conts -> ChoiceL (r, List.map ~f:snd conts)
  in
  match (lty1, lty2) with
  | RecvL (_m1, r1, _lty1_), RecvL (_m2, r2, _lty2_) ->
      if not @@ String.equal r1 r2 then fail () ;
      merge_recv r1 [lty1; lty2]
  | ChoiceL (r1, ltys1), RecvL (_m, r2, lty2) when String.equal r1 r2 ->
      (* Choice is a set of receive *)
      merge_recv r1 (lty2 :: ltys1)
  | RecvL (_m, r2, lty2), ChoiceL (r1, _ltys1) when String.equal r1 r2 ->
      merge projected_role lty2 lty1
  | ChoiceL (r1, ltys1), ChoiceL (r2, ltys2)
    when String.equal r1 r2 && not (String.equal r1 projected_role) ->
      merge_recv r1 (ltys1 @ ltys2)
  | _ -> if lty1 = lty2 then lty1 else fail ()

(* Check whether the first message in a g choice is from choice_r to recv_r,
   if recv_r is Some; return receive role *)
let check_consistent_gchoice choice_r recv_r =
  let err () = unimpl "Error message for inconsistent choice" in
  function
  | MessageG (_, send_r, recv_r_, _) -> (
      if not @@ String.equal send_r choice_r then err () ;
      match recv_r with
      | None -> Some recv_r_
      | Some recv_r ->
          if not @@ String.equal recv_r recv_r then err () ;
          Some recv_r )
  | _ -> err ()

let rec project (gType : global_type) (roles : name list)
    (projected_role : name) =
  assert (List.mem roles projected_role ~equal:String.equal) ;
  match gType with
  | EndG -> EndL
  | TVarG name -> TVarL name
  | MuG (name, gType) -> (
    match project gType roles projected_role with
    | TVarL _ | EndL -> EndL
    | lType -> MuL (name, lType) )
  | MessageG (m, send_r, recv_r, gType) -> (
    match projected_role with
    | _ when String.equal projected_role send_r ->
        SendL (m, recv_r, project gType roles projected_role)
    | _ when String.equal projected_role recv_r ->
        RecvL (m, send_r, project gType roles projected_role)
    | _ -> project gType roles projected_role )
  | ChoiceG (choice_r, gTypes) -> (
      let recv_r =
        List.fold ~f:(check_consistent_gchoice choice_r) ~init:None gTypes
      in
      let recv_r = Option.value_exn recv_r in
      match projected_role with
      | _
        when String.equal projected_role choice_r
             || String.equal projected_role recv_r ->
          ChoiceL
            ( choice_r
            , List.map ~f:(fun g -> project g roles projected_role) gTypes
            )
      | _ ->
          let lTypes =
            List.map ~f:(fun g -> project g roles projected_role) gTypes
          in
          List.reduce_exn ~f:(merge projected_role) lTypes )
