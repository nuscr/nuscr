open! Base
open Syntax
open Err

(** Unroll `do` in protocol *)
let expand_global_protocol (protocol : global_protocol)
    (scr_module : scr_module) =
  let module M = Map in
  let f acc {Loc.value= p; Loc.loc} =
    match M.add acc ~key:(Name.user p.name) ~data:(p.interactions, loc) with
    | `Ok acc -> acc
    | `Duplicate ->
        let _, old_loc = M.find_exn acc (Name.user p.name) in
        uerr (RedefinedProtocol (p.name, loc, old_loc))
  in
  let _protocols =
    List.fold ~f ~init:(M.empty (module String)) scr_module.protocols
  in
  protocol
