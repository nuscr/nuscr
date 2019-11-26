open! Core_kernel
open! Migrate_parsetree

(* open Efsm *)

open! Ast_407
open Parsetree
open Ast_helper
open Syntax
open Efsm

let migrate = Versions.migrate Versions.ocaml_407 Versions.ocaml_current

let state_action_type (g : G.t) (st : int) =
  let merge_state_action_type aty1 aty2 =
    match (aty1, aty2) with
    | `Terminal, aty2 -> aty2
    | aty1, `Terminal -> aty1
    | `Send, `Send -> `Send
    | `Recv, `Recv -> `Recv
    | `Send, `Recv -> `Mixed
    | `Recv, `Send -> `Mixed
    | aty1, `Mixed -> aty1
    | `Mixed, aty2 -> aty2
  in
  let f (_, a, _) acc =
    let aty =
      match a with
      | SendA _ -> `Send
      | RecvA _ -> `Recv
      | Epsilon -> failwith "Impossible"
    in
    merge_state_action_type aty acc
  in
  G.fold_succ_e f g st `Terminal

let gen_callback_typedef (g : G.t) : structure_item =
  let loc = Location.none in
  let unit = [%type: unit] in
  let mk_constr id = Typ.constr (Location.mknoloc (Longident.parse id)) [] in
  let f st acc =
    match state_action_type g st with
    | `Mixed -> failwith "Impossible"
    | `Terminal -> acc
    | `Send ->
        let gen_send (_, a, _) acc =
          match a with
          | SendA (_, msg) ->
              let label = message_label msg in
              let payload_type = message_payload_ty msg in
              let payload_type =
                match payload_type with
                | [] -> unit
                | [x] -> mk_constr x
                | _ -> Typ.tuple (List.map ~f:mk_constr payload_type)
              in
              (label, payload_type) :: acc
          | _ -> failwith "Impossible"
        in
        let return_ty = G.fold_succ_e gen_send g st [] in
        let return_ty =
          match return_ty with
          | [] -> unit
          | _ ->
              let f (label, payload_ty) =
                Rtag (Location.mknoloc label, [], true, [payload_ty])
              in
              let rows = List.map ~f return_ty in
              Typ.variant rows Asttypes.Closed None
        in
        let field_name = sprintf "state%dSend" st in
        let field_ty =
          Typ.arrow Asttypes.Nolabel (mk_constr "unit") return_ty
        in
        let field = Type.field (Location.mknoloc field_name) field_ty in
        field :: acc
    | `Recv ->
        let gen_recv (_, a, _) callbacks =
          match a with
          | RecvA (_, msg) ->
              let label = message_label msg in
              let payload_type = message_payload_ty msg in
              let payload_type =
                match payload_type with
                | [] -> unit
                | [x] -> mk_constr x
                | _ -> Typ.tuple (List.map ~f:mk_constr payload_type)
              in
              let field_ty = Typ.arrow Asttypes.Nolabel payload_type unit in
              let field_name = sprintf "state%dReceive%s" st label in
              let field =
                Type.field (Location.mknoloc field_name) field_ty
              in
              field :: callbacks
          | _ -> failwith "Impossible"
        in
        G.fold_succ_e gen_recv g st acc
  in
  let callbacks = G.fold_vertex f g [] in
  let callbacks =
    Type.mk ~kind:(Ptype_record callbacks) (Location.mknoloc "callbacks")
  in
  let ret = Str.type_ Asttypes.Nonrecursive [callbacks] in
  (* let ret_ = migrate.Versions.copy_structure [ret] in Printast.structure 0
     (Format.formatter_of_out_channel stdout) ret_ ; *)
  ret

let gen_ast (_proto, _role) (_start, g) : structure =
  let loc = Location.none in
  let callback_typedef = gen_callback_typedef g in
  let vb = [%stri let hello = "hello"] in
  [callback_typedef; vb]

let gen_code (proto, role) (start, g) =
  let buffer = Buffer.create 4196 in
  let formatter = Format.formatter_of_buffer buffer in
  let ast = gen_ast (proto, role) (start, g) in
  let ast = migrate.Versions.copy_structure ast in
  Pprintast.structure formatter ast ;
  Format.pp_print_flush formatter () ;
  Buffer.contents buffer
