open Base
open Stdio
open Nuscrlib
open Names
open Gocodegen
open Codegenstate

let version_string () = "%%VERSION%%"

let usage () =
  "usage: "
  ^ (Sys.get_argv ()).(0)
  ^ " [-enum][-verbose][-fsm Role@Protocol][-project Role@Protocol] file"

let nested_usage () =
  "usage: "
  ^ (Sys.get_argv ()).(0)
  ^ " [-verbose][-show_global][-project] file"

let parse_role_protocol_exn rp =
  match String.split rp ~on:'@' with
  | [role; protocol] ->
      Some (RoleName.of_string role, ProtocolName.of_string protocol)
  | _ ->
      Err.UserError
        (InvalidCommandLineParam
           "Role and protocol have to be for the form role@protocol")
      |> raise

let enum = ref false

let verbose = ref false

let version = ref false

let help = ref false

let project_protocols = ref false

let show_global = ref false

let fsm : (RoleName.t * ProtocolName.t) option ref = ref None

let project : (RoleName.t * ProtocolName.t) option ref = ref None

let gencode : (RoleName.t * ProtocolName.t) option ref = ref None

let argspec =
  let open Caml in
  [ ( "-enum"
    , Arg.Unit (fun () -> enum := true)
    , ": list the roles and protocols in the file" )
  ; ( "-verbose"
    , Arg.Unit (fun () -> verbose := true)
    , ": print extra information" )
  ; ( "-version"
    , Arg.Unit (fun () -> version := true)
    , ": print the version number" )
  ; ( "-fsm"
    , Arg.String (fun s -> fsm := parse_role_protocol_exn s)
    , ": project the CFSM for the specified role" )
  ; ( "-project"
    , Arg.String (fun s -> project := parse_role_protocol_exn s)
    , ": project the local type for the specified role" )
  ; ( "-gencode"
    , Arg.String (fun s -> gencode := parse_role_protocol_exn s)
    , ": generate OCaml code for the specified role" ) ]

let nested_argspec =
  let open Caml in
  [ ( "-version"
    , Arg.Unit (fun () -> version := true)
    , ": print the version number" )
  ; ("-ast", Arg.Unit (fun () -> verbose := true), ": print out ast")
  ; ( "-global-type"
    , Arg.Unit (fun () -> show_global := true)
    , ": print out global type" )
  ; ( "-project"
    , Arg.Unit (fun () -> project_protocols := true)
    , ": project the local type for the given protocols" )
  ; ( "-gencode"
    , Arg.String (fun _ -> Err.unimpl "Code generation not implemented")
    , ": generate Golang code for the specified protocol" ) ]

let process_file (fn : string) (proc : string -> In_channel.t -> 'a) : 'a =
  let input = In_channel.create fn in
  let res = proc fn input in
  In_channel.close input ; res

let gen_output ast f = function
  | Some (role, protocol) ->
      let res = f ast protocol role in
      print_endline res
  | _ -> ()

let process_pragmas (pragmas : Syntax.pragmas) : unit =
  let process_global_pragma (k, v) =
    match (k, v) with
    | "PrintUsage", _ -> usage () |> print_endline
    | "ShowPragmas", _ -> Syntax.show_pragmas pragmas |> print_endline
    | prg, _ -> Err.UnknownPragma prg |> Err.uerr
  in
  List.iter ~f:process_global_pragma pragmas

let run filename verbose enumerate proj fsm gencode =
  try
    let pragmas = process_file filename Lib.parse_pragmas in
    process_pragmas pragmas ;
    let ast = process_file filename Lib.parse in
    Lib.validate_exn ast ~verbose ;
    if enumerate then
      Lib.enumerate ast
      |> List.map ~f:(fun (n, r) ->
             RoleName.user r ^ "@" ^ ProtocolName.user n)
      |> String.concat ~sep:"\n" |> print_endline
    else () ;
    gen_output ast
      (fun ast protocol role ->
        Lib.project_role ast ~protocol ~role |> Ltype.show)
      proj ;
    gen_output ast
      (fun ast protocol role ->
        Lib.generate_fsm ast ~protocol ~role |> snd |> Efsm.show)
      fsm ;
    gen_output ast
      (fun ast protocol role ->
        Lib.generate_code ~monad:false ast ~protocol ~role)
      gencode ;
    true
  with
  | Err.UserError msg ->
      "User error: " ^ Err.show_user_error msg |> prerr_endline ;
      false
  | Err.Violation msg ->
      "Violation: " ^ msg |> prerr_endline ;
      false
  | Err.UnImplemented desc ->
      "I'm sorry, it is unfortunate " ^ desc ^ " is not implemented"
      |> prerr_endline ;
      false
  | e ->
      "Reported problem:\n " ^ Exn.to_string e |> prerr_endline ;
      false

let print_protocol_files file_map =
  Map.iteri file_map ~f:(fun ~key ~data ->
      let open Printf in
      print_endline (sprintf "Protocol %s:" (ProtocolName.user key)) ;
      print_endline data)

let print_local_protocol_files file_map =
  Map.iteri file_map ~f:(fun ~key ~data ->
      let open Printf in
      print_endline
        (sprintf "Local Protocol %s:" (LocalProtocolName.user key)) ;
      print_endline data)

let run_nested filename show_ast show_global show_local =
  let show_result ?(sep = "\n\n") ~f verbose input =
    (* only show if verbose is on *)
    if verbose then print_endline (Printf.sprintf "%s%s" (f input) sep)
    else ()
  in
  try
    let ast = process_file filename Lib.parse in
    Protocol.validate_calls_in_protocols ast ;
    show_result ~f:Syntax.show_scr_module show_ast ast ;
    let ast = Protocol.rename_nested_protocols ast in
    let open Gtype in
    let g_type = global_t_of_module ast in
    let g_type = normalise_global_t g_type in
    show_result ~sep:"\n\n----------\n" ~f:show_global_t show_global g_type ;
    let open Ltype in
    let ltype = project_global_t g_type in
    show_result ~f:show_local_t show_local ltype ;
    ensure_unique_identifiers g_type ;
    (* let protocol_channels = gen_protocol_channels g_type ltype in
       print_protocol_files protocol_channels ; *)
    let {channels; invite_channels; callbacks; impl; messages; results; _} =
      gen_code (RootDirName.of_string "Root") g_type ltype
    in
    print_protocol_files messages ;
    print_protocol_files channels ;
    print_protocol_files invite_channels ;
    print_protocol_files results ;
    print_local_protocol_files callbacks ;
    print_local_protocol_files impl ;
    true
  with
  | Err.UserError msg ->
      "User error: " ^ Err.show_user_error msg |> prerr_endline ;
      false
  | Err.Violation msg ->
      "Violation: " ^ msg |> prerr_endline ;
      false
  | Err.UnImplemented desc ->
      "I'm sorry, it is unfortunate " ^ desc ^ " is\n not implemented"
      |> prerr_endline ;
      false

(* | e -> "Reported problem:\n " ^ Exn.to_string e |> prerr_endline ; false *)

let () =
  let filename = ref "" in
  Caml.Arg.parse nested_argspec (fun fn -> filename := fn) @@ nested_usage () ;
  if !version then print_endline @@ version_string ()
  else if String.length !filename = 0 then nested_usage () |> print_endline
  else if run_nested !filename !verbose !show_global !project_protocols then
    Caml.exit 0
  else Caml.exit 1
