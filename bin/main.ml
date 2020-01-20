open Base
open Stdio
open Nuscrlib
open Names

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

let process_file (fn : string) (proc : string -> In_channel.t -> 'a) : 'a =
  let input = In_channel.create fn in
  let res = proc fn input in
  In_channel.close input ; res

let gen_output ast f = function
  | Some (role, protocol) ->
      let res = f ast protocol role in
      print_endline res
  | _ -> ()

let run filename verbose enumerate proj fsm gencode =
  try
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

let usage () =
  "usage: "
  ^ (Sys.get_argv ()).(0)
  ^ " [-enum][-verbose][-fsm Role@Protocol][-project Role@Protocol] file"

let version_string () = "%%VERSION%%"

let () =
  let filename = ref "" in
  Caml.Arg.parse argspec (fun fn -> filename := fn) @@ usage () ;
  if !version then print_endline @@ version_string ()
  else if String.length !filename = 0 then usage () |> print_endline
  else if run !filename !verbose !enum !project !fsm !gencode then
    Caml.exit 0
  else Caml.exit 1
