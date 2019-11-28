open Base
open Stdio
open Nuscrlib

let parse_role_protocol_exn rp =
  match String.split rp ~on:'@' with
  | [role; protocol] -> (role, protocol)
  | _ ->
      Err.UserError
        (InvalidCommandLineParam
           "Role and protocol have to be for the form role@protocol")
      |> raise

let enum = ref false

let verbose = ref false

let version = ref false

let help = ref false

let fsm : (string * string) option ref = ref None

let project : (string * string) option ref = ref None

let gencode : (string * string) option ref = ref None

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
    , Arg.String (fun s -> fsm := parse_role_protocol_exn s |> Some)
    , ": project the CFSM for the specified role" )
  ; ( "-project"
    , Arg.String (fun s -> project := parse_role_protocol_exn s |> Some)
    , ": project the local type for the specified role" )
  ; ( "-gencode"
    , Arg.String (fun s -> gencode := parse_role_protocol_exn s |> Some)
    , ": generate OCaml code for the specified role" ) ]

let process_file (fn : string) (proc : string -> In_channel.t -> 'a) : 'a =
  let input = In_channel.create fn in
  let res = proc fn input in
  In_channel.close input ; res

let gen_output ast f = function
  | Some (role, name) ->
      let res = f ast name role in
      print_endline res
  | _ -> ()

let run filename verbose enumerate proj fsm gencode =
  try
    let ast = process_file filename Lib.parse in
    Lib.validate_exn ast ~verbose ;
    if enumerate then
      Lib.enumerate ast
      |> List.map ~f:(fun (n, r) -> r ^ "@" ^ n)
      |> String.concat ~sep:"\n" |> print_endline
    else () ;
    let _ =
      gen_output ast
        (fun ast r n -> Lib.project_role ast r n |> Ltype.show)
        proj
    in
    let _ =
      gen_output ast
        (fun ast r n -> Lib.generate_fsm ast r n |> snd |> Efsm.show_efsm)
        fsm
    in
    let _ =
      gen_output ast
        (fun ast r n -> Lib.generate_fsm ast r n |> Codegen.gen_code (r, n))
        gencode
    in
    ()
  with
  | Err.UserError msg ->
      "User error: " ^ Err.show_user_error msg |> prerr_endline
  | Err.Violation msg -> "Violation: " ^ msg |> prerr_endline
  | Err.UnImplemented desc ->
      "I'm sorry, it is unfortunate " ^ desc ^ " is not implemented"
      |> prerr_endline
  | e -> "Reported problem:\n " ^ Exn.to_string e |> prerr_endline

let usage () =
  "usage: " ^ Sys.argv.(0)
  ^ " [-e][-verbose][-fsm Role@Protocol][-project Role@Protocol] file"

let version_string () = "%%VERSION%%"

let () =
  let filename = ref "" in
  Caml.Arg.parse argspec (fun fn -> filename := fn) @@ usage () ;
  if !version then print_endline @@ version_string ()
  else if String.length !filename = 0 then usage () |> print_endline
  else run !filename !verbose !enum !project !fsm !gencode
