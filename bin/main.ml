open Base
open Stdio
open Nuscrlib
open Names

let version_string () = "%%VERSION%%"

let usage () =
  "usage: "
  ^ (Sys.get_argv ()).(0)
  ^ " [-enum][-verbose][-fsm Role@Protocol][-project \
     Role@Protocol][-gencode Role@Protocol][-out-dir /file/path][-go-path \
     /file/path] file"

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

let out_dir : string option ref = ref None

let go_path : string option ref = ref None

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
    , ": generate code for the specified role" )
  ; ( "-go-path"
    , Arg.String (fun s -> go_path := Some s)
    , ": path to the go source directory (the parent directory of the \
       project root)" )
  ; ( "-out-dir"
    , Arg.String (fun s -> out_dir := Some s)
    , ": path to the project directory inside which the code is to be \
       generated, relative to go source directory " ) ]

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
    | `PrintUsage, _ -> usage () |> print_endline
    | `ShowPragmas, _ -> Syntax.show_pragmas pragmas |> print_endline
    | `NestedProtocols, _ ->
        if Option.is_some !fsm then
          Err.uerr (Err.IncompatibleFlag ("fsm", Syntax.show_pragma k))
  in
  List.iter ~f:process_global_pragma pragmas

let generate_code (ast : Syntax.scr_module) gencode =
  let gen_ocaml role protocol =
    Lib.generate_ocaml_code ~monad:false ast ~protocol ~role
  in
  let gen_go protocol =
    match !out_dir with
    | Some out_dir ->
        Lib.generate_go_code ast ~protocol ~out_dir ~go_path:!go_path
    | None ->
        Err.UserError
          (Err.MissingFlag
             ( "out-dir"
             , "This flag must be set in order to generate go implementation"
             ))
        |> raise
  in
  match gencode with
  | Some (role, protocol) ->
      let impl =
        match
          List.find ast.pragmas ~f:(fun (pragma, _) ->
              match pragma with `NestedProtocols -> true | _ -> false)
        with
        | None -> gen_ocaml role protocol
        | Some _ -> gen_go protocol
      in
      print_endline impl
  | None -> ()

let run filename verbose enumerate proj fsm gencode =
  try
    let ast = process_file filename Lib.parse in
    process_pragmas ast.pragmas ;
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
    generate_code ast gencode ;
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

let () =
  let filename = ref "" in
  Caml.Arg.parse argspec (fun fn -> filename := fn) @@ usage () ;
  if !version then print_endline @@ version_string ()
  else if String.length !filename = 0 then usage () |> print_endline
  else if run !filename !verbose !enum !project !fsm !gencode then
    Caml.exit 0
  else Caml.exit 1
