open Core
open Nuscrlib

let process_file (fn : string) (proc : string -> In_channel.t -> 'a) : 'a =
  let input = In_channel.create fn in
  let res = proc fn input in
  In_channel.close input ; res

let gen_output ast f = function
  | Some (role, name) ->
      let res = f ast name role in
      print_endline res
  | _ -> ()

let run filename verbose enumerate proj _fsm =
  try
    let ast = process_file filename Lib.parse in
    Lib.validate_exn ast verbose ;
    if enumerate then
      Lib.enumerate ast
      |> List.map ~f:(fun (n, r) -> r ^ "@" ^ n)
      |> String.concat ~sep:"\n" |> print_endline
    else () ;
    let _ =
      gen_output ast
        (fun ast r n ->
          Lib.project_role ast r n |> Ltype.show_local_type)
        proj
    in
    let _ =
      gen_output ast
        (fun ast r n ->
          Lib.generate_fsm ast r n |> snd |> Efsm.show_efsm)
        _fsm
    in
    ()
  with
  | Err.UserError msg ->
      "User error: " ^ Err.show_user_error msg |> print_endline
  | Err.Violation msg -> "Violation: " ^ msg |> print_endline
  | e -> "Reported problem:\n " ^ Exn.to_string e |> print_endline

let get_pwd () = Sys.getenv_exn "PWD"

let role_protocol =
  Command.Arg_type.create (fun rp ->
      match String.split rp ~on:'@' with
      | [role; protocol] -> (role, protocol)
      | _ ->
          Err.UserError
            (InvalidCommandLineParam
               "Role and protocol have to be for the form role@protocol")
          |> raise)

let command =
  let open Command.Let_syntax in
  Command.basic ~summary:"Project global types onto local types and CFSMs"
    ~readme:(fun () -> "Running from: " ^ get_pwd ())
    [%map_open
      let file_name = anon ("FILE" %: string)
      and verbose =
        flag "-verbose" no_arg ~doc:" verbosely process the files"
      and enumerate =
        flag "-enum" no_arg ~doc:" enumerate the contents of the file"
      and project =
        flag "-project" (optional role_protocol)
          ~doc:" project the local type of..."
      and fsm =
        flag "-fsm" (optional role_protocol)
          ~doc:" project the state machine of..."
      in
      fun () -> run file_name verbose enumerate project fsm]

let () =
  Command.run ~version:"0.00" ~build_info:"I'm sorry, no build info... yet?"
    command
