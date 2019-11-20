open Nuscrlib__.Err
open Core

let run fn =
  try
    let str = Nuscrlib.process_file fn in
    print_endline str
  with
  | UserError msg -> "User error: " ^ show_user_error msg |> print_endline
  | Violation msg -> "Violation: " ^ msg |> print_endline
  | e -> "Reported problem:\n " ^ Exn.to_string e |> print_endline

let get_pwd () = Sys.getenv_exn "PWD"

let command =
  let open Command.Let_syntax in
  Command.basic ~summary:"Project global types onto local types and CFSMs"
    ~readme:(fun () -> "Running from: " ^ get_pwd ())
    [%map_open
      let file_name = anon ("FILE" %: string) in
      fun () -> run file_name]

let () =
  Command.run ~version:"0.00" ~build_info:"I'm sorry, no build info... yet?"
    command
