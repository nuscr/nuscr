open Nuscrlib__.Err

let run fn =
  try
    let str = Nuscrlib.process_file fn in
    print_endline str
  with
    UserError msg -> "User error: " ^ msg |> print_endline
  | Violation msg -> "Violation: " ^ msg |> print_endline
  | e -> "Reported problem:\n " ^ (Printexc.to_string e) |> print_endline


let () =
  let pwd =  Sys.getenv "PWD" in
  if Array.length Sys.argv <> 2 then
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <scribble source> \n Running at: " ^ pwd)
  else
    run Sys.argv.(1)
