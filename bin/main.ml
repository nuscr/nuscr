open Nuscrlib__.Err

let () =
  try
    let str = Nuscrlib.process Nuscrlib.message in
    print_endline str
  with
    UserError msg -> "User error: " ^ msg |> print_endline
  | Violation msg -> "Violation: " ^ msg |> print_endline
  | _ -> print_endline "boom"
