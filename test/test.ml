let test_files = [ "/examples/parser/first.scr"
                 ; "/examples/parser/GProtocol.scr"
                 ; "/examples/parser/GMessage.scr"
                 ]

(* test the parser *)
let () =
  try
    let pwd =  Sys.getenv "PWD" in
    let files = List.map (fun x -> (pwd ^ x)) test_files in
    let _ = List.map Nuscrlib.process_file files in
    print_endline "Ok"
  with
  | e -> "Not ok\n" ^ Printexc.to_string e |> print_endline
