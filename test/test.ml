(* directorires to test *)

let dirs =
  [ "examples/parser"
  ; "examples/from-scribble-java/test/test1"
  ; "examples/from-scribble-java/test/test2"
  ; "examples/from-scribble-java/test/test3"
  ]

(* files to not test *)
let avoid = []

let get_files (dir : string) : string list =
  let rec loop res = function
    | [] -> res
    | f :: fs when Sys.is_directory f ->
      let fs' = Sys.readdir f |> Array.to_list
                |> List.map (Filename.concat f)
      in loop res (fs' @ fs)
    | f :: fs -> loop (f :: res) fs
  in
  loop [] [dir]

let get_scribble_files (dir : string) : string list =
  let fs = get_files dir in
  List.filter (fun f -> Filename.extension f = ".scr") fs

let get_scribble_test_files (dir : string) (avoid : string list) : string list =
  let is_avoid f = List.exists (Filename.check_suffix f) avoid in
  let fs = get_scribble_files dir in
  List.filter (fun f -> not (is_avoid f)) fs

let report dirs n =
  let title = "Test report\n~~~~~~~~~~~\n" in
  let tested = String.concat "\n" dirs in
  Printf.sprintf "%s\nTested:\n%s\nPassed: %d\n" title tested n

let write_report dirs n =
  let ch = open_out "test.report" in
  output_string ch (report dirs n) ;
  close_out ch

(* test the parser *)
let () =
  try
    let pwd =  Sys.getenv "PWD" in
    let dirs = List.map (Filename.concat pwd) dirs in
    let files =
      List.map (fun dir ->
          get_scribble_test_files dir avoid) dirs
    |> List.concat in
    let n = List.map Nuscrlib.process_file files |> List.length in
    write_report dirs n ;
    print_endline "Ok"
  with
  | e -> "Not ok\n" ^ Printexc.to_string e |> print_endline
