(* directorires to test *)

let dirs = [ "examples" ]

(* files to not test *)
let avoid =
  [
    (* review these files *)
    "examples/from-scribble-java/tmp/Test.scr"
  ; "examples/from-scribble-java/tmp/Test2.scr"
  ; "examples/from-scribble-java/demo/supplierinfo/SupplierInfoExper.scr"
  ]

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

let report dirs ok err msg =
  let title = "Test report\n~~~~~~~~~~~\n" in
  let tested = String.concat "\n" dirs in
  Printf.sprintf
    "%s\nTested:\n%s\nPassed: %d Failed: %d\n%s\n"
    title tested ok err msg

let write_report dirs ok err msg =
  let ch = open_out "test.report" in
  output_string ch (report dirs ok err msg) ;
  close_out ch

let process_files fns =
  let error_buffer = ref "" in
  let rec pf cnt_ok cnt_err = function
      [] ->  cnt_ok, cnt_err, !error_buffer
    | f::fs ->
      begin try
        let _ = Nuscrlib.process_file f in
        pf (cnt_ok + 1) cnt_err fs
      with
      | e ->
        let msg = Printf.sprintf
            "File: %s -- Error message: %s\n"
            f (Printexc.to_string e)
        in
        error_buffer := !error_buffer ^ msg ^ "\n" ;
        pf cnt_ok (cnt_err +1) fs
    end
  in
  pf 0 0 fns


(* test the parser *)
let () =
  try
    let pwd =  Sys.getenv "PWD" in
    let dirs = List.map (Filename.concat pwd) dirs in
    let files =
      List.map (fun dir ->
          get_scribble_test_files dir avoid) dirs
    |> List.concat in
    let ok, err, errors = process_files files in
    write_report dirs ok err errors ;
    print_endline (if err = 0 then "Ok" else "Not ok")
  with
  | e -> "Unexpected:\n" ^ Printexc.to_string e |> print_endline
