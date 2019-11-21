(* directorires to test *)
open! Core

let dirs = ["examples"]

(* files to not test *)
let avoid =
  [ (* review these files *)
    "examples/from-scribble-java/tmp/Test.scr"
  ; "examples/from-scribble-java/tmp/Test2.scr"
  ; "examples/from-scribble-java/demo/supplierinfo/SupplierInfoExper.scr"
  ; "examples/consensus/ClockAnnotRec.scr" ]

let get_files (dir : string) : string list =
  let rec loop res = function
    | [] -> res
    | f :: fs when Sys.is_directory f = `Yes ->
        let fs' =
          Sys.readdir f |> Array.to_list |> List.map ~f:(Filename.concat f)
        in
        loop res (fs' @ fs)
    | f :: fs -> loop (f :: res) fs
  in
  loop [] [dir]

let get_scribble_files (dir : string) : string list =
  let fs = get_files dir in
  List.filter ~f:(fun f -> Filename.check_suffix f ".scr") fs

let get_scribble_test_files (dir : string) (avoid : string list) :
    string list =
  let is_avoid f = List.exists ~f:(Filename.check_suffix f) avoid in
  let fs = get_scribble_files dir in
  List.filter ~f:(fun f -> not (is_avoid f)) fs

let report dirs ok err msg =
  let title = "Test report\n~~~~~~~~~~~\n" in
  let tested = String.concat ~sep:"\n" dirs in
  Printf.sprintf "%s\nTested:\n%s\nPassed: %d Failed: %d\n%s\n" title tested
    ok err msg

let write_report dirs ok err msg =
  let ch = Out_channel.create "test.report" in
  Out_channel.output_string ch (report dirs ok err msg) ;
  Out_channel.close ch

let process_file (fn : string) (proc : string -> In_channel.t -> 'a) : unit =
  let input = In_channel.create fn in
  let _ = proc fn input in
  In_channel.close input

let process_files fns =
  let buffer = Buffer.create 1024 in
  let rec pf cnt_ok cnt_err = function
    | [] -> (cnt_ok, cnt_err, Buffer.contents buffer)
    | f :: fs -> (
      try
        let run fn in_channel =
          try
            let ast = Nuscrlib.Lib.parse fn in_channel in
            Nuscrlib.Lib.validate_exn ast ~verbose:false
          with
          | Nuscrlib.Err.UnImplemented _ -> ()
          | e -> raise e
        in
        let () = process_file f run in
        pf (cnt_ok + 1) cnt_err fs
      with e ->
        let msg =
          Printf.sprintf "File: %s -- Error message: %s\n" f
            (Exn.to_string e)
        in
        Buffer.add_string buffer msg ;
        Buffer.add_char buffer '\n' ;
        pf cnt_ok (cnt_err + 1) fs )
  in
  pf 0 0 fns

(* test the parser *)
let () =
  try
    let pwd = Sys.getenv_exn "PWD" in
    let dirs = List.map ~f:(Filename.concat pwd) dirs in
    let files =
      List.map ~f:(fun dir -> get_scribble_test_files dir avoid) dirs
      |> List.concat
    in
    let ok, err, errors = process_files files in
    write_report dirs ok err errors ;
    print_endline (if err = 0 then "Ok" else "Not ok")
  with e -> "Unexpected:\n" ^ Exn.to_string e |> print_endline
