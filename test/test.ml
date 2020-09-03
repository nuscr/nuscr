(* directorires to test *)
open! Base
open! Stdio

let dirs = ["examples"]

(* files to not test *)
let avoid =
  [ (* review these files *)
    "examples/from-scribble-java/tmp/Test.scr"
  ; "examples/from-scribble-java/tmp/Test2.scr"
  ; "examples/from-scribble-java/demo/supplierinfo/SupplierInfoExper.scr"
  ; "examples/consensus/ClockTR.scr"
  ; "examples/consensus/ClockAnnotTR.scr"
  ; "examples/consensus/ClockAnnotRec.scr"
  ; "examples/from-scribble-java/test/test5/Test5.scr"
  ; "examples/from-scribble-java/test/test8/Test8.scr"
  ; "examples/from-scribble-java/test/foo/Foo.scr"
  ; "examples/from-scribble-java/demo/fase17/overview/P1.scr" ]

let get_files (dir : string) : string list =
  let rec loop res = function
    | [] -> res
    | f :: fs when Caml.Sys.is_directory f ->
        let fs' =
          Caml.Sys.readdir f |> Array.to_list
          |> List.map ~f:(Caml.Filename.concat f)
        in
        loop res (fs' @ fs)
    | f :: fs -> loop (f :: res) fs
  in
  loop [] [dir]

let get_scribble_files (dir : string) : string list =
  let fs = get_files dir in
  List.filter ~f:(fun f -> Caml.Filename.check_suffix f ".scr") fs

let get_scribble_test_files (dir : string) (avoid : string list) :
    string list =
  let is_avoid f = List.exists ~f:(Caml.Filename.check_suffix f) avoid in
  let fs = get_scribble_files dir in
  List.filter ~f:(fun f -> not (is_avoid f)) fs

let report dirs ok err msg =
  let title = "Test report\n~~~~~~~~~~~\n" in
  let tested = String.concat ~sep:"\n" dirs in
  Printf.sprintf "%s\nTested:\n%s\nPassed: %d Failed: %d\n%s\n" title tested
    ok err msg

let write_report dirs ok err msg =
  let ch = Out_channel.create "test.report" in
  let report_content = report dirs ok err msg in
  Out_channel.output_string ch report_content ;
  Out_channel.close ch ;
  report_content

let process_file (fn : string) (proc : string -> In_channel.t -> 'a) : unit =
  let input = In_channel.create fn in
  let _ = proc fn input in
  In_channel.close input

exception ExpectFail

let process_pragmas (pragmas : Nuscrlib.Syntax.pragmas) : unit =
  let process_global_pragma ((k : Nuscrlib.Syntax.pragma), v) =
    match (k, v) with
    | `PrintUsage, _ -> ()
    | `ShowPragmas, _ -> ()
    | `NestedProtocols, _ -> ()
  in
  List.iter ~f:process_global_pragma pragmas

let process_files fns =
  let buffer = Buffer.create 1024 in
  let rec pf cnt_ok cnt_err = function
    | [] -> (cnt_ok, cnt_err, Buffer.contents buffer)
    | f :: fs -> (
      try
        let run fn in_channel =
          let is_negative_test =
            Caml.Filename.check_suffix (Caml.Filename.dirname fn) "errors"
          in
          try
            In_channel.seek in_channel 0L ;
            let ast = Nuscrlib.Lib.parse fn in_channel in
            process_pragmas ast.pragmas ;
            Nuscrlib.Lib.validate_exn ast ~verbose:false ;
            if is_negative_test then raise ExpectFail
          with
          | Nuscrlib.Err.UnImplemented _ -> ()
          | ExpectFail when is_negative_test -> raise ExpectFail
          | _ when is_negative_test -> ()
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
    let pwd = Caml.Sys.getenv "PWD" in
    let dirs = List.map ~f:(Caml.Filename.concat pwd) dirs in
    let files =
      List.map ~f:(fun dir -> get_scribble_test_files dir avoid) dirs
      |> List.concat
    in
    let ok, err, errors = process_files files in
    let report = write_report dirs ok err errors in
    print_endline (if err = 0 then "Ok" else "Not ok\n" ^ report) ;
    if err <> 0 then Caml.exit 1
  with e ->
    "Unexpected:\n" ^ Exn.to_string e |> print_endline ;
    Caml.exit 1
