open! Base
open! Stdio
open Printf
open Err

let write_file file_name content =
  Out_channel.with_file file_name ~f:(fun file ->
      Out_channel.output_string file content )

let create_dir dir_path =
  let _ = Unix.umask 0o000 in
  try Unix.mkdir dir_path 0o755
  with Unix.Unix_error _ ->
    uerr
      (FileSysErr
         (sprintf "Unable to create new directory: %s in path: %s" dir_path
            (Unix.getcwd ()) ) )

let gen_file_path path file_name = sprintf "%s/%s" path file_name

let change_dir dir_path =
  try Unix.chdir dir_path
  with Unix.Unix_error _ ->
    uerr (FileSysErr (sprintf "Unable to change directory to: %s" dir_path))
