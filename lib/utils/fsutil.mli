val write_file : string -> string -> unit
(** [write_file filename content] writes [content] into a file with
    [filename] *)

val create_dir : string -> unit
(** Creates a directory *)

val change_dir : string -> unit
(** Changes current working directory *)

val gen_file_path : string -> string -> string
(** [gen_file_path path filename] becomes "[path]/[filename]" *)
