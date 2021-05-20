open Base
open Stdio
open Nuscrlib
open Names
open Cmdliner

let parse_role_protocol_exn rp =
  match String.split rp ~on:'@' with
  | [role; protocol] ->
      Some (RoleName.of_string role, ProtocolName.of_string protocol)
  | _ ->
      Err.UserError
        (InvalidCommandLineParam
           "Role and protocol have to be for the form role@protocol" )
      |> raise

let process_file (fn : string) (proc : string -> In_channel.t -> 'a) : 'a =
  let input = In_channel.create fn in
  let res = proc fn input in
  In_channel.close input ; res

let gen_output ast f = function
  | Some (role, protocol) ->
      let res = f ast protocol role in
      print_endline res
  | _ -> ()

let main file enumerate verbose go_path out_dir project fsm gencode_ocaml
    gencode_monadic_ocaml gencode_go gencode_fstar sexp_global_type
    show_solver_queries =
  Config.set_solver_show_queries show_solver_queries ;
  Config.set_verbose verbose ;
  try
    let ast = process_file file Nuscrlib.parse in
    Config.load_from_pragmas ast.pragmas ;
    if Option.is_some fsm && Config.nested_protocol_enabled () then
      Err.uerr
        (Err.IncompatibleFlag
           ("fsm", Config.show_pragma Config.NestedProtocols) ) ;
    Nuscrlib.validate_exn ast ;
    let () =
      if enumerate then
        Nuscrlib.enumerate ast
        |> List.map ~f:(fun (n, r) ->
               RoleName.user r ^ "@" ^ ProtocolName.user n )
        |> String.concat ~sep:"\n" |> print_endline
    in
    let () =
      gen_output ast
        (fun ast protocol role ->
          Nuscrlib.project_role ast ~protocol ~role |> Ltype.show )
        project
    in
    let () =
      gen_output ast
        (fun ast protocol role ->
          Nuscrlib.generate_fsm ast ~protocol ~role |> snd |> Efsm.show )
        fsm
    in
    let () =
      Option.iter
        ~f:(fun (role, protocol) ->
          Nuscrlib.generate_ocaml_code ~monad:false ast ~protocol ~role
          |> print_endline )
        gencode_ocaml
    in
    let () =
      Option.iter
        ~f:(fun (role, protocol) ->
          Nuscrlib.generate_ocaml_code ~monad:true ast ~protocol ~role
          |> print_endline )
        gencode_monadic_ocaml
    in
    let () =
      Option.iter
        ~f:(fun (role, protocol) ->
          Nuscrlib.generate_fstar_code ast ~protocol ~role |> print_endline
          )
        gencode_fstar
    in
    let () =
      Option.iter
        ~f:(fun (_role, protocol) ->
          match out_dir with
          | Some out_dir ->
              let impl =
                Nuscrlib.generate_go_code ast ~protocol ~out_dir ~go_path
              in
              print_endline impl
          | None ->
              Err.UserError
                (Err.MissingFlag
                   ( "out-dir"
                   , "This flag must be set in order to generate go \
                      implementation" ) )
              |> raise )
        gencode_go
    in
    let () =
      Option.iter
        ~f:(fun protocol ->
          let protocol = ProtocolName.of_string protocol in
          Nuscrlib.generate_sexp ast ~protocol |> print_endline )
        sexp_global_type
    in
    `Ok ()
  with
  | Err.UserError msg ->
      `Error (false, "User error: " ^ Err.show_user_error msg)
  | Err.Violation msg -> `Error (false, "Internal Error: " ^ msg)
  | Err.UnImplemented desc ->
      `Error
        ( false
        , "I'm sorry, it is unfortunate " ^ desc ^ " is not implemented" )
  | e -> `Error (false, "Reported problem:\n " ^ Exn.to_string e)

let role_proto =
  let parse input =
    match String.split input ~on:'@' with
    | [role; protocol] ->
        Ok (RoleName.of_string role, ProtocolName.of_string protocol)
    | _ ->
        Error (`Msg "Role and protocol have to be for the form role@protocol")
  in
  let print fmt (r, p) =
    Caml.Format.pp_print_string fmt (RoleName.user r) ;
    Caml.Format.pp_print_char fmt '@' ;
    Caml.Format.pp_print_string fmt (ProtocolName.user p)
  in
  Arg.conv (parse, print)

let enumerate =
  let doc = "Enumerate the roles and protocols in the file" in
  Arg.(value & flag & info ["enum"] ~doc)

let verbose =
  let doc = "Print extra information" in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let go_path =
  let doc =
    "Path to the Go source directory (the parent directory of the project \
     root) [Only applicable for Go Codegen]"
  in
  Arg.(value & opt (some dir) None & info ["go-path"] ~doc ~docv:"DIR")

let project =
  let doc =
    "Project the local type for the specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value
    & opt (some role_proto) None
    & info ["project"] ~doc ~docv:"ROLE@PROTO")

let fsm =
  let doc =
    "Project the CFSM for the specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value & opt (some role_proto) None & info ["fsm"] ~doc ~docv:"ROLE@PROTO")

let gencode_ocaml =
  let doc =
    "Generate OCaml code for specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value
    & opt (some role_proto) None
    & info ["gencode-ocaml"] ~doc ~docv:"ROLE@PROTO")

let gencode_fstar =
  let doc =
    "Generate OCaml code for specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value
    & opt (some role_proto) None
    & info ["gencode-fstar"] ~doc ~docv:"ROLE@PROTO")

let gencode_monadic_ocaml =
  let doc =
    "Generate monadic OCaml code for specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value
    & opt (some role_proto) None
    & info ["gencode-ocaml-monadic"] ~doc ~docv:"ROLE@PROTO")

let gencode_go =
  let doc =
    "Generate Go code for specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value
    & opt (some role_proto) None
    & info ["gencode-go"] ~doc ~docv:"ROLE@PROTO")

let sexp_global_type =
  let doc =
    "Generate the S-expression for the specified protocol. <protocol_name>"
  in
  Arg.(
    value
    & opt (some string) None
    & info ["generate-sexp"] ~doc ~docv:"PROTO")

let out_dir =
  let doc =
    "Path to the project directory inside which the code is to be \
     generated, relative to Go source directory [Only applicable for Go \
     Codegen]"
  in
  Arg.(value & opt (some string) None & info ["out-dir"] ~doc ~docv:"DIR")

let file = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")

let show_solver_queries =
  let doc = "Print solver queries (With RefinementTypes pragma)" in
  Arg.(value & flag & info ["show-solver-queries"] ~doc)

let cmd =
  let doc =
    "A tool to manipulate and validate Scribble-style multiparty protocols"
  in
  let man =
    [ `S Manpage.s_description
    ; `P
        "$(tname) is a toolkit to manipulate Scribble-style multiparty \
         protocols, based on classical multiparty session type theory. The \
         toolkit provides means to define global protocols, project to \
         local protocols, convert local protocols to a CFSM representation, \
         and generate OCaml code for protocol implementations."
    ; `S Manpage.s_bugs
    ; `P "Please report bugs on GitHub at %%PKG_ISSUES%%" ]
  in
  ( Term.(
      ret
        ( const main $ file $ enumerate $ verbose $ go_path $ out_dir
        $ project $ fsm $ gencode_ocaml $ gencode_monadic_ocaml $ gencode_go
        $ gencode_fstar $ sexp_global_type $ show_solver_queries ))
  , Term.info "nuscr" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits
      ~man )

let () = Term.(exit @@ eval cmd)
