open Base
open Stdio
open Nuscrlib
open Names
open Cmdliner

(* Helpers *)

let is_debug =
  Option.is_some (Sys.getenv "DEBUG")
  || Option.is_some (Sys.getenv "NUSCRDEBUG")

let show_position pos =
  let open Lexing in
  Printf.sprintf "%s: line %d" pos.pos_fname pos.pos_lnum

let parse_role_protocol_exn rp =
  match String.split rp ~on:'@' with
  | [role; protocol] ->
      Some (RoleName.of_string role, ProtocolName.of_string protocol)
  | _ ->
      Err.UserError
        (InvalidCommandLineParam
           "Role and protocol have to be for the form role@protocol" )
      |> raise

(* Arguments and options *)

type args =
  { filename: string
  ; enumerate: bool
  ; go_path: string option
  ; out_dir: string option
  ; verbose: bool
  ; show_solver_queries: bool }

let mk_args filename enumerate go_path out_dir verbose show_solver_queries =
  {filename; enumerate; go_path; out_dir; verbose; show_solver_queries}

let file = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")

let go_path =
  let doc =
    "Path to the Go source directory (the parent directory of the project \
     root) [Only applicable for Go Codegen]"
  in
  Arg.(value & opt (some dir) None & info ["go-path"] ~doc ~docv:"DIR")

let out_dir =
  let doc =
    "Path to the project directory inside which the code is to be \
     generated, relative to Go source directory [Only applicable for Go \
     Codegen]"
  in
  Arg.(value & opt (some string) None & info ["out-dir"] ~doc ~docv:"DIR")

let verbose =
  let doc = "Print extra information" in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let enumerate =
  let doc = "Enumerate the roles and protocols in the file" in
  Arg.(value & flag & info ["enum"] ~doc)

let show_solver_queries =
  let doc = "Print solver queries (With RefinementTypes pragma)" in
  Arg.(value & flag & info ["show-solver-queries"] ~doc)

let process_file (fn : string) (proc : string -> In_channel.t -> 'a) : 'a =
  let input = In_channel.create fn in
  let res = proc fn input in
  In_channel.close input ; res

let gen_output ast f = function
  | Some (role, protocol) ->
      let res = f ast protocol role in
      print_endline res
  | _ -> ()

type global_action_verb =
  | ShowGlobalType
  | ShowGlobalTypeMpstk
  | ShowGlobalTypeTex
  | ShowGlobalTypeSexp
  | ShowGlobalTypeProtobuf

type local_action_verb =
  | Project
  | ProjectMpstk
  | ProjectTex
  | ProjectProtobuf
  | Fsm
  | GencodeFstar
  | GencodeGo
  | GencodeOcaml
  | GencodeOcamlMonadic

type global_action = global_action_verb * string (* protocol *)

type local_action =
  local_action_verb * string * string (* protocol and role *)

let main args global_actions local_actions =
  let file = args.filename in
  Pragma.set_solver_show_queries args.show_solver_queries ;
  Pragma.set_verbose args.verbose ;
  try
    let ast = process_file file Nuscrlib.parse in
    Nuscrlib.load_pragmas ast ;
    Nuscrlib.validate_exn ast ;
    let () =
      List.iter
        ~f:(fun (verb, protocol) ->
          let protocol = ProtocolName.of_string protocol in
          match verb with
          | ShowGlobalType ->
              let gtype = Nuscrlib.get_global_type ~protocol ast in
              Nuscrlib.Gtype.show gtype |> print_endline
          | ShowGlobalTypeMpstk ->
              let gtype =
                Nuscrlib.get_global_type_literature_syntax ~protocol ast
              in
              Nuscrlib.LiteratureSyntax.show_gtype_mpstk gtype
              |> print_endline
          | ShowGlobalTypeTex ->
              let gtype =
                Nuscrlib.get_global_type_literature_syntax ~protocol ast
              in
              Nuscrlib.LiteratureSyntax.show_gtype_tex gtype |> print_endline
          | ShowGlobalTypeSexp ->
              Nuscrlib.generate_sexp ast ~protocol |> print_endline
          | ShowGlobalTypeProtobuf ->
              Nuscrlib.get_global_type_protobuf ~protocol ast |> print_string )
        global_actions
    in
    let () =
      List.iter
        ~f:(fun (verb, role, protocol) ->
          match verb with
          | Project ->
              Nuscrlib.project_role ast ~protocol ~role
              |> Ltype.show |> print_endline
          | ProjectMpstk ->
              Nuscrlib.project_role ast ~protocol ~role
              |> Nuscrlib.LiteratureSyntax.from_ltype
              |> Nuscrlib.LiteratureSyntax.show_ltype_mpstk |> print_endline
          | ProjectTex ->
              Nuscrlib.project_role ast ~protocol ~role
              |> Nuscrlib.LiteratureSyntax.from_ltype
              |> Nuscrlib.LiteratureSyntax.show_ltype_tex |> print_endline
          | ProjectProtobuf ->
              Nuscrlib.project_role ast ~protocol ~role
              |> Nuscrlib.ProtobufConvert.string_of_ltype |> print_string
          | Fsm ->
              if Pragma.nested_protocol_enabled () then
                Err.uerr
                  (Err.IncompatibleFlag
                     ("fsm", Pragma.show Pragma.NestedProtocols) ) ;
              Nuscrlib.generate_fsm ast ~protocol ~role
              |> snd |> Efsm.show |> print_endline
          | GencodeFstar ->
              Nuscrlib.generate_fstar_code ast ~protocol ~role
              |> print_endline
          | GencodeGo -> (
            match args.out_dir with
            | Some out_dir ->
                let impl =
                  Nuscrlib.generate_go_code ast ~protocol ~out_dir
                    ~go_path:args.go_path
                in
                print_endline impl
            | None ->
                Err.UserError
                  (Err.MissingFlag
                     ( "out-dir"
                     , "This flag must be set in order to generate go \
                        implementation" ) )
                |> raise )
          | GencodeOcaml ->
              Nuscrlib.generate_ocaml_code ~monad:false ast ~protocol ~role
              |> print_endline
          | GencodeOcamlMonadic ->
              Nuscrlib.generate_ocaml_code ~monad:true ast ~protocol ~role
              |> print_endline )
        local_actions
    in
    let () =
      if args.enumerate then
        Nuscrlib.enumerate ast
        |> List.map ~f:(fun (n, r) ->
               RoleName.user r ^ "@" ^ ProtocolName.user n )
        |> String.concat ~sep:"\n" |> print_endline
    in
    `Ok ()
  with
  | Err.UserError msg ->
      `Error (false, "User error: " ^ Err.show_user_error msg)
  | Err.Violation (msg, where) ->
      `Error
        ( false
        , Printf.sprintf "Internal Error: %s, raised at %s" msg
            (show_position where) )
  | Err.UnImplemented (desc, where) ->
      `Error
        ( false
        , Printf.sprintf
            "I'm sorry, it is unfortunate %s is not implemented (raised at \
             %s)"
            desc (show_position where) )
  | e when not is_debug ->
      `Error (false, "Reported problem:\n " ^ Exn.to_string e)

let role_proto =
  let parse input =
    match String.split input ~on:'@' with
    | [role; protocol] ->
        Ok (RoleName.of_string role, ProtocolName.of_string protocol)
    | _ ->
        Error (`Msg "Role and protocol have to be for the form role@protocol")
  in
  let print fmt (r, p) =
    Stdlib.Format.pp_print_string fmt (RoleName.user r) ;
    Stdlib.Format.pp_print_char fmt '@' ;
    Stdlib.Format.pp_print_string fmt (ProtocolName.user p)
  in
  Arg.conv (parse, print)

let project =
  let doc =
    "Project the local type for the specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value & opt_all role_proto [] & info ["project"] ~doc ~docv:"ROLE@PROTO" )

let project_mpstk =
  let doc =
    "Project the local type for the specified protocol and role. \
     <role_name>@<protocol_name>, but output in MPSTK syntax"
  in
  Arg.(
    value & opt_all role_proto []
    & info ["project-mpstk"] ~doc ~docv:"ROLE@PROTO" )

let project_tex =
  let doc =
    "Project the local type for the specified protocol and role. \
     <role_name>@<protocol_name>, but output in LaTeX format"
  in
  Arg.(
    value & opt_all role_proto []
    & info ["project-tex"] ~doc ~docv:"ROLE@PROTO" )

let project_protobuf =
  let doc =
    "Project the local type for the specified protocol and role. \
     <role_name>@<protocol_name>, but output in protobuf format"
  in
  Arg.(
    value & opt_all role_proto []
    & info ["project-protobuf"] ~doc ~docv:"ROLE@PROTO" )

let fsm =
  let doc =
    "Project the CFSM for the specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(value & opt_all role_proto [] & info ["fsm"] ~doc ~docv:"ROLE@PROTO")

let gencode_ocaml =
  let doc =
    "Generate OCaml code for specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value & opt_all role_proto []
    & info ["gencode-ocaml"] ~doc ~docv:"ROLE@PROTO" )

let gencode_fstar =
  let doc =
    "Generate OCaml code for specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value & opt_all role_proto []
    & info ["gencode-fstar"] ~doc ~docv:"ROLE@PROTO" )

let gencode_monadic_ocaml =
  let doc =
    "Generate monadic OCaml code for specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value & opt_all role_proto []
    & info ["gencode-ocaml-monadic"] ~doc ~docv:"ROLE@PROTO" )

let gencode_go =
  let doc =
    "Generate Go code for specified protocol and role. \
     <role_name>@<protocol_name>"
  in
  Arg.(
    value & opt_all role_proto []
    & info ["gencode-go"] ~doc ~docv:"ROLE@PROTO" )

let mk_local_actions project project_mpstk project_tex project_protobuf fsm
    gencode_fstar gencode_go gencode_ocaml gencode_ocaml_monadic =
  let project = List.map ~f:(fun (r, p) -> (Project, r, p)) project in
  let project_mpstk =
    List.map ~f:(fun (r, p) -> (ProjectMpstk, r, p)) project_mpstk
  in
  let project_tex =
    List.map ~f:(fun (r, p) -> (ProjectTex, r, p)) project_tex
  in
  let project_protobuf =
    List.map ~f:(fun (r, p) -> (ProjectProtobuf, r, p)) project_protobuf
  in
  let fsm = List.map ~f:(fun (r, p) -> (Fsm, r, p)) fsm in
  let gencode_fstar =
    List.map ~f:(fun (r, p) -> (GencodeFstar, r, p)) gencode_fstar
  in
  let gencode_go =
    List.map ~f:(fun (r, p) -> (GencodeGo, r, p)) gencode_go
  in
  let gencode_ocaml =
    List.map ~f:(fun (r, p) -> (GencodeOcaml, r, p)) gencode_ocaml
  in
  let gencode_ocaml_monadic =
    List.map
      ~f:(fun (r, p) -> (GencodeOcamlMonadic, r, p))
      gencode_ocaml_monadic
  in
  List.concat
    [ project
    ; project_mpstk
    ; project_tex
    ; project_protobuf
    ; fsm
    ; gencode_fstar
    ; gencode_go
    ; gencode_ocaml
    ; gencode_ocaml_monadic ]

let sexp_global_type =
  let doc =
    "Generate the S-expression for the specified protocol. <protocol_name>"
  in
  Arg.(value & opt_all string [] & info ["generate-sexp"] ~doc ~docv:"PROTO")

let show_global_type =
  let doc =
    "Print the global type for the specified protocol. <protocol_name>"
  in
  Arg.(
    value & opt_all string [] & info ["show-global-type"] ~doc ~docv:"PROTO" )

let show_global_type_mpstk =
  let doc =
    "Print the global type for the specified protocol in MPSTK syntax. \
     <protocol_name>"
  in
  Arg.(
    value & opt_all string []
    & info ["show-global-type-mpstk"] ~doc ~docv:"PROTO" )

let show_global_type_tex =
  let doc =
    "Print the global type for the specified protocol in LaTeX format. \
     <protocol_name>"
  in
  Arg.(
    value & opt_all string []
    & info ["show-global-type-tex"] ~doc ~docv:"PROTO" )

let show_global_type_protobuf =
  let doc =
    "Print the global type for the specified protocol in protobuf format. \
     <protocol_name>"
  in
  Arg.(
    value & opt_all string []
    & info ["show-global-type-protobuf"] ~doc ~docv:"PROTO" )

let mk_global_actions show_global_type show_global_type_mpstk
    show_global_type_tex show_global_type_sexp show_global_type_protobuf =
  let show_global_type =
    List.map ~f:(fun p -> (ShowGlobalType, p)) show_global_type
  in
  let show_global_type_mpstk =
    List.map ~f:(fun p -> (ShowGlobalTypeMpstk, p)) show_global_type_mpstk
  in
  let show_global_type_tex =
    List.map ~f:(fun p -> (ShowGlobalTypeTex, p)) show_global_type_tex
  in
  let show_global_type_sexp =
    List.map ~f:(fun p -> (ShowGlobalTypeSexp, p)) show_global_type_sexp
  in
  let show_global_type_protobuf =
    List.map
      ~f:(fun p -> (ShowGlobalTypeProtobuf, p))
      show_global_type_protobuf
  in
  List.concat
    [ show_global_type
    ; show_global_type_mpstk
    ; show_global_type_tex
    ; show_global_type_sexp
    ; show_global_type_protobuf ]

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
  let info = Cmd.info "nuscr" ~version:"%%VERSION%%" ~doc ~man in
  let args =
    Term.(
      const mk_args $ file $ enumerate $ go_path $ out_dir $ verbose
      $ show_solver_queries )
  in
  let global_actions =
    Term.(
      const mk_global_actions $ show_global_type $ show_global_type_mpstk
      $ show_global_type_tex $ sexp_global_type $ show_global_type_protobuf )
  in
  let local_actions =
    Term.(
      const mk_local_actions $ project $ project_mpstk $ project_tex
      $ project_protobuf $ fsm $ gencode_fstar $ gencode_go $ gencode_ocaml
      $ gencode_monadic_ocaml )
  in
  let term =
    Term.(ret (const main $ args $ global_actions $ local_actions))
  in
  Cmd.v info term

let () = Stdlib.exit (Cmd.eval cmd)
