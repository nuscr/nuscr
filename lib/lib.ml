open! Base
open! Stdio
open Loc
open Syntax
open Err
open Names
open Gocodegen
open Gonames
open Printf

let set_filename (fname : string) (lexbuf : Lexing.lexbuf) =
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname= fname} ;
  lexbuf

(* pragmas *)
let parse_pragmas_from_lexbuf lexbuf : Syntax.pragmas =
  try Parser.pragmas Lexer.pragma_lexer lexbuf with
  | Lexer.LexError msg -> uerr (LexerError msg)
  | Parser.Error ->
      let err_interval =
        (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      uerr (ParserError (build err_interval))
  | e ->
      Err.Violation
        ("Found a problem while parsing pragmas:" ^ Exn.to_string e)
      |> raise

let parse_pragmas fname (ch : In_channel.t) : Syntax.pragmas =
  let lexbuf = set_filename fname (Lexing.from_channel ch) in
  parse_pragmas_from_lexbuf lexbuf

let parse_pragmas_string string =
  parse_pragmas_from_lexbuf @@ Lexing.from_string string

let parse_from_lexbuf lexbuf : scr_module =
  try Parser.doc Lexer.token lexbuf with
  | Lexer.LexError msg -> uerr (LexerError msg)
  | Parser.Error ->
      let err_interval =
        (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      uerr (ParserError (build err_interval))
  | e -> Err.Violation ("Found a problem:" ^ Exn.to_string e) |> raise

let parse fname (ch : In_channel.t) : scr_module =
  let lexbuf = set_filename fname (Lexing.from_channel ch) in
  parse_from_lexbuf lexbuf

let parse_string string = parse_from_lexbuf @@ Lexing.from_string string

let validate_exn (ast : scr_module) ~verbose : unit =
  let show ~f ~sep xs =
    (* only show if verbose is on *)
    if verbose then String.concat ~sep (List.map ~f xs) |> print_endline
    else ()
  in
  let protocols = ast.protocols in
  let protocols =
    List.map ~f:(Protocol.expand_global_protocol ast) protocols
  in
  show ~sep:"\n" ~f:show_global_protocol protocols ;
  let g_types =
    List.map ~f:(fun p -> (Gtype.of_protocol p, p.value.roles)) protocols
  in
  let g_types =
    List.map ~f:(fun (g, roles) -> (Gtype.normalise g, roles)) g_types
  in
  show ~sep:"\n" ~f:(fun (g, _) -> Gtype.show g) g_types ;
  let l_types =
    List.map
      ~f:(fun (g, roles) ->
        List.map ~f:(fun r -> Ltype.project (RoleName.of_name r) g) roles)
      g_types
  in
  show ~sep:"\n"
    ~f:(fun ls -> String.concat ~sep:"\n" (List.map ~f:Ltype.show ls))
    l_types ;
  let efsmss = List.map ~f:(List.map ~f:Efsm.of_local_type) l_types in
  show ~sep:"\n"
    ~f:(fun efsms ->
      String.concat ~sep:"\n" (List.map ~f:(fun (_, g) -> Efsm.show g) efsms))
    efsmss

let enumerate (ast : scr_module) : (ProtocolName.t * RoleName.t) list =
  let protocols = ast.protocols in
  let roles p =
    let {value= {name; roles; _}; _} = p in
    List.map
      ~f:(fun role -> (ProtocolName.of_name name, RoleName.of_name role))
      roles
  in
  List.concat_map ~f:(fun p -> roles p) protocols

let project_role ast ~protocol ~role : Ltype.t =
  let gp =
    List.find_exn
      ~f:(fun gt ->
        ProtocolName.equal (ProtocolName.of_name gt.value.name) protocol)
      ast.protocols
  in
  let gp = Protocol.expand_global_protocol ast gp in
  let gt = Gtype.of_protocol gp in
  Ltype.project role gt

let generate_fsm ast ~protocol ~role =
  let lt = project_role ast ~protocol ~role in
  Efsm.of_local_type lt

let generate_code ~monad ast ~protocol ~role =
  let fsm = generate_fsm ast ~protocol ~role in
  Codegen.gen_code ~monad (protocol, role) fsm

let generate_ast ~monad ast ~protocol ~role =
  let fsm = generate_fsm ast ~protocol ~role in
  Codegen.gen_ast ~monad (protocol, role) fsm

let write_file file_name content =
  Out_channel.with_file file_name ~f:(fun file ->
      Out_channel.output_string file content)

let create_dir dir_path =
  let _ = Unix.umask 0o000 in
  try Unix.mkdir dir_path 0o755
  with Unix.Unix_error _ ->
    sprintf "Unable to create directory: %s" dir_path |> prerr_endline

let create_pkg pkg_name = create_dir (PackageName.user pkg_name)

let pkg_path pkgs =
  let str_pkgs = List.map ~f:PackageName.user pkgs in
  String.concat ~sep:"/" str_pkgs

let gen_file_path path file_name = sprintf "%s/%s" path file_name

let change_dir dir_path =
  try Unix.chdir dir_path
  with Unix.Unix_error _ ->
    sprintf "Unable to change directory to: %s" dir_path |> prerr_endline

let generate_go_impl
    { messages
    ; channels
    ; invite_channels
    ; results
    ; impl
    ; callbacks
    ; protocol_setup
    ; entry_point } project_root root_pkg first_protocol =
  let write_protocol_pkgs protocol_impls pkg_name file_name =
    create_pkg pkg_name ;
    Map.iteri protocol_impls ~f:(fun ~key:protocol ~data:impl ->
        let protocol_pkg =
          PackageName.of_string @@ protocol_pkg_name protocol
        in
        let protocol_pkg_path = pkg_path [pkg_name; protocol_pkg] in
        create_dir protocol_pkg_path ;
        let file_path = gen_file_path protocol_pkg_path file_name in
        write_file file_path impl)
  in
  let write_messages () =
    write_protocol_pkgs messages pkg_messages messages_file_name
  in
  let write_results () =
    write_protocol_pkgs results pkg_results results_file_name
  in
  let write_channels () =
    write_protocol_pkgs channels pkg_channels channels_file_name
  in
  let write_invite_channels () =
    create_pkg pkg_invitations ;
    Map.iteri invite_channels ~f:(fun ~key:protocol ~data:impl ->
        let file_name = invitations_file_name protocol in
        let file_path =
          gen_file_path (PackageName.user pkg_invitations) file_name
        in
        write_file file_path impl)
  in
  let write_entry_point () =
    create_pkg pkg_protocol ;
    let file_path =
      gen_file_path
        (PackageName.user pkg_protocol)
        (protocol_file_name first_protocol)
    in
    write_file file_path entry_point
  in
  let write_callbacks () =
    create_pkg pkg_callbacks ;
    Map.iteri callbacks ~f:(fun ~key:local_protocol ~data:impl ->
        let file_name = callbacks_file_name local_protocol in
        let file_path =
          gen_file_path (PackageName.user pkg_callbacks) file_name
        in
        write_file file_path impl)
  in
  let write_roles () =
    create_pkg pkg_roles ;
    let file_name_gen = Namegen.create () in
    let file_name_gen =
      Map.fold impl ~init:file_name_gen
        ~f:(fun ~key:local_protocol ~data:impl file_name_gen ->
          let file_name = role_impl_file_name local_protocol in
          let file_name_gen, file_name =
            Namegen.unique_name file_name_gen file_name
          in
          let file_path =
            gen_file_path (PackageName.user pkg_roles) file_name
          in
          write_file file_path impl ; file_name_gen)
    in
    let _ =
      Map.fold protocol_setup ~init:file_name_gen
        ~f:(fun ~key:protocol ~data:impl file_name_gen ->
          let file_name = protocol_setup_file_name protocol in
          let file_name_gen, file_name =
            Namegen.unique_name file_name_gen file_name
          in
          let file_path =
            gen_file_path (PackageName.user pkg_roles) file_name
          in
          write_file file_path impl ; file_name_gen)
    in
    ()
  in
  change_dir project_root ;
  create_pkg root_pkg ;
  Stdio.print_endline (Unix.getcwd ()) ;
  change_dir (PackageName.user root_pkg) ;
  write_messages () ;
  write_results () ;
  write_channels () ;
  write_invite_channels () ;
  write_entry_point () ;
  write_callbacks () ;
  write_roles ()
