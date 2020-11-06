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

let validate_protocols_exn (ast : scr_module) ~verbose : unit =
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

let validate_nested_protocols (ast : scr_module) ~verbose =
  let show ~f ~sep input =
    if verbose then print_endline (Printf.sprintf "%s%s" (f input) sep)
    else ()
  in
  Protocol.validate_calls_in_protocols ast ;
  let ast = Protocol.rename_nested_protocols ast in
  show ~f:show_scr_module ~sep:"\n---------\n\n" ast ;
  let global_t = Gtype.global_t_of_module ast in
  let global_t = Gtype.normalise_global_t global_t in
  show ~f:Gtype.show_global_t ~sep:"\n---------\n\n" global_t ;
  let global_t = Gtype.replace_recursion_with_nested_protocols global_t in
  show ~f:Gtype.show_global_t ~sep:"\n---------\n\n" global_t ;
  let local_t = Ltype.project_global_t global_t in
  show ~f:Ltype.show_local_t ~sep:"\n" local_t ;
  ()

let validate_exn (ast : scr_module) ~verbose : unit =
  if Pragma.nested_protocol_enabled ast then
    validate_nested_protocols ast ~verbose
  else (
    Protocol.ensure_no_nested_protocols ast ;
    validate_protocols_exn ast ~verbose )

let global_t_of_ast (ast : scr_module) : Gtype.global_t =
  let ast = Protocol.rename_nested_protocols ast in
  let global_t = Gtype.global_t_of_module ast in
  let global_t = Gtype.normalise_global_t global_t in
  Gtype.replace_recursion_with_nested_protocols global_t

let enumerate_protocols (ast : scr_module) :
    (ProtocolName.t * RoleName.t) list =
  let protocols = ast.protocols in
  let roles p =
    let {value= {name; roles; _}; _} = p in
    List.map
      ~f:(fun role -> (ProtocolName.of_name name, RoleName.of_name role))
      roles
  in
  List.concat_map ~f:(fun p -> roles p) protocols

let enumerate_nested_protocols (ast : scr_module) :
    (ProtocolName.t * RoleName.t) list =
  let global_t = global_t_of_ast ast in
  let enumerated =
    Map.mapi global_t ~f:(fun ~key:protocol ~data:((roles, roles'), _, _) ->
        List.map (roles @ roles') ~f:(fun role -> (protocol, role)))
  in
  List.concat @@ Map.data enumerated

let enumerate (ast : scr_module) : (ProtocolName.t * RoleName.t) list =
  if Pragma.nested_protocol_enabled ast then enumerate_nested_protocols ast
  else enumerate_protocols ast

let project_protocol_role ast ~protocol ~role : Ltype.t =
  let gp =
    match
      List.find
        ~f:(fun gt ->
          ProtocolName.equal (ProtocolName.of_name gt.value.name) protocol)
        ast.protocols
    with
    | Some gp -> gp
    | None -> uerr (ProtocolNotFound protocol)
  in
  let gp = Protocol.expand_global_protocol ast gp in
  let gt = Gtype.of_protocol gp in
  Ltype.project role gt

let project_nested_protocol ast ~protocol ~role : Ltype.t =
  let global_t = global_t_of_ast ast in
  let open Ltype in
  let local_t = project_global_t global_t in
  let local_protocol_id = LocalProtocolId.create protocol role in
  let _, l_type = Map.find_exn local_t local_protocol_id in
  l_type

let project_role ast ~protocol ~role : Ltype.t =
  if Pragma.nested_protocol_enabled ast then
    project_nested_protocol ast ~protocol ~role
  else project_protocol_role ast ~protocol ~role

let generate_fsm ast ~protocol ~role =
  let lt = project_role ast ~protocol ~role in
  Efsm.of_local_type lt

let write_file file_name content =
  Out_channel.with_file file_name ~f:(fun file ->
      Out_channel.output_string file content)

let create_dir dir_path =
  let _ = Unix.umask 0o000 in
  try Unix.mkdir dir_path 0o755
  with Unix.Unix_error _ ->
    uerr
      (FileSysErr
         (sprintf "Unable to create new directory: %s in path: %s" dir_path
            (Unix.getcwd ())))

let create_pkg pkg_name = create_dir (PackageName.user pkg_name)

let pkg_path pkgs =
  let str_pkgs = List.map ~f:PackageName.user pkgs in
  String.concat ~sep:"/" str_pkgs

let gen_file_path path file_name = sprintf "%s/%s" path file_name

let change_dir dir_path =
  try Unix.chdir dir_path
  with Unix.Unix_error _ ->
    uerr (FileSysErr (sprintf "Unable to change directory to: %s" dir_path))

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
  change_dir (RootDirName.user project_root) ;
  create_pkg root_pkg ;
  change_dir (PackageName.user root_pkg) ;
  write_messages () ;
  write_results () ;
  write_channels () ;
  write_invite_channels () ;
  write_entry_point () ;
  write_callbacks () ;
  write_roles ()

let generate_go_code ast ~protocol ~out_dir ~go_path =
  let protocol_pkg = protocol_pkg_name protocol in
  let is_global_protocol () =
    List.exists ast.protocols ~f:(fun {loc= _; value= proto} ->
        String.equal (Name.Name.user proto.name) (ProtocolName.user protocol))
  in
  let root_dir =
    RootDirName.of_string @@ Printf.sprintf "%s/%s" out_dir protocol_pkg
  in
  let gen_code () =
    let global_t = global_t_of_ast ast in
    ensure_unique_identifiers global_t ;
    let local_t = Ltype.project_global_t global_t in
    Gocodegen.gen_code root_dir protocol global_t local_t
  in
  let write_code_to_files result =
    let project_root =
      RootDirName.of_string
      @@ Printf.sprintf "%s/%s" (Option.value_exn go_path) out_dir
    in
    let protocol_root_pkg =
      PackageName.of_string @@ protocol_pkg_name protocol
    in
    generate_go_impl result project_root protocol_root_pkg protocol
  in
  if not (is_global_protocol ()) then
    uerr
      (InvalidCommandLineParam
         (Printf.sprintf
            "Global protocol '%s' is not defined. Implementation entrypoint \
             must be a global protocol"
            (ProtocolName.user protocol))) ;
  let result = gen_code () in
  if Option.is_some go_path then write_code_to_files result ;
  Gocodegen.show_codegen_result result protocol root_dir

let generate_ocaml_code ~monad ast ~protocol ~role =
  let fsm = generate_fsm ast ~protocol ~role in
  Codegen.gen_code ~monad (protocol, role) fsm

let generate_ast ~monad ast ~protocol ~role =
  let fsm = generate_fsm ast ~protocol ~role in
  Codegen.gen_ast ~monad (protocol, role) fsm
