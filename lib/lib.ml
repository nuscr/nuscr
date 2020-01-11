open! Base
open! Stdio
open Loc
open Syntax
open Err
open Names

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

let parse_pragmas_string string = parse_pragmas_from_lexbuf @@ Lexing.from_string string

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
