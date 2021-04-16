open! Base
open! Stdio

module Toplevel = struct
  open Syntax
  open Err
  open Names

  type scr_module = Syntax.scr_module

  let load_pragmas ast = Pragma.load_from_pragmas ast.pragmas

  let set_filename (fname : string) (lexbuf : Lexing.lexbuf) =
    lexbuf.Lexing.lex_curr_p <-
      {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname= fname} ;
    lexbuf

  let parse_from_lexbuf lexbuf : scr_module =
    try Parser.scr_module Lexer.token lexbuf with
    | Lexer.LexError msg -> uerr (LexerError msg)
    | Parser.Error ->
        let err_interval =
          (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
        in
        uerr (ParserError (Loc.create err_interval))
    | Err.UserError e -> uerr e
    | e -> Err.violation ~here:[%here] ("Found a problem:" ^ Exn.to_string e)

  let parse fname (ch : In_channel.t) : scr_module =
    let lexbuf = set_filename fname (Lexing.from_channel ch) in
    parse_from_lexbuf lexbuf

  let parse_string string = parse_from_lexbuf @@ Lexing.from_string string

  let validate_protocols_exn (ast : scr_module) : unit =
    let show ~f ?(sep = "\n") xs =
      (* only show if verbose is on *)
      if Pragma.verbose () then
        String.concat ~sep (List.map ~f xs) |> print_endline
      else ()
    in
    let protocols = ast.protocols in
    let protocols =
      List.map ~f:(Extraction.expand_global_protocol ast) protocols
    in
    show ~f:show_global_protocol protocols ;
    let g_types =
      List.map
        ~f:(fun p -> (Gtype.of_protocol p, p.Loc.value.roles))
        protocols
    in
    (* let g_types = List.map ~f:(fun (g, roles) -> (Gtype.normalise g,
       roles)) g_types in *)
    show ~f:(fun (g, _) -> Gtype.show g) g_types ;
    if Pragma.refinement_type_enabled () then
      List.iter ~f:(fun (g, _) -> Gtype.validate_refinements_exn g) g_types ;
    let l_types =
      List.map
        ~f:(fun (g, roles) -> List.map ~f:(fun r -> Ltype.project r g) roles)
        g_types
    in
    show
      ~f:(fun ls -> String.concat ~sep:"\n" (List.map ~f:Ltype.show ls))
      l_types ;
    let efsmss = List.map ~f:(List.map ~f:Efsm.of_local_type) l_types in
    show
      ~f:(fun efsms ->
        String.concat ~sep:"\n"
          (List.map ~f:(fun (_, g) -> Efsm.show g) efsms) )
      efsmss

  let validate_nested_protocols (ast : scr_module) =
    let show ~f ~sep input =
      if Pragma.verbose () then
        print_endline (Printf.sprintf "%s%s" (f input) sep)
      else ()
    in
    Extraction.validate_calls_in_protocols ast ;
    let ast = Extraction.rename_nested_protocols ast in
    show ~f:show_scr_module ~sep:"\n---------\n\n" ast ;
    let nested_t = Gtype.nested_t_of_module ast in
    show ~f:Gtype.show_nested_t ~sep:"\n---------\n\n" nested_t ;
    let local_t = Ltype.project_nested_t nested_t in
    show ~f:Ltype.show_nested_t ~sep:"\n" local_t ;
    ()

  let validate_exn (ast : scr_module) : unit =
    if Pragma.nested_protocol_enabled () then validate_nested_protocols ast
    else (
      Extraction.ensure_no_nested_protocols ast ;
      validate_protocols_exn ast )

  let protocols_names_of (ast : scr_module) : ProtocolName.t list =
    let protocols = ast.protocols in
    let name_of_prot p =
      let {Loc.value= {name; _}; _} = p in
      name
    in
    List.map ~f:(fun p -> name_of_prot p) protocols

  let enumerate_protocols (ast : scr_module) :
      (ProtocolName.t * RoleName.t) list =
    let protocols = ast.protocols in
    let roles p =
      let {Loc.value= {name; roles; _}; _} = p in
      List.map ~f:(fun role -> (name, role)) roles
    in
    List.concat_map ~f:(fun p -> roles p) protocols

  let enumerate_nested_protocols (ast : scr_module) :
      (ProtocolName.t * RoleName.t) list =
    let nested_t = Gtype.nested_t_of_module ast in
    let enumerated =
      Map.mapi nested_t
        ~f:(fun
             ~key:protocol
             ~data:{Gtype.static_roles; Gtype.dynamic_roles; _}
           ->
          List.map (static_roles @ dynamic_roles) ~f:(fun role ->
              (protocol, role) ) )
    in
    List.concat @@ Map.data enumerated

  let enumerate (ast : scr_module) : (ProtocolName.t * RoleName.t) list =
    if Pragma.nested_protocol_enabled () then enumerate_nested_protocols ast
    else enumerate_protocols ast

  let get_global_type ast ~protocol : Gtype.t =
    Pragma.load_from_pragmas ast.pragmas ;
    let gp =
      match
        List.find
          ~f:(fun gt -> ProtocolName.equal gt.Loc.value.name protocol)
          ast.protocols
      with
      | Some gp -> gp
      | None -> uerr (ProtocolNotFound protocol)
    in
    Extraction.expand_global_protocol ast gp |> Gtype.of_protocol

  let get_global_type_literature_syntax ast ~protocol =
    let gtype = get_global_type ast ~protocol in
    LiteratureSyntax.from_gtype gtype

  let project_protocol_role ast ~protocol ~role : Ltype.t =
    get_global_type ast ~protocol |> Ltype.project role

  let project_nested_protocol ast ~protocol ~role : Ltype.t =
    let nested_t = Gtype.nested_t_of_module ast in
    let local_t = Ltype.project_nested_t nested_t in
    let local_protocol_id = Ltype.LocalProtocolId.create protocol role in
    let _, l_type = Map.find_exn local_t local_protocol_id in
    l_type

  let project_role ast ~protocol ~role : Ltype.t =
    if Pragma.nested_protocol_enabled () then
      project_nested_protocol ast ~protocol ~role
    else project_protocol_role ast ~protocol ~role

  let generate_fsm ast ~protocol ~role =
    let lt = project_role ast ~protocol ~role in
    Efsm.of_local_type lt

  let generate_go_code = Codegen.Go.generate_go_code

  let generate_ocaml_code ~monad ast ~protocol ~role =
    let fsm = generate_fsm ast ~protocol ~role in
    Ocamlcodegen.gen_code ~monad (protocol, role) fsm

  let generate_sexp ast ~protocol =
    let gp =
      match
        List.find
          ~f:(fun gt -> ProtocolName.equal gt.Loc.value.name protocol)
          ast.protocols
      with
      | Some gp -> gp
      | None -> uerr (ProtocolNotFound protocol)
    in
    let gp = Extraction.expand_global_protocol ast gp in
    let gtype = Gtype.of_protocol gp in
    let gtype = Gtype.normalise gtype in
    Sexp.to_string_hum (Gtype.sexp_of_t gtype)

  let generate_ast ~monad ast ~protocol ~role =
    let fsm = generate_fsm ast ~protocol ~role in
    Ocamlcodegen.gen_ast ~monad (protocol, role) fsm

  let generate_fstar_code ast ~protocol ~role =
    let lt = project_role ast ~protocol ~role in
    let efsm = Efsm.of_local_type lt in
    Fstarcodegen.gen_code efsm
end

include Toplevel
module Pragma = Pragma
module Expr = Expr
module Gtype = Gtype
module Ltype = Ltype
module Efsm = Efsm
module Err = Err
module Names = Names
module Loc = Loc
module LiteratureSyntax = LiteratureSyntax
