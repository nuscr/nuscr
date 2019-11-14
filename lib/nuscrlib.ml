open! Core_kernel
open Syntax
open Err
open Gtype
open Ltype

let set_filename (fname : string) (lexbuf : Lexing.lexbuf) =
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname= fname} ;
  lexbuf

let process_ch fname (ch : In_channel.t) : string =
  let lexbuf = set_filename fname (Lexing.from_channel ch) in
  try
    let ast = Parser.scr_module Lexer.token lexbuf in
    let show ~f ~sep xs = String.concat ~sep (List.map ~f xs) in
    (* show_scr_module ast *)
    let protocols = ast.protocols in
    try
      let g_types =
        List.map
          ~f:(fun p -> (global_type_of_protocol p, p.value.roles))
          protocols
      in
      let g_types_str =
        show ~sep:"\n" ~f:(fun (g, _) -> show_global_type g) g_types
      in
      let g_types =
        List.map ~f:(fun (g, roles) -> (normal_form g, roles)) g_types
      in
      let gnf_types_str =
        show ~sep:"\n" ~f:(fun (g, _) -> show_global_type g) g_types
      in
      let l_types =
        List.map
          ~f:(fun (g, roles) -> List.map ~f:(project g roles) roles)
          g_types
      in
      let l_types_str =
        String.concat ~sep:"\n"
          (List.map
             ~f:(fun ls ->
               String.concat ~sep:"\n" (List.map ~f:show_local_type ls))
             l_types)
      in
      String.concat ~sep:"\n\n\n" [g_types_str; gnf_types_str; l_types_str]
    with UnImplemented desc ->
      "I'm sorry, it is unfortunate " ^ desc ^ " is not implemented"
  with
  | Lexer.LexError msg -> Err.UserError (LexerError msg) |> raise
  | Parser.Error ->
      let err_interval =
        (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      Err.UserError (ParserError err_interval) |> raise
  | e -> Err.Violation ("Found a problem:" ^ Exn.to_string e) |> raise

let process_file (fn : string) : string =
  let input = In_channel.create fn in
  let res = process_ch fn input in
  In_channel.close input ; res
