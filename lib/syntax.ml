
type 'a located =
  { loc: Lexing.position * Lexing.position ; value: 'a }

(* type ast =
 *   raw_ast located
 *
 * and raw_ast =
 *   | Con of string *)

(* a qualified name *)
type raw_qname = string list
type qname = raw_qname located

let qname_to_string qn =
  String.concat "." qn

type mod_decl = raw_mod_decl located
and raw_mod_decl = { module_name: qname }

type scr_module =
  { decl: mod_decl }
