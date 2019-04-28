
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

type raw_mod_decl = { module_name: qname }
type mod_decl = raw_mod_decl located


type raw_type_decl =
  { domain: string (* where does the type come from java|xsd|... *)
  ; type_spec: string (* the spec of the type in its own domain *)
  ; location: string (* location of the the type definition *)
  ; type_name: string (* the name of the defined type *)
  }
type type_decl = raw_type_decl located

type message = { name:  string; payload: string list }

type raw_global_interaction =
  MessageTransfer of
    { message : message
    ; from_role : string
    ; to_roles : string list
    }
  | Choice
type global_interaction = raw_global_interaction located

type raw_global_protocol =
  { name: string
  ; roles: string list
  ; interactions: global_interaction list
  }

type global_protocol = raw_global_protocol located

type scr_module =
  { decl: mod_decl
  ; types: type_decl list
  ; protocols : global_protocol list
  }
