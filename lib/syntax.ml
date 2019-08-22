
type 'a located =
  { loc: Lexing.position * Lexing.position ; value: 'a }

(* type ast =
 *   raw_ast located
 *
 * and raw_ast =
 *   | Con of string *)

(* a simple name *)
type name = string

(* a qualified name *)
type raw_qname = name list
type qname = raw_qname located

let qname_to_string qn =
  String.concat "." qn

type annotation = string

type raw_mod_decl = { module_name: qname }
type mod_decl = raw_mod_decl located

type raw_type_decl =
  { domain: string (* where does the type come from java|xsd|... *)
  ; type_spec: string (* the spec of the type in its own domain *)
  ; location: string (* location of the the type definition *)
  ; type_name: string (* the name of the defined type *)
  ; is_type: bool (* true for types, false for signatures *)
  }
type type_decl = raw_type_decl located

type payloadt =
  | PayloadName of name
  | PayloadDel of name * name (* protocol @ role *)
  | PayloadQName of qname
  | PayloadBnd of name * qname (* var : type *)

type message = Message of { name:  name; payload: payloadt list }
             | MessageName of name
             | MessageQName of qname

type global_interaction = raw_global_interaction located
and raw_global_interaction =
  MessageTransfer of
    { message : message
    ; from_role : name
    ; to_roles : name list
    ; ann : annotation option
    }
  (* recursion variable, protocol *)
  | Recursion of name * global_interaction list
  | Continue of name
  (* role, protocol options *)
  | Choice of name * global_interaction list list
  (* protocol * non role args * roles *)
  | Do of name * message list * name list * annotation option
  (* message, from, to *)
  | Connect of message option * name * name * annotation option
  | Disconnect of name * name

type protocol_mods = Aux | AuxExplicit | Explicit

type raw_global_protocol =
  { name: name
  ; options: protocol_mods option
  (* if parameter is ("foo", None) it's a type *)
  (* if parameter is ("foo", Some "bar") it's a sig *)
  (* neither case I know what it is *)
  ; parameters: (name * name option) list
  ; roles: name list
  ; interactions: global_interaction list
  ; ann : annotation option
  }

type global_protocol = raw_global_protocol located

type scr_module =
  { decl: mod_decl
  ; types: type_decl list
  ; protocols : global_protocol list
  }
