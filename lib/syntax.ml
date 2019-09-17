let render_pos pos =
  Printf.sprintf "%d:%d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let render_pos_interval (startp, endp) : string =
  Printf.sprintf "%s to %s in: %s" (render_pos startp) (render_pos endp)
    startp.Lexing.pos_fname

type 'a located =
  { loc: Lexing.position * Lexing.position
        [@printer
          fun fmt interval ->
            fprintf fmt "%s" (render_pos_interval interval)]
  ; value: 'a [@main] }
[@@deriving show, make]

(* type ast =
 *   raw_ast located
 *
 * and raw_ast =
 *   | Con of string *)

(* a simple name *)
type name = string [@@deriving show]

(* a qualified name *)
type raw_qname = name list [@@deriving show]

type qname = raw_qname located [@@deriving show]

let qname_to_string qn = String.concat "." qn

type annotation = string [@@deriving show]

type raw_mod_decl = {module_name: qname} [@@deriving show]

type mod_decl = raw_mod_decl located [@@deriving show]

type raw_type_decl =
  { domain: string (* where does the type come from java|xsd|... *)
  ; type_spec: string (* the spec of the type in its own domain *)
  ; location: string (* location of the the type definition *)
  ; type_name: string (* the name of the defined type *)
  ; is_type: bool (* true for types, false for signatures *) }
[@@deriving show]

type type_decl = raw_type_decl located [@@deriving show]

type payloadt =
  | PayloadName of name
  | PayloadDel of name * name (* protocol @ role *)
  | PayloadQName of qname
  | PayloadBnd of name * qname (* var : type *)
[@@deriving show]

type message =
  | Message of {name: name; payload: payloadt list}
  | MessageName of name
  | MessageQName of qname
[@@deriving show]

type global_interaction = raw_global_interaction located [@@deriving show]

and raw_global_interaction =
  | MessageTransfer of
      { message: message
      ; from_role: name
      ; to_roles: name list
      ; ann: annotation option }
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
[@@deriving show]

type protocol_mods = Aux | AuxExplicit | Explicit [@@deriving show]

type raw_global_protocol =
  { name: name
  ; options: protocol_mods option
        (* if parameter is ("foo", None) it's a type *)
        (* if parameter is ("foo", Some "bar") it's a sig *)
        (* neither case I know what it is *)
  ; parameters: (name * name option) list
  ; rec_parameters: (name * name) list (* parameters for the recursion *)
  ; roles: name list
  ; interactions: global_interaction list
  ; ann: annotation option }
[@@deriving show]

type global_protocol = raw_global_protocol located [@@deriving show]

type scr_module =
  {decl: mod_decl; types: type_decl list; protocols: global_protocol list}
[@@deriving show]
