open! Base
open Printf
open Loc

(* type ast =
 *   raw_ast located
 *
 * and raw_ast =
 *   | Con of string *)

(* association list of pragmas *)

type pragmas = (string * string option) list [@@deriving show]

(* a simple name *)
module N = Name.Name

type name = N.t [@@deriving show {with_path= false}, sexp_of]

let equal_name = N.equal

let compare_name = N.compare

type annotation = string [@@deriving show {with_path= false}]

type raw_mod_decl = {module_name: name} [@@deriving show {with_path= false}]

type mod_decl = raw_mod_decl located option
[@@deriving show {with_path= false}]

type raw_type_decl =
  { domain: string (* where does the type come from java|xsd|... *)
  ; type_spec: string (* the spec of the type in its own domain *)
  ; location: string (* location of the the type definition *)
  ; type_name: string (* the name of the defined type *)
  ; is_type: bool (* true for types, false for signatures *) }
[@@deriving show {with_path= false}]

type type_decl = raw_type_decl located [@@deriving show {with_path= false}]

type payloadt =
  | PayloadName of name
  | PayloadDel of name * name (* protocol @ role *)
  | PayloadBnd of name * name
[@@deriving eq, ord]

(* var : type *)

let show_payloadt = function
  | PayloadName n -> N.user n
  | PayloadDel (p, r) -> sprintf "%s @ %s" (N.user p) (N.user r)
  | PayloadBnd (n, n') -> sprintf "%s: %s" (N.user n) (N.user n')

let pp_payloadt fmt p = Caml.Format.fprintf fmt "%s" (show_payloadt p)

type message =
  | Message of {name: name; payload: payloadt list}
  | MessageName of name
[@@deriving eq, ord]

let show_message = function
  | Message {name; payload} ->
      sprintf "%s(%s)" (N.user name)
        (String.concat ~sep:", " (List.map ~f:show_payloadt payload))
  | MessageName n -> N.user n

let pp_message fmt m = Caml.Format.fprintf fmt "%s" (show_message m)

let sexp_of_message m = Sexp.Atom (show_message m)

let message_label = function
  | Message {name; _} -> N.user name
  | MessageName name -> N.user name

let message_payload_ty =
  let payload_type_of_payload_t = function
    | PayloadName n -> N.user n
    | PayloadDel (_p, _r) -> failwith "Delegation is not supported"
    | PayloadBnd (_n, n) -> N.user n
  in
  function
  | Message {payload; _} -> List.map ~f:payload_type_of_payload_t payload
  | _ -> []

type global_interaction = raw_global_interaction located
[@@deriving show {with_path= false}]

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
  (* caller * protocol * non role args * roles *)
  | Calls of name * name * message list * name list * annotation option
[@@deriving show {with_path= false}]

type protocol_mods = Aux | AuxExplicit | Explicit
[@@deriving show {with_path= false}]

type raw_global_protocol =
  { name: name
  ; options: protocol_mods option
        (* if parameter is ("foo", None) it's a type *)
        (* if parameter is ("foo", Some "bar") it's a sig *)
        (* neither case I know what it is *)
  ; parameters: (string * string option) list
  ; rec_parameters: (name * annotation) list
        (* parameters for the recursion *)
  ; roles: name list
  ; split_roles: name list * name list
  ; nested_protocols: raw_global_protocol located list
  ; interactions: global_interaction list
  ; ann: annotation option }
[@@deriving show {with_path= false}]

type global_protocol = raw_global_protocol located
[@@deriving show {with_path= false}]

type scr_module =
  { decl: mod_decl
  ; types: type_decl list
  ; nested_protocols: global_protocol list
  ; protocols: global_protocol list }
[@@deriving show {with_path= false}]
