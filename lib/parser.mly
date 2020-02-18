(* ---------------------------------------- *)

%token <string>IDENT
%token <string>EXTIDENT

%token EOI

%token COMMA
%token SEMICOLON
%token COLON
%token DOT
%token LT
%token GT
%token LPAR
%token RPAR
%token LCURLY
%token RCURLY
%token ARROBA

%token RESERVED

(* keywords from Scribble.g with original comments *)
%token MODULE_KW
%token TYPE_KW
%token PROTOCOL_KW
%token GLOBAL_KW
%token EXPLICIT_KW
%token AUX_KW
%token ROLE_KW
%token SIG_KW
%token AS_KW

%token FROM_KW
%token TO_KW
%token CHOICE_KW
%token AT_KW
%token OR_KW
%token REC_KW
%token CONTINUE_KW
%token DO_KW

(* ---------------------------------------- *)
%start <Syntax.scr_module> doc
%start <Syntax.pragmas> pragmas
%{ open Syntax
   open Loc
   module Name = Name.Name

  type name_or_qname = SimplName of Name.t | QName of qname

  let noq (nm : qname) : name_or_qname =
    if List.length (nm.value) = 1
    then SimplName (Name.create (List.hd nm.value) nm.loc)
    else QName nm

  (* list of 'a list option *)
  let loalo = function None -> [] | Some n -> n
%}
%%

(* pragmas -- should use pragma_lexer lexer *)

let pragma_value :=
  | COLON ; v = IDENT ; { v }

let pragma_decl :=
  | k = IDENT ; v = pragma_value? ; { k , v }

let pragmas :=
  | ps = separated_list(COMMA, pragma_decl) ; EOI ; { ps }

(* document -- should use toke lexer *)

let doc :=
  m = scr_module ;
  { m }

(* modules *)
let scr_module :=
  md = module_decl? ;
  ts = payload_type_decl* ;
  ps = protocol_decl* ;
  EOI ;
    { {decl= md ; types = ts ; protocols = ps } }

let module_decl == located (raw_module_decl)

let raw_module_decl ==
  MODULE_KW ; nm = qname ; SEMICOLON ; { { module_name= nm } }

(* types and messages *)

//let datatype_decl := payload_type_decl

let payload_type_decl == located (raw_payload_type_decl)

let raw_payload_type_decl ==
  TYPE_KW ; LT ; d = IDENT ; GT ; ts = EXTIDENT ;
  FROM_KW ; l = EXTIDENT ; AS_KW ; tn = IDENT ;
  SEMICOLON ;
    {
      { domain = d
      ; type_spec = ts
      ; location = l
      ; type_name = tn
      ; is_type = true
      }
    }
| SIG_KW ; LT ; d = IDENT ; GT ; ts = EXTIDENT ;
  FROM_KW ; l = EXTIDENT ; AS_KW ; tn = IDENT ;
  SEMICOLON ;
    { { domain = d
      ; type_spec = ts
      ; location = l
      ; type_name = tn
      ; is_type = true
      }
    }

(* messages... to do *)

(* protocols *)

let protocol_decl == global_protocol_decl (* local pending *)

/* let global_protocol_decl == */
/*   global_protocol_hdr ; global_protocol_def */

(* nuScr extension, the keyword global protocol can be shortened to either *)
let global_protocol_decl == located(raw_global_protocol_decl)
let raw_global_protocol_decl ==
  opts = protocol_options? ; protocol_hdr ; nm = name ;
  pars = parameter_decls? ; rs = role_decls ; rp = rec_parameter_decls? ;
  ann = annotation? ; ints = global_protocol_block ;
  { { name = nm
    ; options = opts
    ; parameters = (match pars with Some p -> p | _ -> [])
    ; rec_parameters = (match rp with Some p -> p | _ -> [])
    ; roles = rs
    ; interactions = ints
    ; ann = ann
  } }

let protocol_hdr ==
  GLOBAL_KW ; PROTOCOL_KW?
  | PROTOCOL_KW

let protocol_options ==
  AUX_KW ; { Aux }
  | AUX_KW ; EXPLICIT_KW ; { AuxExplicit }
  | EXPLICIT_KW ; { Explicit }

let parameter_decls ==
  LT ; pars = separated_nonempty_list(COMMA, parameter_decl) ; GT ; { pars }

(* this is not storing the difference of Type and Sig *)
let parameter_decl :=
| TYPE_KW ; nm = IDENT ; { (nm, None) }
| TYPE_KW ; nm = IDENT ; AS_KW ; nm2 = IDENT ; { (nm, Some nm2) }
| SIG_KW ; nm = IDENT ; { (nm, None) }
| SIG_KW ; nm = IDENT ; AS_KW ; nm2 = IDENT ; { (nm, Some nm2) }

let rec_parameter_decls ==
  LPAR ; pars = separated_nonempty_list(COMMA, rec_parameter_decl) ; RPAR ; { pars }

let rec_parameter_decl == nm = name ; COLON ; ann = IDENT ; { (nm, ann) }

let role_decls == LPAR ; nms = separated_nonempty_list(COMMA, role_decl) ;
                  RPAR ; { nms }

let role_decl == ROLE_KW ; nm = name ; { nm }

let global_protocol_block ==
  LCURLY ; ints = global_interaction* ; RCURLY ; { ints }

let global_interaction == located(raw_global_interaction)
let raw_global_interaction ==
  global_message_transfer
  | global_recursion
  | global_continue
  | global_choice
  | global_do

let global_do ==
  DO_KW ; nm = name ; nra = non_role_args? ;
  ra = role_args? ; SEMICOLON ; ann = annotation? ;
  { Do (nm, loalo nra, loalo ra, ann) }

let role_args ==
  LPAR ; nm = separated_nonempty_list(COMMA, name) ; RPAR ; { nm }

let non_role_args ==
  LT ; nras = separated_nonempty_list(COMMA, non_role_arg) ; GT ; { nras }

(* grammatically the same as message  but may use a qualified name *)

let non_role_arg ==
  msg = message_signature ; { msg }
  /* | ~ = IDENT ; < MessageName > */
  | nm = qname ; { match noq nm with
                   | SimplName n -> MessageName n
                   | QName n -> MessageQName n
                 }

let global_choice ==
  CHOICE_KW ; AT_KW ; ~ = name ;
  ~ = separated_nonempty_list(OR_KW, global_protocol_block) ;
  < Choice >

let global_continue ==
  CONTINUE_KW ; ~ = name ; SEMICOLON ; < Continue >

let global_recursion ==
  REC_KW ; ~ = name ; ~ = global_protocol_block ; < Recursion >

let global_message_transfer ==
  msg = message ; FROM_KW ; frn = name ;
  TO_KW ; trns = separated_nonempty_list(COMMA, name) ;
  SEMICOLON ; ann = annotation? ;
  { MessageTransfer
      { message = msg
      ; from_role = frn
      ; to_roles = trns
      ; ann = ann
      }
  }

(* we have a qname because that's what fixme comments says in the
   Scribble parser *)
let message ==
  msg = message_signature ; { msg }
  | ~ = name ; < MessageName >

(* this corresponds to siglit in Scribble.g *)
let message_signature ==
  /* LPAR ; payload ; RPAR */
  | nm=name ; LPAR ; pars=separated_list(COMMA, payload_el) ; RPAR ;
      { Message { name = nm
                ; payload = pars
                }
      }
  /* | LPAR ; RPAR */


let payload_el ==
  (* protocol @ role (delegation) *)
  | n1 = name ; ARROBA ; n2 = name  ; { PayloadDel(n1, n2) }
  | nm = qname ; { match noq nm with
                   | SimplName n -> PayloadName n
                   | QName n -> PayloadQName n
                 }
  | ~ = name ; COLON ; ~ = qname ; < PayloadBnd >


let annotation == ARROBA ; ann = EXTIDENT ; { ann }


/* let name_or_qname == */
/*   ~ = qname ; < noq > */

(* qualified names *)

let qname == located (raw_qname)

let raw_qname ==
  separated_nonempty_list(DOT, IDENT)

let name == create(raw_name)

let raw_name == IDENT

(* utilities *)
let located(x) ==
  ~ = x; { { loc = build $loc; value = x } }

let create(x) ==
  ~ = x; { Name.create x (build $loc)}
