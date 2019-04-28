

(* ---------------------------------------- *)

%token <string>IDENT
%token <string>EXTIDENT

%token EOI

%token COMMA
%token SEMICOLON
%token DOT
%token LT
%token GT
%token LPAR
%token RPAR
%token LCURLY
%token RCURLY

(* keywords from Scribble.g with original comments *)
%token MODULE_KW
%token IMPORT_KW
%token TYPE_KW
%token PROTOCOL_KW
%token GLOBAL_KW
%token LOCAL_KW
%token EXPLICIT_KW
%token AUX_KW
%token ROLE_KW
%token ACCEPT_KW
%token SELF_KW
%token SIG_KW
%token AS_KW

%token CONNECT_KW
%token DISCONNECT_KW
%token WRAP_KW
%token FROM_KW
%token TO_KW
%token CHOICE_KW
%token AT_KW
%token OR_KW
%token REC_KW
%token CONTINUE_KW
%token AND_KW  // Needed for disconnect
%token DO_KW

(* ---------------------------------------- *)
%start <Syntax.scr_module> scr_module
%{ open Syntax %}
%%

(* modules *)
let scr_module :=
  md = module_decl ;
  ts = payload_type_decl* ;
  ps = protocol_decl* ;
  EOI ;
    { {decl= md ; types = ts ; protocols = ps } }

let module_decl == located (raw_module_decl)

let raw_module_decl ==
  MODULE_KW ; nm = qname ; SEMICOLON ; { {module_name= nm} }

(* types and messages *)

//let datatype_decl := payload_type_decl

let payload_type_decl == located (raw_payload_type_decl)

let raw_payload_type_decl ==
  TYPE_KW ; LT ; d = IDENT ; GT ; ts = EXTIDENT ;
  FROM_KW ; l = EXTIDENT ; AS_KW ; tn = IDENT ;
  SEMICOLON ;
    { { domain = d
      ; type_spec = ts
      ; location = l
      ; type_name = tn} }

(* messages... to do *)

(* protocols *)

let protocol_decl == global_protocol_decl (* local pending *)

/* let global_protocol_decl == */
/*   global_protocol_hdr ; global_protocol_def */

(* nuScr extension, the keyworkd protocol is optional *)
let global_protocol_decl == located(raw_global_protocol_decl)
let raw_global_protocol_decl ==
  GLOBAL_KW ; PROTOCOL_KW? ; nm = IDENT ;
  (*paremter_decls ;*) rs = role_decls ;
  ints = global_protocol_block ;
  { { name = nm ; roles = rs ; interactions = ints } }

let role_decls == LPAR ; nms = separated_nonempty_list(COMMA, role_decl) ;
                  RPAR ; { nms }

let role_decl == ROLE_KW ; nm = IDENT ; { nm }

let global_protocol_block ==
  LCURLY ; ints = global_interaction* ; RCURLY ; { ints }

let global_interaction == located(raw_global_interaction)
let raw_global_interaction ==
  global_message_transfer
  /* | global_choice */
  /* | global_continue */
  /* | global_do */
  /* | global_connect */
  /* | global_disconnect */
  /* | global_wrap */

let global_message_transfer ==
  msg = message ; FROM_KW ; frn = IDENT ;
  TO_KW ; trns = separated_nonempty_list(COMMA, IDENT) ;
  SEMICOLON ;
  { MessageTransfer
      { message = msg
      ; from_role = frn
      ; to_roles = trns
      }
  }

(* we have a qname because that's what fixme comments says in the
   Scribble parser *)
let message ==
  message_signature /* | qname */

let message_signature ==
  /* LPAR ; payload ; RPAR */
  | nm=IDENT ; LPAR ; pars=separated_list(COMMA, payload_el) ; RPAR ;
      { { name = nm
        ; payload = pars
        }
      }
  /* | LPAR ; RPAR */

let payload_el ==
  IDENT
  /* | pn = IDENT ARROBA rn = IDENT */

(* qualified names *)

let qname == located (raw_qname)

let raw_qname ==
  separated_nonempty_list(DOT, IDENT)

(* utilities *)
let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
