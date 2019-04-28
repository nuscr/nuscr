

(* ---------------------------------------- *)

%token <string>IDENT
%token <string>EXTIDENT

%token EOI

%token SEMICOLON
%token DOT
%token LT
%token GT

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
//INSTANTIATES_KW = 'instantiates';
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
//PAR_KW = 'par';
%token AND_KW  // Needed for disconnect
/*INTERRUPTIBLE_KW = 'interruptible';
WITH_KW = 'with';
BY_KW = 'by';  /* from for interrupts is more expected, but from is
                  not good for multiple roles (generally, the comma
	          in interrupt message list and role list looks like
	          "and" rather than "or") * /
THROWS_KW = 'throws';
CATCHES_KW = 'catches';*/
%token DO_KW
//SPAWN_KW = 'spawn';


(* ---------------------------------------- *)
%start <Syntax.scr_module> scr_module
%{ open Syntax %}
%%

(* modules *)
let scr_module :=
  md = module_decl ; ts = payload_type_decl * ; EOI ;
    { {decl= md ; types = ts } }

let module_decl == located (raw_module_decl)

let raw_module_decl :=
  MODULE_KW ; nm = qname ; SEMICOLON ; { {module_name= nm} }

(* types and messages *)

//let datatype_decl := payload_type_decl

let payload_type_decl := located (raw_payload_type_decl)

let raw_payload_type_decl :=
  TYPE_KW ; LT ; d = IDENT ; GT ; ts = EXTIDENT ;
  FROM_KW ; l = EXTIDENT ; AS_KW ; tn = simple_payload_type_name ;
  SEMICOLON ;
    { { domain = d
      ; type_spec = ts
      ; location = l
      ; type_name = tn} }

(* names *)

(* simple names *)

let simple_payload_type_name == IDENT

(* qualified names *)

let qname == located (raw_qname)

let raw_qname :=
  separated_nonempty_list(DOT, IDENT)

(* utilities *)
let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
