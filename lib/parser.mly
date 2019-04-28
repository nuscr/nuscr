

(* ---------------------------------------- *)

%token <string>IDENT

%token EOI

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
%start <Syntax.ast> main
%{ open Syntax %}
%%

let main :=
  located(raw_main)

let raw_main ==
  GLOBAL_KW ; EOI ; { Con "I am using dune now" }

let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
