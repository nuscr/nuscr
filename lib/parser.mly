

(* ---------------------------------------- *)

%token <string>IDENT

%token EOI


(* ---------------------------------------- *)
%start <Syntax.ast> main
%{ open Syntax %}
%%

main :
  e = located(raw_main) { e }

raw_main :
  str = IDENT { Con str }

located(X):
  x = X { {loc = (assert false) ; value = x } }
