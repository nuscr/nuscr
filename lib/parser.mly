

(* ---------------------------------------- *)

%token <string>IDENT

%token EOI


(* ---------------------------------------- *)
%start <Syntax.ast> main
%{ open Syntax %}
%%

let main :=
  located(raw_main)

let raw_main ==
  ~ = IDENT ; < Con >

let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
