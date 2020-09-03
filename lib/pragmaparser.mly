(* ---------------------------------------- *)
%token <string>IDENT

%token COMMA
%token COLON

%token EOI

(* ---------------------------------------- *)
%start <Syntax.pragmas> pragmas
%%

let pragma_value :=
  | COLON ; v = IDENT ; { v }

let pragma_decl :=
  | k = IDENT ; v = pragma_value? ; { Syntax.pragma_of_string k , v }

let pragmas :=
  | ps = separated_list(COMMA, pragma_decl) ; EOI ; { ps }
