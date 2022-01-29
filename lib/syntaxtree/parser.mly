(* ---------------------------------------- *)

%token <string>IDENT
%token <string>EXTIDENT
%token <int>INT

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
%token LSQUARE
%token RSQUARE
%token ARROBA

(* For expressions *)
%token AMPAMP
%token BARBAR
%token EQUAL
%token LTGT
%token PLUS
%token MINUS
%token LTEQ
%token GTEQ

%token LEN_KW
%token NOT_KW
%token TRUE_KW
%token FALSE_KW

%token RESERVED

(* keywords from Scribble.g with original comments *)
%token MODULE_KW
%token TYPE_KW
%token PROTOCOL_KW
%token GLOBAL_KW
%token NESTED_KW
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
%token CALLS_KW
%token NEW_KW

(* pragmas *)
%token PRAGMA_START
%token PRAGMA_END

(* ---------------------------------------- *)
%start <Syntax.scr_module> doc
%{
open Syntax
open Names
%}
%%

let pragma_value :=
  | COLON ; v = IDENT ; { v }

let pragma_decl :=
  | k = IDENT ; v = pragma_value? ; { Pragma.pragma_of_string k , v }

(* pragmas *)
let pragmas :=
  | PRAGMA_START; ps = separated_list(COMMA, pragma_decl) ; PRAGMA_END ; { ps }

(* document -- should use toke lexer *)

let doc :=
  m = scr_module ;
  { m }


let scr_module :=
  pgs = pragmas? ; (* Pragma must be at the beginning of a file *)
  md = option(module_decl) ;
  ts = payload_type_decl* ;
  nps = nested_protocol_decl*;
  ps = protocol_decl* ;
  EOI ;
    {
      { decl = md
      ; pragmas = Option.value ~default:[] pgs
      ; types = ts
      ; nested_protocols = nps
      ; protocols = ps }
    }


let module_decl == located (raw_module_decl)

let raw_module_decl ==
  MODULE_KW ; nm = qname ; SEMICOLON ; { { module_name= nm } }

(* types and messages *)

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

(* nuScr extension, the keyword global protocol can be shortened to either *)
let global_protocol_decl == located(raw_global_protocol_decl)
let raw_global_protocol_decl ==
  opts = protocol_options? ; protocol_hdr ; nm = protoname ;
  pars = parameter_decls? ; rs = role_decls ; rp = rec_parameter_decls? ;
  ann = annotation? ; body = global_protocol_body ;
  {
    let (nested_protos, ints) = body in
    { name = nm
    ; options = opts
    ; parameters = (match pars with Some p -> p | _ -> [])
    ; rec_parameters = (match rp with Some p -> p | _ -> [])
    ; roles = rs
    ; split_roles = (rs, [])
    ; nested_protocols = nested_protos
    ; interactions = ints
    ; ann = ann
  } }

let nested_protocol_decl == located(raw_nested_protocol_decl)
(* TODO: Remove unnecessary stuff? *)
let raw_nested_protocol_decl ==
  nested_hdr ; nm = protoname ;
  pars = parameter_decls? ; rs = nested_role_decls ; rp = rec_parameter_decls? ;
  ann = annotation? ; body = global_protocol_body ;
  {
    let (nested_protos, ints) = body in
    { name = nm
    ; options = None
    ; parameters = (match pars with Some p -> p | _ -> [])
    ; rec_parameters = (match rp with Some p -> p | _ -> [])
    ; roles = (let (rs', rs'') = rs in rs' @ rs'')
    ; split_roles = rs
    ; nested_protocols = nested_protos
    ; interactions = ints
    ; ann = ann
  } }

let protocol_hdr ==
  GLOBAL_KW ; PROTOCOL_KW? ; { () }
  | PROTOCOL_KW ; { () }

let nested_hdr ==
  NESTED_KW ; PROTOCOL_KW

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


let nested_role_decls == LPAR ; nms = separated_nonempty_list(COMMA, role_decl) ;
                         new_nms = loption(new_role_decls) ; RPAR ; { (nms, new_nms) }

let role_decl == ROLE_KW ; nm = rolename ; { nm }


let new_role_decls == SEMICOLON ; NEW_KW ;
                      nms = separated_nonempty_list(COMMA, role_decl) ; { nms }


let global_protocol_body ==
  LCURLY ; nested_protos = nested_protocol_decl*; ints = global_interaction* ;
  RCURLY ; { (nested_protos, ints) }


let global_protocol_block ==
  LCURLY ; ints = global_interaction* ; RCURLY ; { ints }


let global_interaction == located(raw_global_interaction)
let raw_global_interaction ==
  global_message_transfer
  | global_recursion
  | global_continue
  | global_choice
  | global_do
  | global_calls
let global_do ==
  DO_KW ; nm = protoname ;
  ra = loption(role_args) ; SEMICOLON ; ann = annotation? ;
  { Do (nm, ra, ann) }

let global_calls ==
  caller = rolename ; CALLS_KW ; nm = protoname ;
  ra = loption(role_args) ; SEMICOLON ; ann = annotation? ;
  { Calls (caller, nm, ra, ann) }

let role_args ==
  LPAR ; nm = separated_nonempty_list(COMMA, rolename) ; RPAR ; { nm }

let global_choice ==
  CHOICE_KW ; AT_KW ; ~ = rolename ;
  ~ = separated_nonempty_list(OR_KW, global_protocol_block) ;
  < Choice >

let global_continue ==
  | CONTINUE_KW ; n = tyvarname ;
    LSQUARE; exprs = separated_list(COMMA, expr); RSQUARE; SEMICOLON ;
    { Continue(n, exprs) }
  | CONTINUE_KW ; n = tyvarname ; SEMICOLON ; { Continue(n, []) }

let global_recursion ==
  | REC_KW ; n = tyvarname ; LSQUARE ;
    rec_vars = separated_list(COMMA, rec_var); RSQUARE;
    g = global_protocol_block ;
    { Recursion(n, rec_vars, g) }
  | REC_KW ; n = tyvarname ; g = global_protocol_block ; { Recursion(n, [], g) }

let rec_var ==
  | var = varname; LT;
    roles = separated_nonempty_list(COMMA, rolename);
    GT; COLON;
    ty = payload_el;
    EQUAL;
    init = expr;
    { { var ; roles ; ty ; init } }
  | var = varname; LT;
    roles = separated_nonempty_list(COMMA, rolename);
    GT; COLON;
    LPAR ;
    ty = payload_el;
    RPAR ;
    EQUAL;
    init = expr;
    { { var ; roles ; ty ; init } }

let global_message_transfer ==
  msg = message ; FROM_KW ; frn = rolename ;
  TO_KW ; trns = separated_nonempty_list(COMMA, rolename) ;
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
  | ~ = labelname ; < MessageName >

(* this corresponds to siglit in Scribble.g *)
let message_signature ==
  (* LPAR ; payload ; RPAR *)
  | nm = labelname ; LPAR ; pars=separated_list(COMMA, payload_el) ; RPAR ;
      { Message { name = nm
                ; payload = pars
                }
      }


let payload_el ==
  (* protocol @ role (delegation) *)
  | n1 = protoname ; ARROBA ; n2 = rolename  ; { PayloadDel(n1, n2) }
  | nm = create_payload(qname) ; < PayloadName >
  | v = varname ; COLON ; t = payloadtypename; LCURLY; e = expr; RCURLY;
    { PayloadRTy (Refined (v, t, e)) }
  | ~ = varname ; COLON ; ~ = create_payload(qname) ; < PayloadBnd >


let annotation == ARROBA ; ann = EXTIDENT ; { ann }


(* qualified names *)
let qname == raw_qname

let raw_qname ==
  names = separated_nonempty_list(DOT, IDENT); { String.concat "." names }

let name == raw_name

let varname ==
  x = raw_name; { VariableName.create x (Loc.create $loc) }

let payloadtypename ==
  x = raw_name; { PayloadTypeName.create x (Loc.create $loc) }

let protoname ==
  x = raw_name; { ProtocolName.create x (Loc.create $loc) }

let rolename ==
  x = raw_name; { RoleName.create x (Loc.create $loc) }

let tyvarname ==
  x = raw_name; { TypeVariableName.create x (Loc.create $loc) }

let labelname ==
  x = raw_name; { LabelName.create x (Loc.create $loc) }

let create_var(x) ==
  ~ = x; { VariableName.create x (Loc.create $loc) }

let create_payload(x) ==
  ~ = x; { PayloadTypeName.create x (Loc.create $loc) }

let raw_name ==
  | i = IDENT ; { i }
  | i = INT ; { string_of_int i }

(* expressions *)
atomic_expr:
  | i = INT
    { Int i }
  | s = EXTIDENT
    { String s }
  | i = create_var(IDENT)
    { Var i }
  | TRUE_KW { Bool true }
  | FALSE_KW { Bool false }
  | LPAR e = expr RPAR { e }
  | LEN_KW LPAR e = expr RPAR
    { Unop (StrLen, e) }

expr_0:
  | e1 = expr_0 b = op_0 e2 = expr_1
    { Binop (b, e1, e2) }
  | e = expr_1 { e }

%inline op_0:
  | AMPAMP { And }
  | BARBAR { Or }

expr_1:
  | e1 = expr_1 b = op_1 e2 = expr_2
    { Binop (b, e1, e2) }
  | e = expr_2 { e }

%inline op_1:
  | EQUAL { Eq }
  | LTGT { Neq }
  | LT { Lt }
  | GT { Gt }
  | LTEQ { Leq }
  | GTEQ { Geq }

expr_2:
  | e1 = expr_2 b = op_2 e2 = atomic_expr
    { Binop (b, e1, e2) }
  | e = atomic_expr { e }

%inline op_2:
  | PLUS { Add }
  | MINUS { Minus }

expr:
  | e = expr_0 { e }
  | NOT_KW e = expr
    { Unop (Not, e) }
  | MINUS e = expr
    { Unop (Neg, e) }

(* utilities *)
let located(x) ==
  ~ = x; { { Loc.loc = Loc.create $loc; Loc.value = x } }
