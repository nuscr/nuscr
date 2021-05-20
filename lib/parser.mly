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
%token TILDE
%token PLUS
%token MINUS
%token LTEQ
%token GTEQ

%token NOT_KW
%token TRUE_KW
%token FALSE_KW

%left AMPAMP BARBAR
%left EQUAL LTGT LTEQ GTEQ LT GT
%left PLUS MINUS
%nonassoc UMINUS TILDE

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
%{ open Syntax
   open Loc
   module Name = Name.Name

  (* list of 'a list option *)
  let loalo = function None -> [] | Some n -> n
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
  md = module_decl? ;
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
  opts = protocol_options? ; protocol_hdr ; nm = name ;
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
  nested_hdr ; nm = name ;
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
  GLOBAL_KW ; PROTOCOL_KW?
  | PROTOCOL_KW

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
                         new_nms = new_role_decls? ; RPAR ; { (nms, loalo new_nms) }

let role_decl == ROLE_KW ; nm = name ; { nm }


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
  DO_KW ; nm = name ; nra = non_role_args? ;
  ra = role_args? ; SEMICOLON ; ann = annotation? ;
  { Do (nm, loalo nra, loalo ra, ann) }

let global_calls ==
  caller = name ; CALLS_KW ; nm = name ; nra = non_role_args? ;
  ra = role_args? ; SEMICOLON ; ann = annotation? ;
  { Calls (caller, nm, loalo nra, loalo ra, ann) }

let role_args ==
  LPAR ; nm = separated_nonempty_list(COMMA, name) ; RPAR ; { nm }

let non_role_args ==
  LT ; nras = separated_nonempty_list(COMMA, non_role_arg) ; GT ; { nras }

(* grammatically the same as message  but may use a qualified name *)

let non_role_arg ==
  msg = message_signature ; { msg }
  | nm = qname ; < MessageName >

let global_choice ==
  CHOICE_KW ; AT_KW ; ~ = name ;
  ~ = separated_nonempty_list(OR_KW, global_protocol_block) ;
  < Choice >

let global_continue ==
  | CONTINUE_KW ; n = name ; LSQUARE; exprs = separated_list(COMMA, expr); RSQUARE; SEMICOLON ; { Continue(n, exprs) }
  | CONTINUE_KW ; n = name ; SEMICOLON ; { Continue(n, []) }

let global_recursion ==
  | REC_KW ; n = name ; LSQUARE ;
    rec_vars = separated_list(COMMA, rec_var); RSQUARE;
    g = global_protocol_block ;
    { Recursion(n, rec_vars, g) }
  | REC_KW ; n = name ; g = global_protocol_block ; { Recursion(n, [], g) }

let rec_var ==
  | var = name; LT;
    roles = separated_nonempty_list(COMMA, name);
    GT; COLON;
    ty = payload_el;
    EQUAL;
    init = expr;
    { { var ; roles ; ty ; init } }
  | var = name; LT;
    roles = separated_nonempty_list(COMMA, name);
    GT; COLON;
    LPAR ;
    ty = payload_el;
    RPAR ;
    EQUAL;
    init = expr;
    { { var ; roles ; ty ; init } }

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
  (* LPAR ; payload ; RPAR *)
  | nm=name ; LPAR ; pars=separated_list(COMMA, payload_el) ; RPAR ;
      { Message { name = nm
                ; payload = pars
                }
      }


let payload_el ==
  (* protocol @ role (delegation) *)
  | n1 = name ; ARROBA ; n2 = name  ; { PayloadDel(n1, n2) }
  | nm = qname ; < PayloadName >
  | v = name ; COLON ; t = name; LCURLY; e = expr; RCURLY;
    { PayloadRTy (Refined (v, t, e)) }
  | ~ = name ; COLON ; ~ = qname ; < PayloadBnd >


let annotation == ARROBA ; ann = EXTIDENT ; { ann }


(* qualified names *)
let qname == create (raw_qname)

let raw_qname ==
  names = separated_nonempty_list(DOT, IDENT); { String.concat "." names }

let name == create(raw_name)

let raw_name ==
  | i = IDENT ; { i }
  | i = INT ; { string_of_int i }

(* expressions *)
expr:
  | i = INT
    { Int i }
  | s = EXTIDENT
    { String s }
  | i = create(IDENT)
    { Var i }
  | TRUE_KW { Bool true }
  | FALSE_KW { Bool false }
  | LPAR e = expr RPAR { e }
  | e1 = expr b = binop e2 = expr
    { Binop (b, e1, e2) }
  | NOT_KW e = expr %prec TILDE
    { Unop (Not, e) }
  | MINUS e = expr %prec UMINUS
    { Unop (Neg, e) }

%inline binop:
  | PLUS { Add }
  | MINUS { Minus }
  | EQUAL { Eq }
  | LTGT { Neq }
  | LT { Lt }
  | GT { Gt }
  | LTEQ { Leq }
  | GTEQ { Geq }
  | AMPAMP { And }
  | BARBAR { Or }

(* utilities *)
let located(x) ==
  ~ = x; { { loc = build $loc; value = x } }

let create(x) ==
  ~ = x; { Name.create x (build $loc)}
