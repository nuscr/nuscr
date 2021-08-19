open! Base
open Names
open Printf
open Lexing

(** A Violation is reported when an impossible state was reached. It has to
    be considered a bug even when the fix is to change the Violation to a
    user error *)
exception Violation of (string * position)

exception UnImplemented of (string * position)

type user_error =
  | UnknownPragma of string
  | IncompatibleFlag of string * string
  | MissingFlag of string * string
  | PragmaNotSet of string * string
  | LexerError of string
  | ParserError of Loc.t
  | UnboundRecursionName of TypeVariableName.t
  | RedefinedRecursionName of TypeVariableName.t * Loc.t * Loc.t
  | Uncategorised of string
  | InvalidCommandLineParam of string
  | UnboundRole of RoleName.t
  | ReflexiveMessage of RoleName.t * Loc.t * Loc.t
  | UnableToMerge of string * RoleName.t option
  | RedefinedProtocol of ProtocolName.t * Loc.t * Loc.t
  | UnboundProtocol of ProtocolName.t
  | ArityMismatch of ProtocolName.t * int * int
  | InconsistentNestedChoice of RoleName.t * RoleName.t
  | RoleMismatch of RoleName.t * RoleName.t
  | DuplicateLabel of LabelName.t
  | DuplicateRoleArgs of ProtocolName.t
  | DuplicateRoleParams of ProtocolName.t
  | ChoiceCallRoleMismatch of ProtocolName.t
  | DuplicatePayloadField of LabelName.t * VariableName.t
  | FileSysErr of string
  | ProtocolNotFound of ProtocolName.t
  | IllFormedPayloadType of string
  | TypeError of string * string
  | UnknownVariableValue of RoleName.t * VariableName.t
  | UnsatisfiableRefinement (* TODO: Extra Message for error reporting *)
  | StuckRefinement (* TODO: Extra Message for error reporting *)
  | UnguardedTypeVariable of TypeVariableName.t
[@@deriving sexp_of]

(** UserError is a user error and should be reported back so it can be fixed *)
exception UserError of user_error
[@@deriving sexp_of]

let show_user_error = function
  | UnknownPragma prg -> "Unknown pragma: " ^ prg
  | IncompatibleFlag (flag, pragma) ->
      sprintf "Incompatible flag: %s set with pragma: %s" flag pragma
  | MissingFlag (flag, msg) -> sprintf "Flag: %s is not set. %s" flag msg
  | PragmaNotSet (prg, msg) -> sprintf "Pramga: %s is not set. %s" prg msg
  | LexerError msg -> "Lexer error: " ^ msg
  | ParserError interval ->
      "Parser error: An error occurred at " ^ Loc.show interval
  | UnboundRecursionName n ->
      sprintf "Unbound name %s in `continue` at %s" (TypeVariableName.user n)
        (Loc.show (TypeVariableName.where n))
  | RedefinedRecursionName (name, loc1, loc2) ->
      sprintf "Redefined name %s of `rec` at %s and %s"
        (TypeVariableName.user name)
        (Loc.show loc1) (Loc.show loc2)
  | Uncategorised msg -> "Error " ^ msg
  | InvalidCommandLineParam msg -> "Invalid command line parameter: " ^ msg
  | UnboundRole r ->
      sprintf "Unbound role %s at %s" (RoleName.user r)
        (Loc.show (RoleName.where r))
  | ReflexiveMessage (r, loc1, loc2) ->
      let loc_merge = Loc.merge loc1 loc2 in
      sprintf "Reflexive message of role %s at %s" (RoleName.user r)
        (Loc.show loc_merge)
  | UnableToMerge (s, role_opt) ->
      let role =
        Option.map
          ~f:(fun role -> "\nwhen projecting on role " ^ RoleName.user role)
          role_opt
      in
      let role = Option.value ~default:"" role in
      "Unable to merge:\n" ^ s ^ role
  | RedefinedProtocol (name, loc1, loc2) ->
      sprintf "Redefined protocol %s at %s and %s" (ProtocolName.user name)
        (Loc.show loc1) (Loc.show loc2)
  | UnboundProtocol p ->
      sprintf "Unbound protocol call %s at %s" (ProtocolName.user p)
        (Loc.show (ProtocolName.where p))
  | ArityMismatch (p, expected, actual) ->
      sprintf
        "Protocol arity mismatch, %s requires %d roles, but %d is given"
        (ProtocolName.user p) expected actual
  | InconsistentNestedChoice (r1, r2) ->
      sprintf
        "Inconsistent nested choice, a choice at %s at %s cannot be \
         followed by a choice at %s at %s"
        (RoleName.user r1)
        (Loc.show (RoleName.where r1))
        (RoleName.user r2)
        (Loc.show (RoleName.where r2))
  | RoleMismatch (expected, actual) ->
      sprintf "Expecting role %s, but got %s at %s" (RoleName.user expected)
        (RoleName.user actual)
        (Loc.show (RoleName.where actual))
  | DuplicateLabel l ->
      sprintf "Duplicate label %s in choices at %s" (LabelName.user l)
        (Loc.show (LabelName.where l))
  | DuplicateRoleArgs called_proto ->
      sprintf "Duplicate role arguments in call to protocol %s at %s"
        (ProtocolName.user called_proto)
        (Loc.show (ProtocolName.where called_proto))
  | DuplicateRoleParams protocol ->
      sprintf "Duplicate role parameter in declaration of protocol %s at %s"
        (ProtocolName.user protocol)
        (Loc.show (ProtocolName.where protocol))
  | ChoiceCallRoleMismatch called_proto ->
      sprintf
        "Invalid call to protocol '%s' in choice at %s\n\
         Some role participating in call must receive first message in all \
         branches"
        (ProtocolName.user called_proto)
        (Loc.show (ProtocolName.where called_proto))
  | DuplicatePayloadField (label, field) ->
      sprintf "Duplicate field name '%s' in message '%s' at %s"
        (VariableName.user field) (LabelName.user label)
        (Loc.show (LabelName.where label))
  | FileSysErr msg -> "File System Error: " ^ msg
  | ProtocolNotFound p -> "Cannot find protocol: " ^ ProtocolName.user p
  | IllFormedPayloadType ty -> "Ill-formed payload type: " ^ ty
  | TypeError (expr, ty) ->
      sprintf "Type Error: Expression %s should be of type %s" expr ty
  | UnknownVariableValue (role, var) ->
      sprintf "Role %s does not know the value of the variable %s"
        (RoleName.user role) (VariableName.user var)
  | UnsatisfiableRefinement -> "Refinements cannot be satisfied"
  | StuckRefinement -> "Protocol may be stuck due to refinements"
  | UnguardedTypeVariable tv ->
      sprintf "Unguarded type variable %s at %s"
        (TypeVariableName.user tv)
        (Loc.show (TypeVariableName.where tv))

let unimpl ~here desc = UnImplemented (desc, here) |> raise

let uerr e = UserError e |> raise

let violation ~here e = Violation (e, here) |> raise

(** A convenient function for raising a violation with printf strings *)
let violationf ~here fmt = Printf.ksprintf (violation ~here) fmt
