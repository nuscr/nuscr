open! Base
open Loc
open Names

type user_error =
  | UnknownPragma of string
  | IncompatibleFlag of string * string
  | MissingFlag of string * string
  | PragmaNotSet of string * string
  | LexerError of string
  | ParserError of source_loc
  | UnboundRecursionName of TypeVariableName.t
  | RedefinedRecursionName of TypeVariableName.t * source_loc * source_loc
  | Uncategorised of string
  | InvalidCommandLineParam of string
  | UnboundRole of RoleName.t
  | ReflexiveMessage of RoleName.t
  | UnableToMerge of string
  | RedefinedProtocol of ProtocolName.t * source_loc * source_loc
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
[@@deriving sexp_of]

(** UserError is a user error and should be reported back so it can be fixed *)
exception UserError of user_error
[@@deriving sexp_of]

let show_user_error = function
  | UnknownPragma prg -> "Unknown pragma: " ^ prg
  | IncompatibleFlag (flag, pragma) ->
      "Incompatible flag: " ^ flag ^ " set with pragma: " ^ pragma
  | MissingFlag (flag, msg) -> "Flag: " ^ flag ^ " is not set. " ^ msg
  | PragmaNotSet (prg, msg) -> "Pramga: " ^ prg ^ " is not set. " ^ msg
  | LexerError msg -> "Lexer error: " ^ msg
  | ParserError interval ->
      "Parser error: An error occurred at " ^ show_source_loc interval
  | UnboundRecursionName n ->
      "Unbound name " ^ TypeVariableName.user n ^ " in `continue` at "
      ^ show_source_loc (TypeVariableName.where n)
  | RedefinedRecursionName (name, interval1, interval2) ->
      "Redefined name "
      ^ TypeVariableName.user name
      ^ " of `rec` at " ^ show_source_loc interval1 ^ " and "
      ^ show_source_loc interval2
  | Uncategorised msg -> "Error " ^ msg
  | InvalidCommandLineParam msg -> "Invalid command line parameter: " ^ msg
  | UnboundRole r ->
      "Unbound role " ^ RoleName.user r ^ " at " ^ show_source_loc
      @@ RoleName.where r
  | ReflexiveMessage r ->
      "Reflexive message of Role " ^ RoleName.user r ^ " at "
      ^ show_source_loc @@ RoleName.where r
  | UnableToMerge s -> "Unable to merge: " ^ s
  | RedefinedProtocol (name, interval1, interval2) ->
      "Redefined protocol " ^ ProtocolName.user name ^ " at "
      ^ show_source_loc interval1 ^ " and " ^ show_source_loc interval2
  | UnboundProtocol p ->
      "Unbound protocol call " ^ ProtocolName.user p ^ " at "
      ^ show_source_loc @@ ProtocolName.where p
  | ArityMismatch (p, expected, actual) ->
      "Protocol arity mismatch, " ^ ProtocolName.user p ^ " requires "
      ^ Int.to_string expected ^ " roles, but " ^ Int.to_string actual
      ^ " is given"
  | InconsistentNestedChoice (r1, r2) ->
      "Inconsistent nested choice, a choice at " ^ RoleName.user r1 ^ " at "
      ^ show_source_loc (RoleName.where r1)
      ^ " cannot be followed with a choice at " ^ RoleName.user r2 ^ " at "
      ^ show_source_loc (RoleName.where r2)
  | RoleMismatch (expected, actual) ->
      "Expecting role " ^ RoleName.user expected ^ ", but got "
      ^ RoleName.user actual ^ " at "
      ^ show_source_loc (RoleName.where actual)
  | DuplicateLabel l ->
      "Duplicate label " ^ LabelName.user l ^ " in choices at "
      ^ show_source_loc (LabelName.where l)
  | DuplicateRoleArgs called_proto ->
      "Duplicate role arguments in call to protocol "
      ^ ProtocolName.user called_proto
      ^ " at "
      ^ show_source_loc (ProtocolName.where called_proto)
  | DuplicateRoleParams protocol ->
      "Duplicate role parameter in declaration of protocol "
      ^ ProtocolName.user protocol
      ^ " at "
      ^ show_source_loc (ProtocolName.where protocol)
  | ChoiceCallRoleMismatch called_proto ->
      "Invalid call to protocol '"
      ^ ProtocolName.user called_proto
      ^ "' in choice at "
      ^ show_source_loc (ProtocolName.where called_proto)
      ^ "\n\
         Some role participating in call must receive first message in all \
         branches"
  | DuplicatePayloadField (label, field) ->
      "Duplicate field name '" ^ VariableName.user field ^ "' in message '"
      ^ LabelName.user label ^ "' at " ^ show_source_loc
      @@ LabelName.where label
  | FileSysErr msg -> "File System Error: " ^ msg
  | ProtocolNotFound p -> "Cannot find protocol: " ^ ProtocolName.user p
  | IllFormedPayloadType ty -> "Ill-formed payload type: " ^ ty
  | TypeError (expr, ty) ->
      Printf.sprintf "Type Error: Expression %s should be of type %s" expr ty
  | UnknownVariableValue (role, var) ->
      Printf.sprintf "Role %s does not know the value of the variable %s"
        (RoleName.user role) (VariableName.user var)
  | UnsatisfiableRefinement -> "Refinements cannot be satisfied"
  | StuckRefinement -> "Protocol may be stuck due to refinements"

(** A Violation is reported when an impossible state was reached. It has to
    be considered a bug even when the fix is to change the Violation to a
    user error *)
exception Violation of string
[@@deriving sexp_of]

exception UnImplemented of string [@@deriving sexp_of]

let unimpl desc = UnImplemented desc |> raise

let uerr e = UserError e |> raise

let violation e = Violation e |> raise

(** A convenient function for raising a violation with printf strings *)
let violationf fmt = Printf.ksprintf violation fmt
