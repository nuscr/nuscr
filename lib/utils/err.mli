(** Error reporting *)
open Names

(** A user error is an error found in the protocols *)
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
  | ReflexiveMessage of RoleName.t
  | UnableToMerge of string
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
[@@deriving sexp_of]

(** UserError is a user error and should be reported back so it can be fixed *)
exception UserError of user_error
[@@deriving sexp_of]

val show_user_error : user_error -> string

(** A Violation is reported when an impossible state was reached. It has to
    be considered a bug even when the fix is to change the Violation to a
    user error *)
exception Violation of string
[@@deriving sexp_of]

(** An Unimplemented is reported when certain features are not implemented,
    the string attached provides a description of the feature missing. *)
exception UnImplemented of string
[@@deriving sexp_of]

(** {2 Shortcuts for raising exceptions} *)

val uerr : user_error -> 'a
(** Raise an user error *)

val unimpl : string -> 'a
(** Raise an Unimplemented error *)

val violationf : ('a, unit, string, 'b) format4 -> 'a
(** Raise a violation, with the possibility of using format strings *)
