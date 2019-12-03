open! Base
open Loc
open Syntax

type user_error =
  | LexerError of string
  | ParserError of source_loc
  | UnboundRecursionName of name
  | RedefinedRecursionName of string * source_loc * source_loc
  | Uncategorised of string
  | InvalidCommandLineParam of string
  | UnboundRole of name
  | ReflexiveMessage of name
  | UnableToMerge of string
  | RedefinedProtocol of name * source_loc * source_loc
  | UnboundProtocol of name
  | ArityMismatch of name * int * int
  | InconsistentNestedChoice of name * name
  | RoleMismatch of name * name
[@@deriving sexp_of]

exception UserError of user_error
[@@deriving sexp_of]
(** UserError is a user error and should be reported back so it can be fixed *)

let show_user_error = function
  | LexerError msg -> "Lexer error: " ^ msg
  | ParserError interval ->
      "Parser error: An error occurred at " ^ show_source_loc interval
  | UnboundRecursionName n ->
      "Unbound name " ^ Name.user n ^ " in `continue` at "
      ^ show_source_loc (Name.where n)
  | RedefinedRecursionName (name, interval1, interval2) ->
      "Redefined name " ^ name ^ " of `rec` at " ^ show_source_loc interval1
      ^ " and " ^ show_source_loc interval2
  | Uncategorised msg -> "Error " ^ msg
  | InvalidCommandLineParam msg -> "Invalid command line parameter: " ^ msg
  | UnboundRole r ->
      "Unbound role " ^ Name.user r ^ " at " ^ show_source_loc
      @@ Name.where r
  | ReflexiveMessage r ->
      "Reflexive message of Role " ^ Name.user r ^ " at " ^ show_source_loc
      @@ Name.where r
  | UnableToMerge s -> "Unable to merge: " ^ s
  | RedefinedProtocol (name, interval1, interval2) ->
      "Redefined protocol " ^ Name.user name ^ " at "
      ^ show_source_loc interval1 ^ " and " ^ show_source_loc interval2
  | UnboundProtocol p ->
      "Unbound protocol call " ^ Name.user p ^ " at " ^ show_source_loc
      @@ Name.where p
  | ArityMismatch (p, expected, actual) ->
      "Protocol arity mismatch, " ^ Name.user p ^ " requires "
      ^ Int.to_string expected ^ " roles, but " ^ Int.to_string actual
      ^ " is given"
  | InconsistentNestedChoice (r1, r2) ->
      "Inconsistent nested choice, a choice at " ^ Name.user r1 ^ " at "
      ^ show_source_loc (Name.where r1)
      ^ " cannot be followed with a choice at " ^ Name.user r2 ^ " at "
      ^ show_source_loc (Name.where r2)
  | RoleMismatch (expected, actual) ->
      "Expecting role " ^ Name.user expected ^ ", but got "
      ^ Name.user actual ^ " at "
      ^ show_source_loc (Name.where actual)

exception Violation of string
[@@deriving sexp_of]
(** A Violation is reported when an impossible state was reached. It has to
    be considered a bug even when the fix is to change the Violation to a
    user error *)

exception UnImplemented of string [@@deriving sexp_of]

let unimpl desc = UnImplemented desc |> raise

let uerr e = UserError e |> raise
