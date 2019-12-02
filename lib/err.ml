open! Base
open Loc
open Syntax

type user_error =
  | LexerError of string
  | ParserError of source_loc
  | UnboundRecursionName of string * source_loc
  | RedefinedRecursionName of string * source_loc * source_loc
  | Uncategorised of string
  | InvalidCommandLineParam of string
  | UnboundRole of name
  | ReflexiveMessage of name
  | UnableToMerge of string
[@@deriving sexp_of]

exception UserError of user_error
[@@deriving sexp_of]
(** UserError is a user error and should be reported back so it can be fixed *)

let show_user_error = function
  | LexerError msg -> "Lexer error: " ^ msg
  | ParserError interval ->
      "Parser error: An error occurred at " ^ show_source_loc interval
  | UnboundRecursionName (name, interval) ->
      "Unbound name " ^ name ^ " in `continue` at "
      ^ show_source_loc interval
  | RedefinedRecursionName (name, interval1, interval2) ->
      "Redefined name " ^ name ^ " of `rec` at " ^ show_source_loc interval1
      ^ " and " ^ show_source_loc interval2
  | Uncategorised msg -> "Error " ^ msg
  | InvalidCommandLineParam msg -> "Invalid command line parameter: " ^ msg
  | UnboundRole r ->
      "Unbound role " ^ Name.user r ^ " in " ^ show_source_loc
      @@ Name.where r
  | ReflexiveMessage r ->
      "Reflexive message of Role " ^ Name.user r ^ " at " ^ show_source_loc
      @@ Name.where r
  | UnableToMerge s -> "Unable to merge: " ^ s

exception Violation of string
[@@deriving sexp_of]
(** A Violation is reported when an impossible state was reached. It has to
    be considered a bug even when the fix is to change the Violation to a
    user error *)

exception UnImplemented of string [@@deriving sexp_of]

let unimpl desc = UnImplemented desc |> raise

let uerr e = UserError e |> raise
