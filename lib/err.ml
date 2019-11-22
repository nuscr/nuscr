open! Core_kernel
open Syntax

type user_error =
  | LexerError of string
  | ParserError of source_loc
  | UnboundRecursionName of string * source_loc
  | RedefinedRecursionName of string * source_loc * source_loc
  | Uncategorised of string
  | InvalidCommandLineParam of string
  | UnboundRole of string * source_loc
  | NonTailRecursive of source_loc
[@@deriving sexp_of]

exception UserError of user_error
[@@deriving sexp_of]
(** UserError is a user error and should be reported back so it can be fixed *)

let show_user_error = function
  | LexerError msg -> "Lexer error: " ^ msg
  | ParserError interval ->
      "Parser error: An error occurred at " ^ render_pos_interval interval
  | UnboundRecursionName (name, interval) ->
      "Unbound name " ^ name ^ " in `continue` at "
      ^ render_pos_interval interval
  | RedefinedRecursionName (name, interval1, interval2) ->
      "Redefined name " ^ name ^ " of `rec` at "
      ^ render_pos_interval interval1
      ^ " and "
      ^ render_pos_interval interval2
  | Uncategorised msg -> "Error " ^ msg
  | InvalidCommandLineParam msg -> "Invalid command line parameter: " ^ msg
  | UnboundRole (r, interval) ->
      "Unbound role " ^ r ^ " in " ^ render_pos_interval interval
  | NonTailRecursive interval ->
      "Protocol is not tail recursive - Check "
      ^ render_pos_interval interval

exception Violation of string
[@@deriving sexp_of]
(** A Violation is reported when an impossible state was reached. It has to
    be considered a bug even when the fix is to change the Violation to a
    user error *)

exception UnImplemented of string [@@deriving sexp_of]

let unimpl desc = UnImplemented desc |> raise

let uerr e = UserError e |> raise
