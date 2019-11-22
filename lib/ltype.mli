(** Local type management *)
(** This module defines local types and basic operations on them. *)

open Syntax

type t =
  | RecvL of message * name * t
  (** [RecvL (msg, name, t)] waits for message [msg] from [name] and continues as [t] *)
  | SendL of message * name * t
  (** [SendL (msg, name, t)] sends message [msg] to [name] and continues as [t] *)
  | ChoiceL of name * t list
  (** [ChoiceL (name, ts)] is a choice (internal or external) from [name] between the [ts] *)
  | TVarL of name
  (** Recursive variable *)
  | MuL of name * t
  (** Fixpoint *)
  | EndL
(** Empty type *)
(** Local types. *)

val show : t -> string
(** Converts a local type to a string. *)

val project: Gtype.t -> name list -> name -> t
(** Project a global type into a particular role. *)
