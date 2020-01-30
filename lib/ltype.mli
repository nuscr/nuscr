(** Local type management *)

(** This module defines local types and basic operations on them. *)

open Names

(** Local types. *)
type t =
  | RecvL of Gtype.message * RoleName.t * t
      (** [RecvL (msg, name, t)] waits for message [msg] from [name] and
          continues as [t] *)
  | SendL of Gtype.message * RoleName.t * t
      (** [SendL (msg, name, t)] sends message [msg] to [name] and continues
          as [t] *)
  | ChoiceL of RoleName.t * t list
      (** [ChoiceL (name, ts)] is a choice (internal or external) from [name]
          between the [ts] *)
  | TVarL of TypeVariableName.t  (** Recursive variable *)
  | MuL of TypeVariableName.t * t  (** Fixpoint *)
  | EndL  (** Empty type *)

val show : t -> string
(** Converts a local type to a string. *)

val project : RoleName.t -> Gtype.t -> t
(** Project a global type into a particular role. *)
