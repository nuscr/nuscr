(** Global types *)
open Syntax

type t =
  | MessageG of message * name * name * t
  (** [MessageG (msg, sender, receiver, t)] starts by sending message [msg] from [sender] to [receiver] and continues as [t] *)
  | MuG of name * t
  (** Fixpoint *)
  | TVarG of name
  (** Recursive variable *)
  | ChoiceG of name * t list
  (** [ChoiceG (name, ts)] expresses a choice located at participant [name] between the [ts] *)
  | EndG
(** Empty global type *)

(** The type of global types *)
val show : t -> string
(** Provides a textual representation of a global type *)
val of_protocol : raw_global_protocol located -> t
(** Turn a raw protocol (from the parser) into a global type *)
val normalise : t -> t
(** Normalise a  global type. This mainly collapses nested choice on the same participant and unfolds fixpoints *)
