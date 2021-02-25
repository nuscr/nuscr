(** This module contains variables configuarations, to be set by pragmas or
    command line arguments, not to be changed for the duration of the program *)

val solver_show_queries : unit -> bool
(** Whether to display queries to SMT solvers (with RefinementTypes pragma) *)

val set_solver_show_queries : bool -> unit
(** Set solver_show_queries *)

val nested_protocol_enabled : unit -> bool
(** Whether NestedProtocol pragma is enabled *)

val set_nested_protocol : bool -> unit
(** Set nested_protocol_enabled *)

val refinement_type_enabled : unit -> bool
(** Whether RefinementTypes pragma is enabled *)

val set_refinement_type : bool -> unit
(** Set refinement_type *)

val sender_validate_refinements : unit -> bool
(** When refinement types are enabled, senders should validate refinements *)

val set_sender_validate_refinements : bool -> unit
(** Set sender_validate_refinements *)

val receiver_validate_refinements : unit -> bool
(** When refinement types are enabled, receivers should validate refinements *)

val set_receiver_validate_refinements : bool -> unit
(** Set receiver_validate_refinements *)

val verbose : unit -> bool
(** Whether to produce verbose outputs *)

val set_verbose : bool -> unit
(** Set verbose *)

val reset : unit -> unit
(** Reset all configuration to default *)
