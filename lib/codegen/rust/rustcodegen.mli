(** Generate Rust monitor code from EFSM *)

open Names

val gen_code : Efsm.state * Efsm.t -> protocol:ProtocolName.t -> string
(** [gen_code (start, efsm) ~protocol] emits a standalone Rust monitor for
    [protocol], starting from EFSM state [start]. The generated monitor
    exposes an [Action] enum plus a monitor state machine. *)

val gen_test_code : Efsm.state * Efsm.t -> protocol:ProtocolName.t -> string
(** [gen_test_code efsm ~protocol] is like {!gen_code}, but also emits
    support definitions used by the cram tests to compile the generated
    monitor as a complete Rust source file. *)
