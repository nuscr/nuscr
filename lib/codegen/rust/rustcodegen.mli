(** Generate Rust monitor code from EFSM *)

open Names

val gen_code : Efsm.state * Efsm.t -> protocol:ProtocolName.t -> string

val gen_test_code : Efsm.state * Efsm.t -> protocol:ProtocolName.t -> string
