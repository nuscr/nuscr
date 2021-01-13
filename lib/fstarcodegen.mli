(** Generate F* code from EFSM with variable annotations *)

val gen_code : Efsm.state * Efsm.t * Efsm.var_info -> string
