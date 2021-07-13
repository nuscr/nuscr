(** Generate F* code from EFSM with variable annotations *)
open Mpst

val gen_code : Efsm.state * Efsm.t * Efsm.rec_var_info -> string
