type t =
  { solver_show_queries: bool
  ; nested_protocol_enabled: bool
  ; refinement_type_enabled: bool
  ; verbose: bool }

let default =
  { solver_show_queries= false
  ; nested_protocol_enabled= false
  ; refinement_type_enabled= false
  ; verbose= false }

let config = ref default

let solver_show_queries () = !config.solver_show_queries

let set_solver_show_queries solver_show_queries =
  config := {!config with solver_show_queries}

let nested_protocol_enabled () = !config.nested_protocol_enabled

let set_nested_protocol nested_protocol_enabled =
  config := {!config with nested_protocol_enabled}

let refinement_type_enabled () = !config.refinement_type_enabled

let set_refinement_type refinement_type_enabled =
  config := {!config with refinement_type_enabled}

let verbose () = !config.verbose

let set_verbose verbose = config := {!config with verbose}

let reset () = config := default
