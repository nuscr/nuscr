open! Base

type t =
  | NestedProtocols
  | ShowPragmas
  | PrintUsage
  | RefinementTypes
  | SenderValidateRefinements
  | ReceiverValidateRefinements
  | ValidateRefinementSatisfiability
  | ValidateRefinementProgress
  | ErrorHandlingCrashBranch
[@@deriving show]

let pragma_of_string str : t =
  match str with
  | "ShowPragmas" -> ShowPragmas
  | "PrintUsage" -> PrintUsage
  | "NestedProtocols" -> NestedProtocols
  | "RefinementTypes" -> RefinementTypes
  | "SenderValidateRefinements" -> SenderValidateRefinements
  | "ReceiverValidateRefinements" -> ReceiverValidateRefinements
  | "ValidateRefinementSatisfiability" -> ValidateRefinementSatisfiability
  | "ValidateRefinementProgress" -> ValidateRefinementProgress
  | "ErrorHandlingCrashBranch" -> ErrorHandlingCrashBranch
  | prg -> Err.UnknownPragma prg |> Err.uerr

type pragmas = (t * string option) list [@@deriving show]

type config =
  { solver_show_queries: bool
  ; nested_protocol_enabled: bool
  ; refinement_type_enabled: bool
  ; sender_validate_refinements: bool
  ; receiver_validate_refinements: bool
  ; validate_refinement_satisfiability: bool
  ; validate_refinement_progress: bool
  ; error_handling_crash_branch: bool
  ; verbose: bool }

let default =
  { solver_show_queries= false
  ; nested_protocol_enabled= false
  ; refinement_type_enabled= false
  ; sender_validate_refinements= false
  ; receiver_validate_refinements= false
  ; validate_refinement_satisfiability= false
  ; validate_refinement_progress= false
  ; error_handling_crash_branch= false
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

let sender_validate_refinements () = !config.sender_validate_refinements

let set_sender_validate_refinements sender_validate_refinements =
  config := {!config with sender_validate_refinements}

let receiver_validate_refinements () = !config.receiver_validate_refinements

let set_receiver_validate_refinements receiver_validate_refinements =
  config := {!config with receiver_validate_refinements}

let validate_refinement_satisfiability () =
  !config.validate_refinement_satisfiability

let set_validate_refinement_satisfiability validate_refinement_satisfiability
    =
  config := {!config with validate_refinement_satisfiability}

let validate_refinement_progress () = !config.validate_refinement_progress

let set_validate_refinement_progress validate_refinement_progress =
  config := {!config with validate_refinement_progress}

let error_handling_crash_branch () = !config.error_handling_crash_branch

let set_error_handling_crash_branch error_handling_crash_branch =
  config := {!config with error_handling_crash_branch}

let verbose () = !config.verbose

let set_verbose verbose = config := {!config with verbose}

let reset () = config := default

let validate_config () =
  if
    !config.sender_validate_refinements
    && not !config.refinement_type_enabled
  then
    Err.uerr
      (Err.PragmaNotSet
         ( show RefinementTypes
         , "This is required by SenderValidateRefinements" ) ) ;
  if
    !config.receiver_validate_refinements
    && not !config.refinement_type_enabled
  then
    Err.uerr
      (Err.PragmaNotSet
         ( show RefinementTypes
         , "This is required by ReceiverValidateRefinements" ) ) ;
  if
    !config.validate_refinement_satisfiability
    && not !config.refinement_type_enabled
  then
    Err.uerr
      (Err.PragmaNotSet
         ( show RefinementTypes
         , "This is required by ValidateRefinementSatisfiabiltiy" ) ) ;
  if !config.refinement_type_enabled && !config.nested_protocol_enabled then
    Err.uerr
      (Err.IncompatibleFlag (show RefinementTypes, show NestedProtocols))

let load_from_pragmas pragmas =
  let process_global_pragma (k, v) =
    match (k, v) with
    | NestedProtocols, _ -> set_nested_protocol true
    | RefinementTypes, _ -> set_refinement_type true
    | SenderValidateRefinements, _ -> set_sender_validate_refinements true
    | ReceiverValidateRefinements, _ ->
        set_receiver_validate_refinements true
    | ValidateRefinementSatisfiability, _ ->
        set_validate_refinement_satisfiability true
    | ValidateRefinementProgress, _ -> set_validate_refinement_progress true
    | ErrorHandlingCrashBranch, _ -> set_error_handling_crash_branch true
    | ShowPragmas, _ | PrintUsage, _ -> ()
  in
  List.iter ~f:process_global_pragma pragmas ;
  validate_config ()
