open! Base

type pragma =
  | NestedProtocols
  | ShowPragmas
  | PrintUsage
  | RefinementTypes
  | SenderValidateRefinements
  | ReceiverValidateRefinements
[@@deriving show]

let pragma_of_string str : pragma =
  match str with
  | "ShowPragmas" -> ShowPragmas
  | "PrintUsage" -> PrintUsage
  | "NestedProtocols" -> NestedProtocols
  | "RefinementTypes" -> RefinementTypes
  | "SenderValidateRefinements" -> SenderValidateRefinements
  | "ReceiverValidateRefinements" -> ReceiverValidateRefinements
  | prg -> Err.UnknownPragma prg |> Err.uerr

type pragmas = (pragma * string option) list [@@deriving show]

type t =
  { solver_show_queries: bool
  ; nested_protocol_enabled: bool
  ; refinement_type_enabled: bool
  ; sender_validate_refinements: bool
  ; receiver_validate_refinements: bool
  ; verbose: bool }

let default =
  { solver_show_queries= false
  ; nested_protocol_enabled= false
  ; refinement_type_enabled= false
  ; sender_validate_refinements= false
  ; receiver_validate_refinements= false
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
         ( show_pragma RefinementTypes
         , "This is required by SenderValidateRefinements" ) ) ;
  if
    !config.receiver_validate_refinements
    && not !config.refinement_type_enabled
  then
    Err.uerr
      (Err.PragmaNotSet
         ( show_pragma RefinementTypes
         , "This is required by ReceiverValidateRefinements" ) ) ;
  if !config.refinement_type_enabled && !config.nested_protocol_enabled then
    Err.uerr
      (Err.IncompatibleFlag
         (show_pragma RefinementTypes, show_pragma NestedProtocols) )

let load_from_pragmas pragmas =
  let process_global_pragma (k, v) =
    match (k, v) with
    | NestedProtocols, _ -> set_nested_protocol true
    | RefinementTypes, _ -> set_refinement_type true
    | SenderValidateRefinements, _ -> set_sender_validate_refinements true
    | ReceiverValidateRefinements, _ ->
        set_receiver_validate_refinements true
    | ShowPragmas, _ | PrintUsage, _ -> ()
  in
  List.iter ~f:process_global_pragma pragmas ;
  validate_config ()