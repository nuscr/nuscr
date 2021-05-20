open! Base

let run_solver input =
  if Pragma.solver_show_queries () then Stdio.print_endline (input ^ "\n") ;
  let input = Bytes.of_string input in
  (* https://stackoverflow.com/questions/30998111/executing-a-z3-script-in-command-line-prompt
   * *)
  try
    let output = Process.read_stdout ~stdin:input "z3" [|"-smt2"; "-in"|] in
    String.concat ~sep:"\n" output
  with Process.Exit.Error e ->
    Err.violationf "Solver error: %s" (Process.Exit.error_to_string e)
