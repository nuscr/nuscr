open! Base

let run_solver input =
  let input = Bytes.of_string input in
  (* https://stackoverflow.com/questions/30998111/executing-a-z3-script-in-command-line-prompt
   * *)
  let output = Process.read_stdout ~stdin:input "z3" [|"-smt2"; "-in"|] in
  String.concat ~sep:"\n" output
