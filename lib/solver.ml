open! Base

let run_solver input =
  let input = Bytes.of_string input in
  let output = Process.read_stdout ~stdin:input "z3" [|"-in"|] in
  String.concat ~sep:"\n" output
