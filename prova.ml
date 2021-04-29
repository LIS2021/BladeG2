open Format;;
open Eval;;


let print_var ide value =
  print_string(ide ^ " " ^ (string_of_value value )^ "\n");;

let print_conf (conf : configuration) : unit =
  printf "{\n  mu: [| ";
  Array.iter (fun v -> printf "%d, " v) conf.mu;
  printf " |],\n  rho:\n";
  StringMap.iter print_var conf.rho;
  printf "\n}\n";;

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let result, rho, mem_size = Parser.main Scanner.token lexbuf in
    let proc = new processor in
      printf "\n-------\n%s\n------\n" (Eval.string_of_cmd result);
      let init_conf = { is = []; cs = [result]; mu = Array.make mem_size 0 ; rho = rho} in
      let conf, obs_trace, count = jiteval proc init_conf in
        print_conf conf;
        (* String.concat ", " obs_trace; *)
        printf "Count: %d\n" count;
        flush stdout
  with Scanner.Eof ->
    exit 0
