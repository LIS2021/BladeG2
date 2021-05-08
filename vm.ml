open Format;;
open Expr;;
open Commands;;
open Processor;;
open Eval;;

let print_var mem ide value =
  match value with | CstA(a) -> print_string (ide ^ " = array " ^ a.name ^" [|");
                                Array.iter (fun v -> printf "%d, " v) (Array.sub mem a.base a.length);
                                print_string "|]\n"
                   | value -> print_string (ide ^ " = " ^ (string_of_value value) ^ "\n");;

let print_conf (conf : configuration) : unit =
  printf "{\n  mu: [| ";
  Array.iter (fun v -> printf "%d, " v) conf.mu;
  printf " |],\n  rho:\n";
  StringMap.iter (print_var conf.mu) conf.rho;
  printf "\n}\n";;

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let result, rho, mem_size = Parser.main Scanner.token lexbuf in
  let proc = new Processor.complex_processor in
    printf "\n-------\n%s\n------\n" (Commands.string_of_cmd result);
    let init_conf = { is = []; cs = [result]; mu = Array.make (mem_size + 100) 0 ; rho = rho} in
    let conf, obs_trace, count = jiteval proc init_conf in
      print_conf conf;
      (* printf "Observables: %s\n" (String.concat ", " (List.map string_of_obs obs_trace)); *)
      printf "Count: %d\n" count;
      proc#print_state ();
      flush stdout
