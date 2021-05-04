open Graph.MatrixGraph;;
open Expr;;
open Commands;;
open Def_use_graph;;
open Format;;
open Flow_network;;

module FNM = FlowNetworkMaker (Graph.MatrixGraph) (BFS (Graph.MatrixGraph))

let rec repair (c : augmented_cmd_t) (cut : node list) : cmd =
  match c with
    | ASkip -> Skip
    | AFail -> Fail
    | AVarAssign(ide, r, n_ide, n_r) ->
        (* printf "%s - " (string_of_acmd c);
        print_node n_ide;
        print_node n_r;
        printf "\n"; *)
        if (not (List.mem n_ide cut)) && ((List.mem n_r cut)) then
          (match r with | ArrayRead(_, _) -> Protect(ide, Slh, r)
                        | _ -> Protect(ide, Fence, r))
        else
          VarAssign(ide, r)
    | APtrAssign(e1, e2, l) -> PtrAssign(e1, e2, l)
    | AArrAssign(a, e1, e2) -> ArrAssign(a, e1, e2)
    | ASeq(c1, c2) -> Seq(repair c1 cut, repair c2 cut)
    | AIf(e, c1, c2) -> If(e, repair c1 cut, repair c2 cut)
    | AWhile(e, c1) -> While(e, repair c1 cut)
    | AProtect(ide, p, r) -> Protect(ide, p, r);;

let print_environment (rho : environment) : unit =
  let print_var (ide : identifier) (v : value) : unit =
    (match v with | CstI(n) -> printf "%s := %d\n" ide n
                  | CstA(a) -> printf "%s[%d]\n" ide a.length
                  | CstB(_) -> failwith "unexpected const b in initial environment")
  in
  StringMap.iter print_var rho

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let result, rho, _ = Parser.main Scanner.token lexbuf in
  let ac, g = build_def_use result in
  let cut = FNM.min_cut g in
  (* printf "\n---------\nCut: [";
  List.iter (fun v -> print_node v; printf ", ") cut;
  printf "]\n--------\n"; *)
  let repaired = repair ac cut in
    printf "%%\n";
    print_environment rho;
    printf "%%\n";    
    printf "%s\n" (string_of_cmd repaired);
    flush stdout
