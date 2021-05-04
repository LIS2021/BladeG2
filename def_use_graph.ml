open Graph.MatrixGraph;;
open Expr;;
open Commands;;

type node_t =
  | NVar of identifier
  | NExpr of expr
  | NRhs of rhs;;

type augmented_cmd_t =
  | ASkip
  | AFail
  | AVarAssign of identifier * rhs * node * node
  | APtrAssign of expr * expr * label
  | AArrAssign of arr * expr * expr
  | ASeq of augmented_cmd_t * augmented_cmd_t
  | AIf of expr * augmented_cmd_t * augmented_cmd_t
  | AWhile of expr * augmented_cmd_t
  | AProtect of identifier * protect * rhs;;

let rec string_of_acmd (c : augmented_cmd_t) : string =
  match c with | ASkip -> "skip"
               | AFail -> "fail"
               | AVarAssign(ide, r, _, _) -> ide ^ " := " ^ string_of_rhs r
               | APtrAssign(e1, e2, _) -> "*" ^ string_of_expr e1 ^ " := " ^ string_of_expr e2
               | AArrAssign(a, e1, e2) -> string_of_value (CstA a) ^ "[" ^ string_of_expr e1 ^ "] := " ^ string_of_expr e2
               | ASeq(c1, c2) -> string_of_acmd c1 ^ ";\n" ^ string_of_acmd c2
               | AIf(e, c1, c2) -> "if (" ^ string_of_expr e ^ ") then \n" ^ string_of_acmd c1 ^ "\nelse\n" ^ string_of_acmd c2 ^ "\nfi"
               | AWhile(e, c) -> "while (" ^ string_of_expr e ^ ") do \n" ^ string_of_acmd c ^ "\nod"
               | AProtect(ide, p, r) -> ide ^ " := Protect(" ^ string_of_rhs r ^ ")"

let rec reduce_cmd (c : augmented_cmd_t) : cmd =
  match c with
    | ASkip -> Skip
    | AFail -> Fail
    | AVarAssign(ide, r, _, _) -> VarAssign(ide, r)
    | APtrAssign(e1, e2, l) -> PtrAssign(e1, e2, l)
    | AArrAssign(a, e1, e2) -> ArrAssign(a, e1, e2)
    | ASeq(c1, c2) -> Seq(reduce_cmd c1, reduce_cmd c2)
    | AIf(e, c1, c2) -> If(e, reduce_cmd c1, reduce_cmd c2)
    | AWhile(e, c1) -> While(e, reduce_cmd c1)
    | AProtect(ide, p, r) -> Protect(ide, p, r);;

type _builder_t = {
  mutable g : node_t graph;
  ht : (node_t, node) Hashtbl.t
};;

let inf = max_int;;

let normalize_node_t (n : node_t) : node_t =
  match n with | NExpr(Var(ide))
               | NRhs(Expr(Var(ide))) -> NVar ide
               | NRhs(Expr(e)) -> NExpr e
               | _ -> n;;

open Format;;

(* This also adds the node if it's not already in the graph *)
let get_node (b : _builder_t) (n : node_t) : node =
  let n' = normalize_node_t n in
  match Hashtbl.find_opt b.ht n' with
    | Some v -> v
    | None -> let g', v = add_node b.g n' in
                b.g <- g';
                Hashtbl.add b.ht n' v;
                (* printf "%s -> " (match n' with | NVar(ide) -> "ide " ^ ide
                                               | NExpr(e) -> "expr " ^ string_of_expr e
                                               | NRhs(r) -> "rhs " ^ string_of_rhs r);
                flush stdout;
                print_node v;
                printf "\n"; *)
                v

let rec _build_du_expr (e : expr) (b : _builder_t) : unit =
  match e with | Cst(v) -> ()
               | Var(ide) -> let _ = get_node b (NVar ide) in ()
               | BinOp(e1, e2, _) ->
                  _build_du_expr e1 b;
                  _build_du_expr e2 b;
                  let n_e = get_node b (NExpr e) in
                  b.g <- connect b.g (get_node b (NExpr e1), n_e) inf;
                  b.g <- connect b.g (get_node b (NExpr e2), n_e) inf;
               | InlineIf(e1, e2, e3) ->
                  _build_du_expr e1 b;
                  _build_du_expr e2 b;
                  _build_du_expr e3 b;
                  let n_e  = get_node b (NExpr e) in
                  b.g <- connect b.g (get_node b (NExpr e1), n_e) inf;
                  b.g <- connect b.g (get_node b (NExpr e2), n_e) inf;
                  b.g <- connect b.g (get_node b (NExpr e3), n_e) inf;
               | Length(e)
               | Base(e) -> _build_du_expr e b;;

let _build_du_rhs (r : rhs) (b : _builder_t) : unit =
  match r with | Expr(e) -> _build_du_expr e b
               | PtrRead(e, _)
               | ArrayRead(_, e) ->
                    _build_du_expr e b;
                    b.g <- connect b.g (get_node b (NExpr e), sink b.g) inf;
                    b.g <- connect b.g (source b.g, get_node b (NRhs r)) inf;;

let rec _build_du_cmd (c : cmd) (b : _builder_t) : augmented_cmd_t =
  match c with | Skip -> ASkip
               | Fail -> AFail
               | VarAssign(ide, r) ->
                    _build_du_rhs r b;
                    let n_r = get_node b (NRhs r) in
                    let n_ide = get_node b (NVar ide) in
                    b.g <- connect b.g (n_r, n_ide) 1;
                    AVarAssign(ide, r, n_ide, n_r)
               | PtrAssign(e1, e2, l) ->
                    _build_du_expr e1 b;
                    _build_du_expr e2 b;
                    b.g <- connect b.g (get_node b (NExpr e1), sink b.g) inf;
                    b.g <- connect b.g (get_node b (NExpr e2), sink b.g) 0; (* cambiare costo per Spectre v1.1 o no *)
                    APtrAssign(e1, e2, l)
               | ArrAssign(a, e1, e2) ->
                    _build_du_expr e1 b;
                    _build_du_expr e2 b;
                    b.g <- connect b.g (get_node b (NExpr e1), sink b.g) inf;
                    b.g <- connect b.g (get_node b (NExpr e2), sink b.g) 0; (* cambiare costo per Spectre v1.1 o no *)
                    AArrAssign(a, e1, e2)
               | Seq(c1, c2) -> ASeq(_build_du_cmd c1 b, _build_du_cmd c2 b)
               | If(e, c1, c2) ->
                    _build_du_expr e b;
                    b.g <- connect b.g (get_node b (NExpr e), sink b.g) inf;
                    AIf(e, _build_du_cmd c1 b, _build_du_cmd c2 b)
               | While(e, c) ->
                    _build_du_expr e b;
                    b.g <- connect b.g (get_node b (NExpr e), sink b.g) inf;
                    AWhile(e, _build_du_cmd c b)
               | Protect(ide, p, r) ->
                    (* we consider already present protects as constraints and preserve them *)
                    _build_du_rhs r b;
                    AProtect(ide, p, r);;

let build_def_use (c : cmd) : augmented_cmd_t * node_t graph =
  let b = ({g = Graph.MatrixGraph.empty (); ht = Hashtbl.create 10} : _builder_t) in
  let ac = _build_du_cmd c b in
    ac, b.g;;

(* let print_graph_bello g : unit =
  let print_node_bello (v) : unit =
    print_node v;
    printf " -> [";
    List.iter (fun (u, i) -> print_node u; printf " (%d), " i) (neighbors g v);
    printf "]\n"
  in
  List.iter print_node_bello (nodes g);;

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let result, _, _ = Parser.main Scanner.token lexbuf in
  let ac, g = build_def_use result in
    printf "------\n%s\n-------\n" (string_of_acmd ac);
    print_graph_bello g;
    flush stdout *)
