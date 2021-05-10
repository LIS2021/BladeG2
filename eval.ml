module StringMap = Map.Make(String)
open Option;;
open Format;;

open Expr;;
open Commands;;
open Utils;;

class fresh_factory =
  object (self)
    val mutable p = (0 : int)

    method get =
      p <- p + 1; p
  end

let fresh = new fresh_factory;;

let stepFetch (conf : configuration) : configuration * observation =
  match conf.cs with
    | Skip :: cs1 -> {conf with cs = cs1; is = conf.is @ [Nop]}, None
    | Fail :: cs1 -> {conf with cs = cs1; is = conf.is @ [Fail(fresh#get)]}, None
    | VarAssign(ide, Expr(expr)) :: cs1 -> {conf with cs = cs1; is = conf.is @ [Assign(ide, expr)]}, None
    | VarAssign(ide, PtrRead(addr_expr, l)) :: cs1 -> {conf with cs = cs1; is = conf.is @ [Load(ide, l, addr_expr)]}, None
    | VarAssign(ide, ArrayRead(a, idx_expr)) :: cs1 -> let guard_expr = BinOp(idx_expr, Length(Cst(CstA(a))), Lt)
                                                       and then_cmd = VarAssign(ide, PtrRead(BinOp(Base(Cst(CstA(a))), idx_expr, Add), a.label)) in
                                                         {conf with cs = If(guard_expr, then_cmd, Fail) :: cs1}, None
    | PtrAssign(addr_expr, expr, l) :: cs1 -> {conf with cs = cs1; is = conf.is @ [Store(addr_expr, l, expr)]}, None
    | ArrAssign(a, idx_expr, expr) :: cs1 -> let guard_expr = BinOp(idx_expr, Length(Cst(CstA(a))), Lt)
                                             and then_cmd = PtrAssign(BinOp(Base(Cst(CstA(a))), idx_expr, Add), expr, a.label) in
                                              {conf with cs = If(guard_expr, then_cmd, Fail) :: cs1}, None
    | Seq(c1, c2) :: cs1 -> {conf with cs = c1 :: c2 :: cs1}, None
    | While(guard_expr, body_cmd) :: cs1 -> let c1 = Seq(body_cmd, While(guard_expr, body_cmd)) in
                                               {conf with cs = If(guard_expr, c1, Skip) :: cs1}, None
    (* protect *)
    | Protect(ide, Slh, ArrayRead(a, idx_expr)) :: cs1
      -> let e1 = BinOp(idx_expr, Length(Cst(CstA(a))), Lt) in (* e < length(a) *)
         let e2 = BinOp(Base(Cst(CstA(a))), idx_expr, Add) in (* base(a) + e *)
         let e3 = InlineIf(e1, Cst(CstI(Int.max_int)), Cst(CstI(0)) ) in (* e < length(a) ? 1 : 0 *)
         let e4 = BinOp(e2, e3, BitAnd) in (* (base(a) + e) land (e < length(a) ? 1 : 0) *)
         let c1 = VarAssign(ide, PtrRead(e4, a.label)) in
          {conf with cs = If(e1, c1, Fail) :: cs1}, None
    (*
compiliamo
x := protect(a[e])

come

if e < length(a) then
  x := *((base(a) + e) land (e < length(a) ? 1 : 0))
else
  fail

invece che 

r := e < length(a)
if r then
  r := r ? 1 : 0
  x := *((base(a) + e) land r)
else
  fail

come fanno nel paper, per evitare di introdurre un nome nuovo
  *)
  (* Implementiamo la protect-fence mettendo la Fence appena prima dell'Assign nelle istruzioni hw *)
    | Protect(ide, Fence, Expr(expr)) :: cs1 -> {conf with cs = cs1; is = conf.is @ [Fence; Assign(ide, expr)]}, None
    | Protect(ide, prot_kind, Expr(expr)) :: cs1 -> {conf with cs = cs1; is = conf.is @ [IProtect(ide, expr)]}, None
    | Protect(ide, prot_kind, PtrRead(addr_expr, l)) :: cs1 -> let c1 = VarAssign(ide, PtrRead(addr_expr, l))
                                                               and c2 = Protect(ide, prot_kind, Expr(Var(ide))) in
                                                                 {conf with cs = c1 :: c2 :: cs1}, None
    | Protect(ide, prot_kind, ArrayRead(a, idx_expr)) :: cs1 -> let c1 = VarAssign(ide, ArrayRead(a, idx_expr))
                                                                and c2 = Protect(ide, prot_kind, Expr(Var(ide))) in
                                                                  {conf with cs = c1 :: c2 :: cs1}, None
    | _ -> failwith "Can't Fetch"

let stepPFetch (pred : prediction) (conf : configuration) : configuration * observation =
  match conf.cs with
    | If(guard_expr, then_cmd, else_cmd) :: cs1
      -> let taken = if pred then then_cmd else else_cmd
         and not_taken = if pred then else_cmd else then_cmd in
          {conf with cs = taken :: cs1; is = conf.is @ [Guard(guard_expr, pred, not_taken :: cs1, fresh#get)]}, None
    | _ -> failwith "Can't PFetch"

let stepRetire (conf : configuration) : configuration * observation =
  match conf.is with 
    | Nop :: is1 -> {conf with is = is1}, None
    | Assign(ide, Cst(v)) :: is1 -> ({conf with rho = StringMap.add ide v conf.rho; is = is1}, None)
    | Store(Cst(CstI(addr)), _, Cst(CstI(v))) :: is1 ->
        conf.mu.(addr) <- v;
        ({conf with is = is1}, None)
    | Store(Cst(_), _, Cst(_)) :: is1 -> failwith "store of non integer values"
    | Fail(p) :: is1 -> ({conf with is = []; cs = []}, OFail(p))
    | Fence :: is1 -> ({conf with is = is1}, None)
    | _ -> failwith "Can't Retire"


(* StringMaps are immutable, phi returns a new rho *)
let rec phi (is : instruction list) (rho : value StringMap.t) : value StringMap.t =
  match is with 
    | [] -> rho
    | Assign(ide, Cst(v)) :: is1 -> phi is1 (StringMap.add ide v rho)
    | Assign(ide, _) :: is1 
    | Load(ide, _, _) :: is1 
    | IProtect(ide, _) :: is1
      -> phi is1 (StringMap.remove ide rho) 
    | _ :: is1 -> phi is1 rho


let rec eval (e: expr) (rho : value StringMap.t): value =
  match e with
    | Cst(v) -> v
    | Var(ide) -> StringMap.find ide rho
    | BinOp(e1, e2, Add) -> (match (eval e1 rho), (eval e2 rho) with 
                              | CstI(v1), CstI(v2) -> CstI(v1+v2)
                              | _,_ -> failwith("Errore di tipo: Add"))
    | BinOp(e1, e2, Lte) -> (match (eval e1 rho), (eval e2 rho) with 
                              | CstI(v1), CstI(v2) -> CstB(v1 <= v2)
                              | _,_ -> failwith("Errore di tipo: Lte"))
    | BinOp(e1, e2, Lt) -> (match (eval e1 rho), (eval e2 rho) with 
                              | CstI(v1), CstI(v2) -> CstB(v1 < v2)
                              | _,_ -> failwith("Errore di tipo: Lt"))
    | BinOp(e1, e2, BitAnd) -> (match (eval e1 rho), (eval e2 rho) with 
                              | CstI(v1), CstI(v2) -> CstI(v1 land v2) 
                              | _,_ -> failwith("Errore di tipo: BitAnd"))
    | InlineIf(e1, e2, e3) -> (match (eval e1 rho), (eval e2 rho), (eval e3 rho) with (* non serve che i due rami abbiano lo stesso tipo *)
                              | CstB(true), v, _ 
                              | CstB(false), _, v -> v
                              | _,_,_ -> failwith("Errore di tipo: if inline"))
    | Length(e) -> (match (eval e rho) with 
                              | CstA(v) -> CstI(v.length)
                              | _ -> failwith("Errore di tipo: length"))
    | Base(e) -> (match (eval e rho) with
                              | CstA(v) -> CstI(v.base)
                              | _ -> failwith("Errore di tipo: base"));;

let stepExec (n : int) (conf : configuration) : configuration * observation =
  match split conf.is n with
    | None -> failwith "Can't Exec, index out of range"
    | Some (is1, is, is2) ->
      (* The LFENCE instruction does not execute until all prior instructions have completed locally, and no later instruction begins execution until LFENCE completes.
      https://software.intel.com/content/www/us/en/develop/articles/using-intel-compilers-to-mitigate-speculative-execution-side-channel-issues.html?wapkw=lfence%20x86 *)
      if List.exists (fun i -> i = Fence) is1 = true
        then failwith "Can't Exec after a Fence"
        else try (let rho = phi is1 conf.rho in
          match is with
            | Assign(_, Cst(_)) -> failwith "Can't Exec"
            | Assign(ide, e) -> {conf with is = is1 @ [Assign(ide, Cst(eval e rho))] @ is2}, None
            | Guard(e, p, cs', id) -> 
                if eval e rho = CstB(p)
                  then {conf with is = is1 @ [Nop] @ is2}, None
                  else {conf with is = is1 @ [Nop]; cs = cs'}, Rollback(id)
            | Load(ide, l, e) -> 
                if List.exists (fun i -> match i with | Store(_,_,_) -> true | _ -> false) is1 = true 
                  then failwith "Can't Exec" 
                  else (match eval e rho with
                          | CstI(idx) -> {conf with is = is1 @ [Assign(ide, Cst(CstI(conf.mu.(idx))))] @ is2}, Read(n, []) (* controllare secondo parametro read *)
                          | _ -> failwith "memory address should be an integer"
                  )
            | Store(e1, l, e2) ->
                (match eval e1 rho, eval e2 rho with
                      | (CstI(n), CstI(v)) -> {conf with is = is1 @ [Store(Cst(CstI(n)), l, Cst(CstI(v)))] @is2}, Write(n, [])
                      | _, _ -> failwith "store of non integer values")
            | IProtect(ide, Cst(v)) ->
                if List.exists (fun i -> match i with | Guard(_,_,_,_) -> true | _ -> false) is1 = true 
                  then failwith "Can't Exec"
                  else {conf with is = is1 @ [Assign(ide, Cst(v))] @ is2}, None
            | IProtect(ide, expr) -> {conf with is = is1 @ [IProtect(ide, Cst(eval expr rho))] @ is2}, None
            | Nop -> failwith "Can't Exec"
            | Fail(_) -> failwith "Can't Exec"
            | Fence -> failwith "Can't Exec")
                with | Not_found -> failwith "Can't Exec, var not found"
                     | Invalid_argument(_) -> failwith "Can't Exec"

let step (conf : configuration) (d : directive) : configuration * observation =
  match conf.is, conf.cs, d with
      | [], [], _       -> failwith "Can't step"
      | _, _, Fetch     -> stepFetch conf
      | _, _, PFetch(b) -> stepPFetch b conf
      | _, _, Exec(n)   -> stepExec n conf
      | _, _, Retire    -> stepRetire conf;;

let jiteval (proc) (init_conf : configuration) : configuration * observation list =
  let rec _jiteval (conf : configuration) (obs_trace : observation list) : configuration * observation list =
    if (conf.is = [] && conf.cs = []) then (conf, List.rev obs_trace)
    else let direct = proc#get_next_directive conf in
         let conf', obs = step conf direct in _jiteval conf' (obs :: obs_trace)
  in _jiteval init_conf [];;
