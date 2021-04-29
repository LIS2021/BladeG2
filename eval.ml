module StringMap = Map.Make(String)
open Option;;
open Format;;

type identifier = string;;

type op =
  | Add
  | Lte
  | Lt
  | BitAnd;;

let string_of_op (o : op) : string =
  match o with | Add -> "+"
               | Lte -> "<="
               | Lt  -> "<"
               | BitAnd -> "&"

type label = unit;;

type arr = {base : int; length : int; label : label; name : string};;

type value =
  | CstI of int
  | CstB of bool
  | CstA of arr;;

let string_of_value (v : value) : string =
  match v with | CstI n -> string_of_int n
               | CstB b -> string_of_bool b
               | CstA a -> a.name

type environment = value StringMap.t;;

type expr =
  | Cst of value
  | Var of identifier
  | BinOp of expr * expr * op
  | InlineIf of expr * expr * expr
  | Length of expr
  | Base of expr;;

let rec string_of_expr (e : expr) : string =
  match e with | Cst v -> string_of_value v
               | Var x -> x
               | BinOp(e1, e2, op) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2 ^ ")"
               | InlineIf(e1, e2, e3) -> string_of_expr e1 ^ " ? " ^ string_of_expr e2 ^ " : " ^ string_of_expr e3
               | Length(e1) -> "length(" ^ string_of_expr e1 ^ ")"
               | Base(e1) -> "base(" ^ string_of_expr e1 ^ ")"

type rhs =
  | Expr of expr
  | PtrRead of expr * label
  | ArrayRead of arr * expr;;

let rec string_of_rhs (r : rhs) : string =
  match r with | Expr e -> string_of_expr e
               | PtrRead(e, _) -> "*(" ^ string_of_expr e ^ ")"
               | ArrayRead(a, e) -> string_of_value (CstA a) ^ "[" ^ string_of_expr e ^ "]"

type protect = Slh | Fence | Auto;;

type cmd =
  | Skip
  | Fail
  | VarAssign of identifier * rhs
  | PtrAssign of expr * expr * label
  | ArrAssign of arr * expr * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | While of expr * cmd
  | Protect of identifier * protect * rhs;;


let rec string_of_cmd (c : cmd) : string =
  match c with | Skip -> "Skip"
               | Fail -> "Fail"
               | VarAssign(ide, r) -> ide ^ " := " ^ string_of_rhs r
               | PtrAssign(e1, e2, _) -> "*" ^ string_of_expr e1 ^ " := " ^ string_of_expr e2
               | ArrAssign(a, e1, e2) -> string_of_value (CstA a) ^ "[" ^ string_of_expr e1 ^ "] := " ^ string_of_expr e2
               | Seq(c1, c2) -> string_of_cmd c1 ^ ";\n" ^ string_of_cmd c2
               | If(e, c1, c2) -> "If (" ^ string_of_expr e ^ ") then \n" ^ string_of_cmd c1 ^ "\nelse\n" ^ string_of_cmd c2 ^ "\nfi"
               | While(e, c) -> "While (" ^ string_of_expr e ^ ") do \n" ^ string_of_cmd c ^ "\nod"
               | Protect(ide, Slh, r) -> ide ^ " := Protect_slh(" ^ string_of_rhs r ^ ")"
               | Protect(ide, Fence, r) -> ide ^ " := Protect_f(" ^ string_of_rhs r ^ ")"
               | Protect(ide, Auto, r) -> ide ^ " := Protect(" ^ string_of_rhs r ^ ")"


(** 		DIRECTIVES 		**)
type prediction = bool

type directive =
  | Fetch
  | PFetch of prediction
  | Exec of int
  | Retire

type guard_id = int;;

(** 		OBSERVATIONS 		**)
type observation =
  | None
  | Read of int * int list
  | Write of int * int list
  | OFail of guard_id
  | Rollback of guard_id

(**		INSTRUCTION SET		**)
type instruction =
  | Nop
  | Assign of identifier * expr
  | Load of identifier * label * expr 			(* 	id := load(e) 		*)
  | Store of expr * label * expr
  | IProtect of identifier * expr 	(* 	id := protect(e) 	*)
  | Guard of expr * prediction * cmd list * guard_id
  | Fail of guard_id
  | Fence ;;

let string_of_instruction (instr : instruction) : string =
  match instr with | Nop -> "Nop"
                   | Assign(ide, expr) -> ide ^ " := " ^ (string_of_expr expr)
                   | Load(ide, _, e1) -> ide ^ " := load(" ^ string_of_expr e1 ^ ")"
                   | Store(e1, _, e2) -> "store(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
                   | IProtect(ide, e1) -> ide ^ " := protect(" ^ string_of_expr e1 ^ ")"
                   | Guard(e1, p, _, id) -> "guard(" ^ string_of_expr e1 ^ ", " ^ string_of_bool p ^ ", " ^ string_of_int id ^ ")"
                   | Fail(id) -> "fail(" ^ string_of_int id ^ ")"
                   | Fence -> "fence"

(**		CONFIGURATIONS 		**)
type configuration = {
	is : instruction list ;
	cs : cmd list ;
	mu : int array ;
	rho : value StringMap.t ;
};;

(* type decl_type =
  | TypI
  | TypA of int * int
  | TypP;; *)

class processor =
  object (self)
    val mutable state = (0 : int)
    val mutable printed = (false : bool);

    (* method get_next_directive (conf : configuration) : directive =
      match conf.cs with
        | If(_, _, _) :: _ -> PFetch true
        | _ :: _ -> Fetch
        | [] -> (if not printed then (printf "%s\n" (String.concat "\n" (List.map string_of_instruction conf.is)); printed <- true) else ();
            match conf.is with
              (* Fa Retire se può altrimenti Exec 0 *)
              | Nop :: _
              | Assign(_, Cst(_)) :: _
              | Store(Cst(CstI(_)), _, Cst(CstI(_))) :: _
              | Store(Cst(_), _, Cst(_)) :: _
              | Fail(_) :: _
              | Fence :: _ -> Retire
              | _ -> Exec 0) *)
      method get_next_directive (conf : configuration) : directive =
        match state with | 0 -> state <- 1; Fetch
                         | 1 -> state <- 2; PFetch true
                         | 2 -> state <- 3; Exec 0
                         | 3 -> state <- 0; Retire
                         | _ -> failwith "unexpected processor internal state"
  end

class fresh_factory =
  object (self)
    val mutable p = (0 : int)

    method get =
      p <- p + 1; p
  end

let fresh = new fresh_factory;;

let stepFetch (conf : configuration) : (configuration * observation) option =
  match conf.cs with
    | Skip :: cs1 -> some ({conf with cs = cs1; is = conf.is @ [Nop]}, None)
    | Fail :: cs1 -> some ({conf with cs = cs1; is = conf.is @ [Fail(fresh#get)]}, None)
    | VarAssign(ide, Expr(expr)) :: cs1 -> some ({conf with cs = cs1; is = conf.is @ [Assign(ide, expr)]}, None)
    | VarAssign(ide, PtrRead(addr_expr, l)) :: cs1 -> some ({conf with cs = cs1; is = conf.is @ [Load(ide, l, addr_expr)]}, None)
    | VarAssign(ide, ArrayRead(a, idx_expr)) :: cs1 -> let guard_expr = BinOp(idx_expr, Length(Cst(CstA(a))), Lt)
                                                       and then_cmd = VarAssign(ide, PtrRead(BinOp(Base(Cst(CstA(a))), idx_expr, Add), a.label)) in
                                                         some ({conf with cs = If(guard_expr, then_cmd, Fail) :: cs1}, None)
    | PtrAssign(addr_expr, expr, l) :: cs1 -> some ({conf with cs = cs1; is = conf.is @ [Store(addr_expr, l, expr)]}, None)
    | ArrAssign(a, idx_expr, expr) :: cs1 -> let guard_expr = BinOp(idx_expr, Length(Cst(CstA(a))), Lt)
                                             and then_cmd = PtrAssign(BinOp(Base(Cst(CstA(a))), idx_expr, Add), expr, a.label) in
                                              some ({conf with cs = If(guard_expr, then_cmd, Fail) :: cs1}, None)
    | Seq(c1, c2) :: cs1 -> some ({conf with cs = c1 :: c2 :: cs1}, None)
    | While(guard_expr, body_cmd) :: cs1 -> let c1 = Seq(body_cmd, While(guard_expr, body_cmd)) in
                                               some ({conf with cs = If(guard_expr, c1, Skip) :: cs1}, None)
    (* protect *)
    | Protect(ide, Slh, ArrayRead(a, idx_expr)) :: cs1
      -> let e1 = BinOp(idx_expr, Length(Cst(CstA(a))), Lt) in (* e < length(a) *)
         let e2 = BinOp(Base(Cst(CstA(a))), idx_expr, Add) in (* base(a) + e *)
         let e3 = InlineIf(e1, Cst(CstI(Int.max_int)), Cst(CstI(0)) ) in (* e < length(a) ? 1 : 0 *)
         let e4 = BinOp(e2, e3, BitAnd) in (* (base(a) + e) land (e < length(a) ? 1 : 0) *)
         let c1 = VarAssign(ide, PtrRead(e4, a.label)) in
          some ({conf with cs = If(e1, c1, Fail) :: cs1}, None)
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
    | Protect(ide, Fence, Expr(expr)) :: cs1 -> some ({conf with cs = cs1; is = conf.is @ [Fence; Assign(ide, expr)]}, None)
    | Protect(ide, prot_kind, Expr(expr)) :: cs1 -> some ({conf with cs = cs1; is = conf.is @ [IProtect(ide, expr)]}, None)
    | Protect(ide, prot_kind, PtrRead(addr_expr, l)) :: cs1 -> let c1 = VarAssign(ide, PtrRead(addr_expr, l))
                                                               and c2 = Protect(ide, prot_kind, Expr(Var(ide))) in
                                                                 some ({conf with cs = c1 :: c2 :: cs1}, None)
    | Protect(ide, prot_kind, ArrayRead(a, idx_expr)) :: cs1 -> let c1 = VarAssign(ide, ArrayRead(a, idx_expr))
                                                                and c2 = Protect(ide, prot_kind, Expr(Var(ide))) in
                                                                  some ({conf with cs = c1 :: c2 :: cs1}, None)
    | _ -> (* printf "unexpected command for fetch, ignoring"; *) none

let stepPFetch (pred : prediction) (conf : configuration) : (configuration * observation) option =
  match conf.cs with
    | If(guard_expr, then_cmd, else_cmd) :: cs1
      -> let taken = if pred then then_cmd else else_cmd
         and not_taken = if pred then else_cmd else then_cmd in
          some ({conf with cs = taken :: cs1; is = conf.is @ [Guard(guard_expr, pred, not_taken :: cs1, fresh#get)]}, None)
    | _ -> (* printf "unexpected command for pfetch, ignoring"; *) none

let stepRetire (conf : configuration) : (configuration * observation) option =
  match conf.is with 
    | Nop :: is1 -> some ({conf with is = is1}, None)
    | Assign(ide, Cst(v)) :: is1 -> some ({conf with rho = StringMap.add ide v conf.rho; is = is1}, None)
    | Store(Cst(CstI(addr)), _, Cst(CstI(v))) :: is1 ->
        conf.mu.(addr) <- v;
        some ({conf with is = is1}, None)
    | Store(Cst(_), _, Cst(_)) :: is1 -> failwith "store of non integer values"
    | Fail(p) :: is1 -> some ({conf with is = []; cs = []}, OFail(p))
    | Fence :: is1 -> some ({conf with is = is1}, None)
    | _ -> none


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

let split (l : 'a list) (n : int) =
  if List.length l <= n then none else
    let rec _split (l1, x, l2) (n : int) =
      match n with
      | 0 -> some (l1, x, l2)
      | n -> _split (l1 @ [x], List.hd l2, List.tl l2) (n - 1)
    in _split ([], List.hd l, List.tl l) n

let stepExec (n : int) (conf : configuration) : (configuration * observation) option =
  match split conf.is n with
    | None -> none
    | Some (is1, is, is2) ->
      (* The LFENCE instruction does not execute until all prior instructions have completed locally, and no later instruction begins execution until LFENCE completes.
      https://software.intel.com/content/www/us/en/develop/articles/using-intel-compilers-to-mitigate-speculative-execution-side-channel-issues.html?wapkw=lfence%20x86 *)
      if List.exists (fun i -> i = Fence) is1 = true
        then none
        else let rho = phi is1 conf.rho in
          match is with
            | Assign(ide, e) -> some ({conf with is = is1 @ [Assign(ide, Cst(eval e rho))] @ is2}, None)
            | Guard(e, p, cs', id) -> 
                if eval e rho = CstB(p)
                  then some({conf with is = is1 @ [Nop] @ is2}, None)
                  else some({conf with is = is1 @ [Nop]; cs = cs'}, Rollback(id))
            | Load(ide, l, e) -> 
                if List.exists (fun i -> match i with | Store(_,_,_) -> true | _ -> false) is1 = true 
                  then none 
                  else (match eval e rho with
                          | CstI(idx) -> some({conf with is = is1 @ [Assign(ide, Cst(CstI(conf.mu.(idx))))] @ is2}, Read(n, [])) (* controllare secondo parametro read *)
                          | _ -> failwith "memory address should be an integer"
                  )
            | Store(e1, l, e2) ->
                (match eval e1 rho, eval e2 rho with
                      | (CstI(n), CstI(v)) -> some({conf with is = is1 @ [Store(Cst(CstI(n)), l, Cst(CstI(v)))] @is2}, Write(n, []))
                      | _, _ -> failwith "store of non integer values")
            | IProtect(ide, Cst(v)) ->
                if List.exists (fun i -> match i with | Guard(_,_,_,_) -> true | _ -> false) is1 = true 
                  then none
                  else some({conf with is = is1 @ [Assign(ide, Cst(v))] @ is2}, None)
            | IProtect(ide, expr) -> some({conf with is = is1 @ [IProtect(ide, Cst(eval expr rho))] @ is2}, None)
            | Nop -> none
            | Fail(_) -> none
            | _ -> failwith "unexpected instruction for exec"

let step (conf : configuration) (d : directive) : (configuration * observation) option =
  match conf.is, conf.cs, d with
      | [], [], _       -> none
      | _, _, Fetch     -> stepFetch conf
      | _, _, PFetch(b) -> stepPFetch b conf
      | _, _, Exec(n)   -> stepExec n conf
      | _, _, Retire    -> stepRetire conf;;

let jiteval (proc : processor) (init_conf : configuration) : configuration * observation list * int =
  let rec _jiteval (conf : configuration) (obs_trace : observation list) (count : int) : configuration * observation list * int =
    if (conf.is = [] && conf.cs = []) then (conf, List.rev obs_trace, count)
    else let direct = proc#get_next_directive conf in
          match step conf direct with
              | None -> (* printf "No transition with the given directive\n"; *) _jiteval conf obs_trace count
              | Some(conf', obs) -> _jiteval conf' (obs :: obs_trace) (count + 1)
  in _jiteval init_conf [] 0;;