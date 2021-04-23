module StringMap = Map.Make(String)
open Option;;
open Format;;

type identifier = string;;

type op =
  | Add
  | Lte
  | Lt
  | BitAnd;;

type label = unit;;

type arr = {base : int; length : int; label : label};;

type value =
  | CstI of int
  | CstB of bool
  | CstA of arr;;

type expr =
  | Cst of value
  | Var of identifier
  | BinOp of expr * expr * op
  | InlineIf of expr * expr * expr
  | Length of expr
  | Base of expr;;

type rhs =
  | Expr of expr
  | PtrRead of expr * label
  | ArrayRead of arr * expr;;

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
  | Fail of guard_id
  | Rollback of guard_id

(**		INSTRUCTION SET		**)
type instruction =
  | Nop
  | AssignE of identifier * expr
  | AssignV of identifier * value
  | Load of identifier * label * expr 			(* 	id := load(e) 		*)
  | StoreE of expr * label * expr
  | StoreV of int * label * int
  | IProtectE of identifier * protect * expr 	(* 	id := protect(e) 	*)
  | IProtectV of identifier * value 	(* 	id := protect(v) 	*)
  | Guard of expr * prediction * cmd list * guard_id
  | Fail of guard_id ;;

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

    method get_next_directive =
      Fetch
  end

class fresh_factory =
  object (self)
    val mutable p = (0 : int)

    method get =
      p <- p + 1; p
  end

let fresh = new fresh_factory;;

(* Get a fresh variable name. Use x as the base, to highlight that it
   holds a temporary value of variable x *)
let get_fresh_name (rho : value StringMap.t) (x : identifier) : identifier =
  let rec _get_fresh (y : identifier) =
    let res = y ^ ("'") in
      if StringMap.mem res rho then res else _get_fresh res
  in _get_fresh x

let stepFetch (conf : configuration) : (configuration * observation) option =
  match conf.cs with
    | Skip :: cs1 -> some ({conf with cs = cs1; is = conf.is @ [Nop]}, None)
    | Fail :: cs1 -> some ({conf with cs = cs1; is = conf.is @ [Fail(fresh#get)]}, None)
    | VarAssign(ide, Expr(expr)) :: cs1
      -> some ({conf with cs = cs1; is = conf.is @ [AssignE(ide, expr)]}, None)
    | VarAssign(ide, PtrRead(addr_expr, l)) :: cs1
      -> some ({conf with cs = cs1; is = conf.is @ [Load(ide, l, addr_expr)]}, None)
    | VarAssign(ide, ArrayRead(a, idx_expr)) :: cs1
      -> let guard_expr = BinOp(idx_expr, Length(Cst(CstA(a))), Lt)
         and then_cmd = VarAssign(ide, PtrRead(BinOp(Base(Cst(CstA(a))), idx_expr, Add), a.label)) in
          some ({conf with cs = If(guard_expr, then_cmd, Fail) :: cs1}, None)
    | PtrAssign(addr_expr, expr, l) :: cs1
      -> some ({conf with cs = cs1; is = conf.is @ [StoreE(addr_expr, l, expr)]}, None)
    | ArrAssign(a, idx_expr, expr) :: cs1
      -> let guard_expr = BinOp(idx_expr, Length(Cst(CstA(a))), Lt)
         and then_cmd = PtrAssign(BinOp(Base(Cst(CstA(a))), idx_expr, Add), expr, a.label) in
          some ({conf with cs = If(guard_expr, then_cmd, Fail) :: cs1}, None)
    | Seq(c1, c2) :: cs1 -> some ({conf with cs = c1 :: c2 :: cs1}, None)
    | While(guard_expr, body_cmd) :: cs1
      -> let c2 = Seq(body_cmd, While(guard_expr, body_cmd)) in
          some ({conf with cs = If(guard_expr, c2, Skip) :: cs1}, None)
    (* protect *)
    | Protect(ide, Slh, ArrayRead(a, idx_expr)) :: cs1
      -> let e1 = BinOp(idx_expr, Length(Cst(CstA(a))), Lt)
         and e2 = BinOp(Base(Cst(CstA(a))), idx_expr, Add)
         and r = get_fresh_name conf.rho "r" in
         let c1 = VarAssign(r, Expr(e1))
         and c2 = VarAssign(r, Expr(InlineIf(Var(r), Cst(CstI(1)), Cst(CstI(0)))))
         and c3 = VarAssign(ide, PtrRead(BinOp(e2, Var(r), BitAnd), a.label)) in
         let c' = Seq(c1, If(Var(r), Seq(c2, c3), Fail)) in
          some ({conf with cs = c' :: cs1}, None)
    | Protect(ide, prot_kind, Expr(expr)) :: cs1
      -> some ({conf with cs = cs1; is = conf.is @ [IProtectE(ide, prot_kind, expr)]}, None)
    | Protect(ide, prot_kind, PtrRead(addr_expr, l)) :: cs1
      -> let x' = get_fresh_name conf.rho ide in
         let c1 = VarAssign(x', PtrRead(addr_expr, l))
         and c2 = Protect(ide, prot_kind, Expr(Var(x'))) in
          some ({conf with cs = c1 :: c2 :: cs1}, None)
    | Protect(ide, prot_kind, ArrayRead(a, idx_expr)) :: cs1
      -> let x' = get_fresh_name conf.rho ide in
         let c1 = VarAssign(x', ArrayRead(a, idx_expr))
         and c2 = Protect(ide, prot_kind, Expr(Var(x'))) in
          some ({conf with cs = c1 :: c2 :: cs1}, None)
    | _ -> printf "unexpected command for fetch, ignoring"; none

let stepPFetch (pred : prediction) (conf : configuration) : (configuration * observation) option =
  match conf.cs with
    | If(guard_expr, then_cmd, else_cmd) :: cs1
      -> let taken = if pred then then_cmd else else_cmd
         and not_taken = if pred then else_cmd else then_cmd in
          some ({conf with cs = taken :: cs1; is = conf.is @ [Guard(guard_expr, pred, not_taken :: cs1, fresh#get)]}, None)
    | _ -> printf "unexpected command for pfetch, ignoring"; none

let step (conf : configuration) (d : directive) : (configuration * observation) option =
  match conf.is, conf.cs, d with
      | [], [], _       -> none
      | _, _, Fetch     -> printf "fetch"; stepFetch conf
      | _, _, PFetch(b) -> printf "pfetch"; stepPFetch b conf
      | _, _, Exec(n)   -> printf "exec"; stepExec n conf
      | _, _, Retire    -> printf "retire"; stepRetire conf;;

(* let rec eval attacker conf trace counter = *)
  (* questo si occupa di aggiornare la lista degli osservabili e il counter *)
