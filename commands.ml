(* Commands and instructions types, plus printing functions *)
open Expr;;

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
match c with | Skip -> "skip"
             | Fail -> "fail"
             | VarAssign(ide, r) -> ide ^ " := " ^ string_of_rhs r
             | PtrAssign(e1, e2, _) -> "*" ^ string_of_expr e1 ^ " := " ^ string_of_expr e2
             | ArrAssign(a, e1, e2) -> string_of_value (CstA a) ^ "[" ^ string_of_expr e1 ^ "] := " ^ string_of_expr e2
             | Seq(c1, c2) -> string_of_cmd c1 ^ ";\n" ^ string_of_cmd c2
             | If(e, c1, c2) -> "if " ^ string_of_expr e ^ " then \n" ^ string_of_cmd c1 ^ "\nelse\n" ^ string_of_cmd c2 ^ "\nfi"
             | While(e, c) -> "while " ^ string_of_expr e ^ " do \n" ^ string_of_cmd c ^ "\nod"
             | Protect(ide, Slh, r) -> ide ^ " := protect_slh(" ^ string_of_rhs r ^ ")"
             | Protect(ide, Fence, r) -> ide ^ " := protect_fence(" ^ string_of_rhs r ^ ")"
             | Protect(ide, Hw, r) -> ide ^ " := protect_hw(" ^ string_of_rhs r ^ ")"
             | Protect(ide, Auto, r) -> ide ^ " := protect(" ^ string_of_rhs r ^ ")"


(** 		DIRECTIVES 		**)
type prediction = bool

type directive =
  | Fetch
  | PFetch of prediction
  | Exec of int
  | Retire

let string_of_directive (d : directive) : string =
  match d with | Fetch -> "Fetch"
               | PFetch(b) -> "PFetch(" ^ string_of_bool b ^ ")"
               | Exec(n) -> "Exec(" ^ string_of_int n ^ ")"
               | Retire -> "Retire"

type guard_id = int;;

(** 		OBSERVATIONS 		**)
type observation =
  | None
  | Read of int * int list
  | Write of int * int list
  | OFail of guard_id
  | Rollback of guard_id

let string_of_obs (o : observation) : string =
match o with | None -> "None"
             | Read(i, _) -> "Read(" ^ string_of_int i ^ ")"
             | Write(i, _) -> "Write(" ^ string_of_int i ^ ")"
             | OFail(gid) -> "Fail(" ^ string_of_int gid ^ ")"
             | Rollback(gid) -> "Rollback(" ^ string_of_int gid ^ ")"

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
