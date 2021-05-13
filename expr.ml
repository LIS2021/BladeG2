(* Module with expr and related types *)
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
  | Length of identifier
  | Base of expr;;

let rec string_of_expr (e : expr) : string =
  match e with | Cst v -> string_of_value v
               | Var x -> x
               | BinOp(e1, e2, op) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2 ^ ")"
               | InlineIf(e1, e2, e3) -> string_of_expr e1 ^ " ? " ^ string_of_expr e2 ^ " : " ^ string_of_expr e3
               | Length(ide) -> "length(" ^ ide ^ ")"
               | Base(e1) -> "base(" ^ string_of_expr e1 ^ ")"

type rhs =
  | Expr of expr
  | PtrRead of expr * label
  | ArrayRead of arr * expr;;

let rec string_of_rhs (r : rhs) : string =
  match r with | Expr e -> string_of_expr e
               | PtrRead(e, _) -> "*(" ^ string_of_expr e ^ ")"
               | ArrayRead(a, e) -> string_of_value (CstA a) ^ "[" ^ string_of_expr e ^ "]"

type protect = Slh | Fence | Hw | Auto;;