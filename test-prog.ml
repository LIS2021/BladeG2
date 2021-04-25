module StringMap = Map.Make(String)

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

let prog1 =
  Seq(VarAssign("x", Expr(Cst(CstI(2)))),
      VarAssign("y", Expr(BinOp(Var("x"), Cst(CstI(3)), Add))));;

let a = ({ base = 10; length = 20; label = ()} : arr)
and b = ({ base = 50; length = 10; label = ()} : arr)

let prog2 =
  Seq(VarAssign("i1", Expr(Cst(CstI(10)))),
  Seq(VarAssign("i2", Expr(Cst(CstI(5)))),
  Seq(ArrAssign(a, Var "i1", Cst(CstI(100))),
  Seq(VarAssign("x", ArrayRead(a, Var "i1")),
  Seq(VarAssign("y", ArrayRead(a, Var "i2")),
  Seq(VarAssign("z", Expr(BinOp(Var "x", Var "y", Add))),
  Seq(VarAssign("w", ArrayRead(b, Var "z")),
      Skip
  )))))));;

let proc = new processor;;

let print_var ide value =
  print_string(ide ^ " " ^ (string_of_value value )^ "\n");;

let conf, obs_trace, count = jiteval proc prog2 in
  StringMap.iter print_var conf.rho; conf, obs_trace, count;;