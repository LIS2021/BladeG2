%{
  module StringMap = Map.Make(String);;
  open Expr;;

  let rho = ref (StringMap.empty);;
  let used = ref 1;;
  let if_id_count = ref 1;;

  let new_array (ide : identifier) (len : int) : unit =
    let result = (CstA { base = !used; length = len; label = (); name = ide }) in
      rho := StringMap.add ide result (!rho);
      used := ((!used) + len);;

  let new_var (ide : identifier) (value : int) : unit =
    rho := StringMap.add ide (CstI value) (!rho);;

  let get_array (ide : identifier) : arr =
    match StringMap.find ide (!rho) with | CstA(a) -> a
                                            | _ -> failwith "???"

  let get_if_id () : int =
    let result = !if_id_count in
        if_id_count := !if_id_count + 1;
        result
%}

/* File parser.mly */
%token EOF
%token ADD LTE LT BITAND
%token LPAREN RPAREN
%token LENGTH BASE QUESTIONMARK COLON
%token STAR LSQUARE RSQUARE
%token PERCENT
%token SKIP FAIL SEMICOLON ASSIGN
%token PROTECT PROTECT_SLH PROTECT_FENCE PROTECT_HW 
%token IF THEN ELSE FI
%token WHILE DO OD
%token <string> IDENTIFIER
%token <int> INT

%nonassoc QUESTIONMARK COLON
%left ADD LTE LT BITAND
%nonassoc LENGTH BASE
%nonassoc STAR
%right SEMICOLON

%start main             /* the entry point */
%type <Commands.cmd * Expr.environment * int> main
%%
main:
    PERCENT decls cmd EOF   { $3, !rho, !used }
    | cmd EOF               { $1, !rho, !used }
;
decl:
    IDENTIFIER LSQUARE INT RSQUARE { new_array $1 $3 }
    | IDENTIFIER ASSIGN INT        { new_var $1 $3 }
;
decls:
    PERCENT                        { () }
    | decl decls                   { () }
;
rhs:
    expr                                { Expr($1)}
    | STAR expr                         { PtrRead($2, ()) }
    | IDENTIFIER LSQUARE expr RSQUARE   { ArrayRead(get_array $1, $3) }
;
expr:
    INT                     { Cst(CstI($1)) }
    | LPAREN expr RPAREN      { $2 }
    | expr ADD expr          { BinOp($1, $3, Add) }
    | expr LTE expr         { BinOp($1, $3, Lte) }
    | expr LT expr         { BinOp($1, $3, Lt) }
    | expr BITAND expr           { BinOp($1, $3, BitAnd) }
    | LENGTH LPAREN IDENTIFIER RPAREN {Length($3)}
    | BASE LPAREN expr RPAREN {Base($3)}
    | expr QUESTIONMARK expr COLON expr { InlineIf($1, $3, $5) }
    | IDENTIFIER {Var($1)}
;
cmd:
    SKIP         { Skip }
    | FAIL       { Fail }
    | IDENTIFIER ASSIGN rhs { VarAssign($1, $3) }
    | STAR expr ASSIGN expr { PtrAssign($2, $4, ()) }
    | IDENTIFIER LSQUARE expr RSQUARE ASSIGN expr { ArrAssign(get_array $1, $3, $6) }
    | cmd SEMICOLON cmd     { Seq($1, $3) }
    | IF expr THEN cmd ELSE cmd FI { If($2, $4, $6, get_if_id ()) }
    | WHILE expr DO cmd OD  { While($2, $4, get_if_id ()) }
    | IDENTIFIER ASSIGN PROTECT LPAREN rhs RPAREN { Protect($1, Auto, $5) }
    | IDENTIFIER ASSIGN PROTECT_SLH LPAREN rhs RPAREN { Protect($1, Slh, $5) }
    | IDENTIFIER ASSIGN PROTECT_FENCE LPAREN rhs RPAREN { Protect($1, Fence, $5) }
    | IDENTIFIER ASSIGN PROTECT_HW LPAREN rhs RPAREN { Protect($1, Hw, $5) }
;