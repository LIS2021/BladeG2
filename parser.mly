%{
  module StringMap = Map.Make(String);;
  open Eval;;

  let arrays = ref (StringMap.empty);;
  let used = ref 1;;

  let new_array (ide : identifier) (len : int) : unit =
    let result = (CstA { base = !used; length = len; label = (); name = ide }) in
      arrays := StringMap.add ide result (!arrays);
      used := ((!used) + len);;

  let get_array (ide : identifier) : arr =
    match StringMap.find ide (!arrays) with | CstA(a) -> a
                                            | _ -> failwith "???"

%}

/* File parser.mly */
%token EOF
%token ADD LTE LT BITAND
%token LPAREN RPAREN
%token LENGTH BASE QUESTIONMARK COLON
%token STAR LSQUARE RSQUARE
%token PERCENT
%token SKIP FAIL PROTECT SEMICOLON ASSIGN
%token IF THEN ELSE FI
%token WHILE DO OD
%token <string> IDENTIFIER
%token <int> INT

%nonassoc QUESTIONMARK COLON
%left ADD LTE LT BITAND
%nonassoc LENGTH BASE
%nonassoc STAR
%left SEMICOLON

%start main             /* the entry point */
%type <Eval.cmd * Eval.environment * int> main
%%
main:
    PERCENT decls cmd EOF   { $3, !arrays, !used }
    | cmd EOF               { $1, !arrays, !used }
;
decl:
    IDENTIFIER LSQUARE INT RSQUARE { new_array $1 $3 }
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
    | LENGTH LPAREN expr RPAREN {Length($3)}
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
    | IF expr THEN cmd ELSE cmd FI { If($2, $4, $6) }
    | WHILE expr DO cmd OD  { While($2, $4) }
    | IDENTIFIER ASSIGN PROTECT LPAREN rhs RPAREN { Protect($1, Auto, $5) }
;