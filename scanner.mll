(* File lexer.mll *)
{
    open Parser        (* The type token is defined in parser.mli *)
    exception Eof
}
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | '+'            { ADD }
    | ":="           { ASSIGN }
    | "<="           { LTE }
    | '<'            { LT }
    | '&'            { BITAND }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | '?'            { QUESTIONMARK }
    | ':'            { COLON }
    | '*'            { STAR }
    | '['            { LSQUARE }
    | ']'            { RSQUARE }
    | '%'            { PERCENT }
    | ';'            { SEMICOLON }
    | "length"       { LENGTH }
    | "base"         { BASE }
    | "skip"         { SKIP }
    | "fail"         { FAIL }
    | "if"           { IF }
    | "fi"           { FI }
    | "then"         { THEN }
    | "else"         { ELSE }
    | "while"        { WHILE }
    | "do"           { DO }
    | "od"           { OD }
    | "protect"      { PROTECT }
    | ['a'-'z'] [ 'a'-'z' '0'-'9' ]* as lxm { IDENTIFIER(lxm) }
    | eof            { EOF }