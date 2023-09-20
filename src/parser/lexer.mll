{

  open Parser
  exception Quit

}

let digit = ['0'-'9']
let identchar = ['a'-'z' 'A'-'Z' '\'' '_' '0'-'9']
let ident = ['a'-'z' '_'] identchar*
let uident = ['A'-'Z' '_'] identchar*
let tident = '\'' identchar*
let opencomment = "(*"
let closecomment = "*)"
let ws = [' ' '\t']

rule comment = parse
       | closecomment { token lexbuf }
       | '\n' { Lexing.new_line lexbuf; comment lexbuf }
       | _ { comment lexbuf}
and token = parse
       | ws { token lexbuf }
       | '\n' { Lexing.new_line lexbuf; token lexbuf }
       | opencomment { comment lexbuf }
       | ['-']?digit+ as n { NUM (int_of_string n) }
       | '\"' ((_ # '\"')* as s) '\"' { STRING s }
       | '\'' ((_ # '\'') as c) '\'' { CHAR c }
       | "true" { TRUE }
       | "false" { FALSE }
       | "()" { UNIT }
       | "[]" { NIL }

       | "int" { TINT }
       | "string" { TSTRING }
       | "char" { TCHAR }
       | "bool" { TBOOL }
       | "unit" { TUNIT }
       | ('\'' ident) as s { TIDENT s }

       | "type" { TYPE }
       | "external" { EXTERNAL }
       | "of" { OF }

       | "ref" { REF }
       | "!" { BANG }
       | ":=" { COLONEQ }

       | "futref" { REFFUT }

       | "future" { FUTURE }
       | "force" { FORCE }

       | "," { COMMA }
       | "+" { PLUS }
       | "-" { MINUS }
       | "*" { TIMES }
       | "/" { DIV }
       | "^" { CARAT }
       | "::" { CONS }
       | ":" { COLON }
       | "&&" { AND }
       | "||" { OR }
       | "<=" { LE }
       | "<" { LT }
       | ">=" { GE }
       | ">" { GT }
       | "<>" { NE }

       | "=" { EQUAL }

       | "fun" { FUN }
       | "->" { ARROW }

       | "if" { IF }
       | "then" { THEN }
       | "else" { ELSE }

       | "match" { MATCH }
       | "with" { WITH }
       | "|" { PIPE }

       | "let" { LET }
       | "rec" { REC }
       | "in" { IN }

       | "try" { TRY }

       | "(" { LPAREN }
       | ")" { RPAREN }

       | ";" { SEMI }

       | ident as s { IDENT s }
       | uident as s { UIDENT s }
       | tident as s { TIDENT s }
       | "." { DOT }

       | eof { EOF }
       | ";;" { DOUBLESEMI }