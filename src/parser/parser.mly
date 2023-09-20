%{
    open Ast

    exception SyntaxError

    let syn_err s (spos, epos) =
      let string_of_pos p =
	Lexing.(Printf.sprintf "%s:%d:%d"
	          p.pos_fname
	          p.pos_lnum
	          (p.pos_cnum - p.pos_bol))
      in
      Printf.printf "%s--%s: Syntax Error: %s\n"
		    (string_of_pos spos)
		    (string_of_pos epos)
		    s;
      raise SyntaxError

    let mk_exp e l =
      { edesc = e
      ; eloc = l
      ; etyp = ()
      ; egr = ()
      }

    let mk_typ t l =
      { tdesc = t
      ; tloc = l
      }

    type p_typ_ = (void, unit, unit, unit) typ_desc
    type p_expr_ = (void, void, unit, unit, unit, unit) expr_desc
    type p_decl_ = (void, void, unit, unit, unit, unit) decl_desc

  let base_ty id =
     (TConstr (id, [], ()))

%}

%token <int> NUM
%token <string> IDENT
%token <string> UIDENT
%token <string> TIDENT
%token <string> STRING
%token <char> CHAR
%token TRUE FALSE UNIT NIL
%token TINT TBOOL TUNIT TSTRING TCHAR
%token PLUS MINUS TIMES DIV CARAT
%token AND OR
%token COLON COMMA CONS
%token LT LE GT GE NE
%token EQUAL
%token IF THEN ELSE
%token MATCH WITH PIPE
%token LET REC IN
%token FUN ARROW
%token LPAREN RPAREN
%token SEMI DOUBLESEMI
%token EOF
%token DOT
%token REF BANG COLONEQ REFFUT
%token FUTURE FORCE
%token TRY

%token TYPE OF EXTERNAL

%right ARROW CONS
%left LT LE GT GE NE EQUAL
%left PLUS MINUS
%left TIMES DIV
%left AND OR


%start prog

%type <Ast.p_expr list> exprcommalist
%type <string list> identcommalist
%type <string list> tidentcommalist
%type <Ast.longid> longid
%type <Ast.p_typ> atom_typ
%type <p_typ_> atom_typ_
%type <Ast.p_typ> app_typ
%type <p_typ_> app_typ_
%type <p_typ_> typ_
%type <Ast.p_typ list> ttyp
%type <Ast.p_typ> typ
%type <Ast.p_const> const
%type <p_expr_> expr_
%type <p_expr_> simple_expr_
%type <p_expr_> app_expr_
%type <Ast.p_expr> expr
%type <Ast.p_expr> simple_expr
%type <string * Ast.p_typ option> ppat
%type <Ast.p_typ option> optannot

%type <Ast.longid * string list * Ast.p_expr> match_case
%type <(Ast.longid * string list * Ast.p_expr) list> match_cases
%type <(Ast.longid * string list * Ast.p_expr) list> more_match_cases
%type <string * Ast.p_typ> type_case
%type <(string * Ast.p_typ) list> type_cases
%type <(string * Ast.p_typ) list> more_type_cases
%type <Ast.p_typ list> typcommalist
%type <Ast.longid> ulongid

%type <p_decl_> decl_
%type <Ast.p_decl> decl

%type <Ast.p_prog> prog

%%

exprcommalist:
  expr COMMA expr                          { [$1; $3] }
| expr COMMA exprcommalist                 { $1::$3 }
;

identcommalist:
  IDENT COMMA IDENT                        { [$1; $3] }
| IDENT COMMA identcommalist               { $1::$3 }
;

tidentcommalist:
  TIDENT COMMA TIDENT                        { [$1; $3] }
| TIDENT COMMA tidentcommalist               { $1::$3 }
;

longid:
  IDENT                                    { Id $1 }
| UIDENT DOT longid                        { Modid ($1, $3) }
;

ulongid:
  UIDENT                                   { Id $1 }
| UIDENT DOT ulongid                       { Modid ($1, $3) }
;

match_case:
  ulongid IDENT ARROW expr                  { ($1, [$2], $4) }
| ulongid ARROW expr                        { ($1, [], $3) }
| ulongid LPAREN identcommalist RPAREN ARROW expr
    { ($1, $3, $6) }
| NIL ARROW expr                            { (Id "Nil", [], $3) }
| IDENT CONS IDENT ARROW expr               { (Id "Cons", [$1; $3], $5) }
| ulongid error { syn_err "Expected pattern after constructor" $loc }
;

match_cases:
| PIPE match_case more_match_cases         { $2::$3 }
| match_case more_match_cases              { $1::$2 }
;

more_match_cases:
|                                          { [] }
| PIPE match_case more_match_cases         { $2::$3}
;

(*
more_match_cases:
| PIPE match_case          { [$2] }
;
*)

expr_:
| app_expr_                                { $1 }
| MATCH expr WITH match_cases              { EMatch ($2, $4) }
| TRY expr WITH match_cases                { ETry ($2, $4) }
| IF expr THEN expr ELSE expr              { EIf ($2, $4, $6) }
| LET IDENT optannot EQUAL expr IN expr    { ELet ($2, $3, $5, $7) }
| LET REC IDENT ppat optannot EQUAL expr IN expr {
                            let (x, x_ty) = $4 in
                            let fn = mk_exp (EFunc (Recursive, $3, x, x_ty, $5, (), (), $7)) $loc in
                            ELet ($3, None, fn, $9) }
| LET IDENT ppat optannot EQUAL expr IN expr {
                            let (x, x_ty) = $3 in
                            let fn = mk_exp (EFunc (Terminal, $2, x, x_ty, $4, (), (), $6)) $loc in
                            ELet ($2, None, fn, $8) }
| LET LPAREN identcommalist RPAREN EQUAL expr IN expr
    { ELetTuple ($3, $6, $8) }
| FUN ppat ARROW expr
    { EFunc (Terminal, "", fst $2, snd $2, None, (), (), $4) }
| simple_expr COLON typ                           { EAnnot ($1, $3) }
| simple_expr CONS error { syn_err "Expected expression" $loc }
| MATCH expr WITH error { syn_err "Expected case after WITH" $loc }
| MATCH expr error { syn_err "Expected WITH" $loc }
| MATCH error { syn_err "Expected expression" $loc }
| TRY expr WITH error { syn_err "Expected case after WITH" $loc }
| TRY expr error { syn_err "Expected WITH" $loc }
| TRY error { syn_err "Expected expression" $loc }
| IF expr THEN expr ELSE error { syn_err "Expected expression" $loc }
| IF expr THEN expr error { syn_err "Invalid token" $loc }
| IF expr THEN error { syn_err "Expected expression (if without else not supported)" $loc }
| IF expr error { syn_err "Expected THEN" $loc }
| IF error { syn_err "Expected expression" $loc }
| LET IDENT optannot EQUAL expr IN error
    { syn_err "Expected expression" $loc }
| LET IDENT optannot EQUAL expr error
    { syn_err "Expected IN" $loc }
| LET IDENT optannot EQUAL error
    { syn_err "Expected expression" $loc }
| LET IDENT error
    { syn_err "Expected = or pattern (Maybe you forgot a type annotation on a function?)" $loc }
| LET error
    { syn_err "Expected pattern or REC" $loc }
| LET REC IDENT ppat optannot EQUAL expr IN error
    { syn_err "Expected expression" $loc }
| LET REC IDENT ppat optannot EQUAL expr error
    { syn_err "Expected IN" $loc }
| LET REC IDENT ppat optannot EQUAL error
    { syn_err "Expected expression" $loc }
| LET REC IDENT ppat error
    { syn_err "Expected =" $loc }
| LET REC IDENT error
    { syn_err "Expected pattern (x : t) -- note type annotation is required" $loc }
| LET REC error
    { syn_err "Expected identifier" $loc }
| LET IDENT ppat optannot EQUAL expr IN error
    { syn_err "Expected expression" $loc }
| LET IDENT ppat optannot EQUAL expr error
    { syn_err "Expected IN" $loc }
| LET IDENT ppat optannot EQUAL error
    { syn_err "Expected expression" $loc }
| LET IDENT ppat error
    { syn_err "Expected =" $loc }
| LET LPAREN identcommalist RPAREN EQUAL expr IN error
    { syn_err "Expected expression here" $loc }
| LET LPAREN identcommalist RPAREN EQUAL expr error
    { syn_err "Expected IN" $loc }
| LET LPAREN identcommalist RPAREN EQUAL error
    { syn_err "Expected expression" $loc }
| LET LPAREN identcommalist RPAREN error
    { syn_err "Expected =" $loc }
| LET LPAREN identcommalist error
    { syn_err "Expected )" $loc }
| FUN ppat ARROW error {syn_err "Expected expression" $loc}
| FUN ppat error { syn_err "Expected ->" $loc }
| FUN error { syn_err "Expected pattern" $loc }
| simple_expr COLON error { syn_err "Expected type" $loc }

app_expr_:
| simple_expr_                             { $1 }
| ulongid simple_expr                      { EApp (mk_exp (EVar $1) ($loc($1)), (), (), $2) }
| REF simple_expr                          { ERef $2 }
| BANG simple_expr                         { EDeref $2 }
                                            (* note: what if instead of `()` we used unit as the vertex_struct type at this stage *)
| app_expr_ simple_expr                    { EApp (mk_exp $1 $loc, (), (), $2)}
| FUTURE simple_expr                       { EFuture ((), $2) }
| FORCE simple_expr                         { EForce $2 }
;

simple_expr_:
  simple_expr PLUS simple_expr                       { EInfixop (Plus, $1, $3) }
| simple_expr MINUS simple_expr                      { EInfixop (Minus, $1, $3) }
| simple_expr TIMES simple_expr                      { EInfixop (Times, $1, $3) }
| simple_expr DIV simple_expr                        { EInfixop (Div, $1, $3) }
| simple_expr AND simple_expr                        { EInfixop (And, $1, $3) }
| simple_expr OR simple_expr                         { EInfixop (Or, $1, $3) }
| simple_expr EQUAL simple_expr                      { EInfixop (Eq, $1, $3) }
| simple_expr LT simple_expr                         { EInfixop (Lt, $1, $3) }
| simple_expr LE simple_expr                         { EInfixop (Le, $1, $3) }
| simple_expr GT simple_expr                         { EInfixop (Gt, $1, $3) }
| simple_expr GE simple_expr                         { EInfixop (Ge, $1, $3) }
| simple_expr NE simple_expr                         { EInfixop (Ne, $1, $3) }
| simple_expr CARAT simple_expr                      { EInfixop (Concat, $1, $3) }
| simple_expr CONS simple_expr                       {
    EApp (mk_exp (EVar (Id "Cons")) $loc, (), (), mk_exp (ETuple [$1; $3]) $loc)}
| simple_expr COLONEQ simple_expr          { EUpdate ($1, $3) }
| const                                    { EConst $1 }
| ulongid                                  {
  EApp (mk_exp (EVar $1) $loc, (), (), mk_exp (ETuple []) $loc) }
| NIL                                      {
  EApp (mk_exp (EVar (Id "Nil")) $loc, (), (), mk_exp (ETuple []) $loc) }
| IDENT                                    { EVar (Id $1) }
| LPAREN expr RPAREN                       { $2.edesc }
| simple_expr PLUS error { syn_err "Expected expression" $loc }
| simple_expr MINUS error { syn_err "Expected expression" $loc }
| simple_expr TIMES error { syn_err "Expected expression" $loc }
| simple_expr DIV error { syn_err "Expected expression" $loc }
| simple_expr AND error { syn_err "Expected expression" $loc }
| simple_expr OR error { syn_err "Expected expression" $loc }
| simple_expr LT error { syn_err "Expected expression" $loc }
| simple_expr LE error { syn_err "Expected expression" $loc }
| simple_expr GT error { syn_err "Expected expression" $loc }
| simple_expr GE error { syn_err "Expected expression" $loc }
| simple_expr NE error { syn_err "Expected expression" $loc }
| LPAREN expr error { syn_err "Expected )" $loc }
| LPAREN exprcommalist RPAREN                     { ETuple $2 }
| LPAREN exprcommalist error {syn_err "Expected )" $loc }
| LPAREN error { syn_err "Expected )" $loc }
;

expr:
  expr_                                    { mk_exp $1 $loc }
| expr SEMI expr                           { mk_exp (ELet ("_", None, $1, $3)) $loc }
;

simple_expr:
  simple_expr_                             { mk_exp $1 $loc }
;


ppat:
  LPAREN IDENT COLON typ RPAREN            { ($2, Some $4) }
| IDENT                                    { ($1, None) }
;


optannot:
  COLON typ                                { Some $2 }
|                                          { None }

const:
  NUM                                      { Num $1 }
| TRUE                                     { Bool true }
| FALSE                                    { Bool false }
| UNIT                                     { Unit }
| CHAR                                     { Char $1 }
| STRING                                   { String $1 }
| REFFUT                                   { Futref () }
;

typcommalist:
  typ COMMA typ                            { [$1; $3] }
| typ COMMA typcommalist                   { $1::$3 }
;

atom_typ_:
  TINT                                     { base_ty (Id "int") }
| TBOOL                                    { base_ty (Id "bool") }
| TUNIT                                    { base_ty (Id "unit") }
| TSTRING                                  { base_ty (Id "string") }
| TCHAR                                    { base_ty (Id "char") }
| TIDENT                                   { (TVar $1) }
| longid                                   { base_ty $1 }
| LPAREN typ RPAREN                        { $2.tdesc }
| LPAREN typ error { syn_err "Expected )" $loc }
;

app_typ_:
| atom_typ_                               { $1 }
| app_typ longid                         { TConstr
					       ($2, [$1], ())}
| app_typ FUTURE                         { TFuture
					     ((), $1)}
| app_typ REF                            { TRef $1}
| LPAREN typcommalist RPAREN longid        { TConstr
					       ($4, $2, ()) }
;

ttyp:
  app_typ                                  { [$1] }
| app_typ TIMES ttyp                       { $1::$3 }
| app_typ TIMES error { syn_err "Expected type" $loc }
;

typ_:
  ttyp                                     { if (List.length $1) = 1 then (List.hd $1).tdesc else TProd $1 }
| typ ARROW typ                            { TArrow ($1, $3, (), (), ()) }
| typ ARROW error { syn_err "Expected type" $loc }
;

typ:
  typ_                                 { mk_typ $1 $loc }
;

atom_typ:
  atom_typ_                                { mk_typ $1 $loc }
;

app_typ:
  app_typ_                                { mk_typ $1 $loc }
;

decl:
  decl_                                { { ddesc = $1;
                                           dloc = $loc;
                                           dinfo = () }
                                       }
;

type_case:
  UIDENT                                   { ($1, mk_typ (TProd []) $loc) }
| UIDENT OF typ                            { ($1, $3) }
| ulongid error { syn_err "Expected pattern after constructor" $loc }
;

type_cases:
| PIPE type_case more_type_cases           { $2::$3 }
| type_case more_type_cases                { $1::$2 }
;

more_type_cases:
|                                          { [] }
| PIPE type_case more_type_cases           { $2::$3}
;

decl_:
| LET IDENT optannot EQUAL expr            { DVal ($2, $3, $5) }
| LET REC IDENT ppat optannot EQUAL expr
    { let (x, x_ty) = $4 in
      DVal ($3, $5, mk_exp (EFunc (Recursive, $3, x, x_ty, $5, (), (), $7)) $loc) }
| LET IDENT ppat optannot EQUAL expr
    { let (x, x_ty) = $3 in
      DVal ($2, $4, mk_exp (EFunc (Terminal, $2, x, x_ty, $4, (), (), $6)) $loc) }
| expr                                     { DExp $1 }
| TYPE IDENT                               { DExtType $2 }
| TYPE LPAREN tidentcommalist RPAREN IDENT EQUAL type_cases
                                           { DTypeDef ($3, $5, $7) }
| TYPE TIDENT IDENT EQUAL type_cases
                                           { DTypeDef ([$2], $3, $5) }
| TYPE IDENT EQUAL type_cases
                                           { DTypeDef ([], $2, $4) }
| EXTERNAL longid COLON typ                { DExternal ($2, $4) }
| error { syn_err "Expected declaration" $loc }
;

prog:
| EOF                                     { [] }
| decl DOUBLESEMI prog                    { $1::$3 }
;
