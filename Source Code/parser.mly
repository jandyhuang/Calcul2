%{ 
open Printf
open Ast 
%}

%token PLUS MINUS TIMES DIVIDE INTDIVIDE POWER  MOD
%token ASSIGN DERIV INTEG SQRT SIN COS TAN ASIN ACOS ATAN LOG LN
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LVEC RVEC SEMI OUTPUT COMMA
%token IF ELSE FOR WHILE TO DOWNTO
%token RETURN

%token <int> LITERAL
%token <float> REAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right OUTPUT
%right ASSIGN
%left EQ NEQ
%left LT LEQ GT GEQ AND OR 
%left PLUS MINUS
%left TIMES DIVIDE INTDIVIDE
%right MOD POWER DERIV INTEG SQRT SIN COS TAN ASIN ACOS ATAN LOG LN NOT

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

/* Function declaration:
foo(x, y) {z=3;...} 
or
foo(x, y)=x+y;
*/ 
fdecl:
   ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $1;
	 formals = $3;
	 locals = List.rev $6;
	 body = List.rev $7 } }
  | ID LPAREN formals_opt RPAREN ASSIGN expr SEMI
     { { fname = $1; 
	 unknowns = $3;
	 formula = $6 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   ID ASSIGN expr SEMI{ { name = $1; value = $3; } }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | OUTPUT expr SEMI{ Output($2) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr POWER  expr { Binop($1, Pow,   $3) }
  | expr INTDIVIDE expr { Binop($1, IntDiv,  $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | expr EQ     expr { Binop($1, Eq, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }

  | expr DERIV  expr { Binop($1, Deriv,   $3) }
  | expr INTEG  expr { Binop($1, Integ,   $3) }

  | SQRT expr        {PreUnaop(Sqrt,$2)}
  | SIN expr         {PreUnaop(Sin,$2)}
  | COS expr         {PreUnaop(Cos,$2)}
  | TAN expr         {PreUnaop(Tan,$2)} 
  | ASIN expr        {PreUnaop(ASin,$2)} 
  | ACOS expr        {PreUnaop(ACos,$2)} 
  | ATAN expr        {PreUnaop(ATan,$2)}
  | LOG expr         {PreUnaop(Log,$2)} 
  | LN expr          {PreUnaop(Ln,$2)} 
  | NOT expr         {PreUnaop(Not,$2)}

  | LITERAL          {Num($1) }
  | REAL             {Real($1)}
  | ID               {Id($1)}

  | ID ASSIGN expr   {Assign($1,$3)} 
  | ID LPAREN actuals_opt RPAREN {Call($1,$3)}
  | LPAREN expr RPAREN {$2}
  
actuals_opt:
|{ [] }
| actuals_list { List.rev $1 }
actuals_list:
|expr { [$1] }
|expr COMMA actuals_list { $3 :: $1 }

  
vecter:
    LVEC float_list RVEC	{ List.rev $2 } 

float_list:
    /* nothing */		{ [] }
  | REAL COMMA REAL	{ $3 :: $1 }
    
