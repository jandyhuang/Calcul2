%{ 
open Printf
open Ast 
%}

%token PLUS MINUS TIMES DIVIDE POWER
%token ASSIGN DERIV INTEG SQRT SIN COS TAN ASIN ACOS ATAN LOG LN
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE SEMI OUTPUT COMMA
%token IF ELSE FOR WHILE
%token RETURN OUTPUT

%token DOLLAR
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
%left TIMES DIVIDE
%right POWER DERIV INTEG SQRT SIN COS TAN ASIN ACOS ATAN LOG LN NOT DOLLAR

%start program
%type <Ast.program> program

%%

program:
		{ [] }
 | fdecl program { $1 :: $2 }

/* 	 { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program math_fdecl { ($2 :: fst $1), snd $1 }
*/

/* Function declaration:
foo(x, y) {z=3;...} 
or
foo(x, y)=x+y;
*/ 

fdecl:
   ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { fname = $1;
	 formals = $3;
	 body = List.rev $6 } }

formals_opt:
    /* nothing */  { [] }
  | formal_list   { List.rev $1 }


formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }  
  
  
actuals_opt:
	/* nothing */	{ [] }
  | actuals_list { List.rev $1 }

actuals_list:
     expr 				   { [$1] }
  |actuals_list COMMA expr { $3 :: $1 }


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
  | ID LPAREN var_list RPAREN ASSIGN expr SEMI { Math_func($1, List.rev $3, $6) }

 
var_list:
	DOLLAR ID			{ [$2] }
  | var_list COMMA DOLLAR ID 		{ $4 :: $1 }
  
  
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr POWER  expr { Binop($1, Pow,   $3) }
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

  | REAL             {Real($1)}
  | ID               {Id($1)}

  | ID ASSIGN expr   {Assign($1,$3)} 
  | ID LPAREN actuals_opt RPAREN {Call($1,$3)}
  | LPAREN expr RPAREN {$2}
  

