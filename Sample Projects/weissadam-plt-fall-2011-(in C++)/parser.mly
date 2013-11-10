%{ open Ast %}
/* Author: Adam Weiss */
%token EOF ASSIGN SEMI PIPE IN ELLIPSIS YUP NOPE

/* funcdefs */
%token FUNCTION LBRACE RBRACE LBRACKET RBRACKET RETURNS RETURN 

/* types */
%token INT SET FLOAT STRING TUPLE BOOL

/* binops */
%token PLUS MINUS TIMES DIVIDE NDIVIDE LETHAN GRTHAN LETHANEQ GRTHANEQ
%token EQUALS NOTEQUALS LOGICALAND LOGICALOR UNION CROSS SETMINUS INTERSECT

/* unops */
%token POUND NOT

/* statements / funcalls */
%token LPAREN RPAREN COMMA WHILE IF THEN ELSE PRINT

%token <int>    INT_LITERAL
%token <string> STRING_LITERAL
%token <string> FLOAT_LITERAL
%token <string> ID 

%right ASSIGN
%left PLUS MINUS
%left EQUALS NOTEQUALS LOGICALAND LOGICALOR UNION CROSS SETMINUS INTERSECT
%left TIMES DIVIDE NDIVIDE
%left NOT POUND
%left LETHAN GRTHAN LETHANEQ GRTHANEQ
%left UMINUS

%start program 
%type < Ast.program> program 

%%

program:
  /* empty */ { { funcs = []; globals = [] } }
  | func_def program { { funcs = $1::$2.funcs; globals = $2.globals } }
  | var_decl program { { funcs = $2.funcs;     globals = $1::$2.globals } }

var_decl:
  typespec ID SEMI  { Decl($1, $2, NullExpr) }

stmt_list:
    stmt            { [$1] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI           { Expr($1) }
  | typespec ID SEMI  { Decl($1, $2, NullExpr) }
  | typespec ID ASSIGN expr SEMI { Decl($1, $2, $4) }
  | RETURN expr SEMI  { Return($2) }
  | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE 
    { While($3, Block($6)) }
  | IF LPAREN expr RPAREN THEN LBRACE stmt_list RBRACE
    ELSE LBRACE stmt_list RBRACE
    { If($3, Block($7), Block($11)) }
  | PRINT LPAREN expr RPAREN SEMI	{ Print($3) }

func_def:
  FUNCTION ID LBRACKET formals_list RBRACKET RETURNS typespec 
  LBRACE stmt_list RBRACE 
  { { 
    func_name = $2;
    formals = $4;
    ret_type = $7;
    body = Block($9);
     } }

formals_list:
  /* empty */ { [ ] }
  | formals_tail { $1 }

formals_tail:
  formal { [$1] }
  | formal COMMA formals_tail { $1::$3 }

formal:
  typespec ID { ($1, $2) }

typespec:
  | INT       { IntType }
  | SET       { SetType }
  | FLOAT     { FloatType }
  | STRING    { StringType }
  | TUPLE     { TupleType }
  | BOOL      { BoolType }

expr:
    ID ASSIGN expr            			{ Assign($1, $3) }
  | expr PLUS expr            			{ Binop($1, Plus, $3) }
  | expr MINUS expr           			{ Binop($1, Minus, $3) }
  | expr TIMES expr           			{ Binop($1, Times, $3) }
  | expr DIVIDE expr         			{ Binop($1, Divide, $3) }
  | expr NDIVIDE expr        			{ Binop($1, NDivide, $3) }
  | expr LETHAN expr          			{ Binop($1, LeThan, $3) }
  | expr GRTHAN expr          			{ Binop($1, GrThan, $3) }
  | expr LETHANEQ expr        			{ Binop($1, LeThanEq, $3) }
  | expr GRTHANEQ expr        			{ Binop($1, GrThanEq, $3) }
  | expr EQUALS expr          			{ Binop($1, Equals, $3) }
  | expr NOTEQUALS expr       			{ Binop($1, NotEquals, $3) }
  | expr LOGICALAND expr      			{ Binop($1, LogicalAnd, $3) }
  | expr LOGICALOR expr       			{ Binop($1, LogicalOr, $3) }
  | expr UNION expr           			{ Binop($1, Union, $3) }
  | expr INTERSECT expr           			{ Binop($1, Intersect, $3) }
  | expr CROSS expr			            { Binop($1, Cross, $3) }
  | expr SETMINUS expr        			{ Binop($1, SetMinus, $3) }
  | MINUS expr %prec UMINUS   			{ Unop(Negative, $2) }
  | NOT expr                		  	{ Unop(Not, $2) }
  | POUND expr                			{ Unop(Cardinality, $2) } 
  | ID LPAREN arglist RPAREN  			{ FuncCall($1, $3) }
  | ID                       		 	{ Id($1) }
  | INT_LITERAL               			{ Int($1) }
  | STRING_LITERAL            { String($1) }
  | FLOAT_LITERAL             { Float($1) } /* FIXME */
  | YUP                       { Boolean(true) }
  | NOPE                      { Boolean(false) }
  | LPAREN exprlist RPAREN    { Tuple($2) }
  | LBRACE exprlist RBRACE    { SetLiteral($2) }
  | LBRACE expr PIPE sourcelist RBRACE             { SetBuilder($2,$4) }
  | LBRACE expr ELLIPSIS expr RBRACE { SetRange($2,$4) }

sourcelist:
    ID IN expr                  { [($1,$3)] }
  | ID IN expr COMMA sourcelist { ($1,$3)::$5 }

arglist:
    /* empty */ { [] }
  | exprlist    { $1 }

exprlist:
  expr           { [$1] }
  | expr COMMA exprlist { $1::$3 } 

