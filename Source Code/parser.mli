type token =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | INTDIVIDE
  | POWER
  | MOD
  | ASSIGN
  | DERIV
  | INTEG
  | SQRT
  | SIN
  | COS
  | TAN
  | ASIN
  | ACOS
  | ATAN
  | LOG
  | LN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | NOT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LVEC
  | RVEC
  | SEMI
  | OUTPUT
  | COMMA
  | IF
  | ELSE
  | FOR
  | WHILE
  | RETURN
  | LITERAL of (int)
  | REAL of (float)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
