{ open Parser }

let digit = ['0'-'9']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "#*"     { comment lexbuf }           (* Comments *)
| "##"     { comline lexbuf }           (* Comments in Line*)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { POWER }
| "//"     { INTDIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| '\''      { DERIV }
| '@'      { INTEG }
| "sqrt"   { SQRT }
| "sin"    { SIN }
| "cos"    { COS }
| "tan"    { TAN }
| "asin"   { ASIN }
| "acos"   { ACOS }
| "atan"   { ATAN }
| "log"    { LOG }
| "ln"     { LN }

| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| '!'      { NOT}

| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LVEC }
| ']'      { RVEC }
| ';'      { SEMI }
| ':'      { OUTPUT }
| ','      { COMMA }

| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| digit+ as lxm { LITERAL(int_of_string lxm) }
| (digit+'.'digit*)('e'['+''-']?digit+)?
| digit+'e'['+''-']?digit+ as lxm { REAL(float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*#" { token lexbuf }
| _    { comment lexbuf }

and comline = parse
  '\n' { token lexbuf }
| _    { comline lexbuf }
