{ open Parser }
(* Author Adam Weiss *)
(* for floats *)
let digit = ['0'-'9']
let sign = ['+' '-']
let exp = ('e' sign? digit+)

rule token = parse
(* eat whitespace *)
  [' ' '\t' '\r' '\n'] { token lexbuf }

(* eat comments *)
| "/*"        { comment lexbuf }

(* funcdefs *)
| "function"  { FUNCTION }
| '{'         { LBRACE } 
| '}'         { RBRACE } 
| '['         { LBRACKET }
| ']'         { RBRACKET }
| "returns"   { RETURNS }
| "return"    { RETURN }

(* types *)
| "int"       { INT }
| "set"       { SET }
| "float"     { FLOAT }
| "string"    { STRING }
| "tuple"     { TUPLE }
| "bool"      { BOOL }

(* binops *)
| '+'         { PLUS }
| '-'         { MINUS }
| '*'         { TIMES }
| '/'         { DIVIDE }
| "//"        { NDIVIDE }
| '<'         { LETHAN }
| '>'         { GRTHAN }
| "<="        { LETHANEQ }
| ">="        { GRTHANEQ }
| "=="        { EQUALS }
| "!="        { NOTEQUALS }
| "&&"        { LOGICALAND }
| "||"        { LOGICALOR }
| "union"     { UNION }
| "cross"     { CROSS }
| "minus"     { SETMINUS }
| "intersect" { INTERSECT }

(* unops *)
| '#'         { POUND }
| '!'         { NOT }

(* statements / funcalls *)
| '('         { LPAREN }
| ')'         { RPAREN }
| "while"     { WHILE }
| "if"        { IF }
| "then"      { THEN }
| "else"      { ELSE }
| "print"	  { PRINT }

(* dregs *)
| '='        { ASSIGN }
| ';'        { SEMI }
| ','        { COMMA }
| '|'        { PIPE }
| "in"       { IN }
| "..."      { ELLIPSIS }
| "yup"      { YUP }
| "nope"     { NOPE }

| digit* '.' digit+ exp? as lxm { FLOAT_LITERAL(lxm) }
| digit+ '.' digit* exp? as lxm { FLOAT_LITERAL(lxm) }
| digit+ exp as lxm             { FLOAT_LITERAL(lxm) }
| ['0'-'9']+ as lxm             { INT_LITERAL(int_of_string lxm) }
| '"' [^'"']* '"' as lxm        { STRING_LITERAL(lxm) }

| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) } 
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
