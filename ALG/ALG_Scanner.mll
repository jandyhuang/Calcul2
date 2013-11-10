{
  open Printf
	open ALG_Parser
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let string=['a'-'z' 'A'-'Z' '0'-'9' '_' ' ']
let ignore=[' ' '\t' '\r' '\n']

rule token=parse
|ignore {token lexbuf}
| "(!" 	{comment lexbuf}
| "!!"	{comment1 lexbuf}
(*type key words*)
|"int" {INT_KEY}|"float" { FLOAT_KEY } |"string" {STRING_KEY}
|"array" {ARRAY_KEY}|"boolean" {BOOLEAN_KEY}|"void" {VOID_KEY}
(*geometry type key words*)
|"point" {POINT_KEY}|"line" {LINE_KEY}|"polygon" {POLYGON_KEY}|"ellipse" {ELLIPSE_KEY}
(*control key words*)
|"for" {FOR}|"while" {WHILE}|"if" {IF}
| "else" {ELSE}|"elseif" {ELSEIF}|"case" {CASE}|"return" {RETURN}
| "switch" {SWITCH}|"goon" {CONTINUE}|"done" {BREAK}
| "def" {DEF}|"default" {DEFAULT}
| "and" {AND} |"not" {NOT}|"or" {OR}
| "\"" string* "\"" as str {String(str)}
|"+" {PLUS} |"-." {MINUS}
|id		as identifier {ID(identifier)}
|"-"?digit+ as int_num {INT(int_of_string int_num)}  
|"-"?(digit+'.'digit*)('e'['+' '-']?digit+)?
|"-"?digit+'e'['+' '-']?digit+  as float_num {NUM(float_of_string float_num)}
(*punctuation*)
|"(" {LPAREN} | ")" {RPAREN} | "{" {LBRACE} | "}" {RBRACE} 
| "[" {LBRACKET } | "]" {RBRACKET } |"," {COMMA } | ";" {SEMI}
(*Geometry operators*)
|"//" {PARA}|"^" {INTERS}|"|-" {RELAT}
|"==" {EE}|"!=" {NE}
|"<<=" {SSE}|">>=" {LLE}
|"<<" {SS}|">>" {LL}
|"<=" {SE}|">=" {LE}
|"<" {S}|">" {L}
|"~>" {TL}|"~<"{TS}|"~~>"{TTL}|"~~<"{TTS}|"~="{TE}
|"~" {T}
|":=" {QE}
(*basic operators*)
| "*" {MUL}
|"/" {DIV}|"%" {PERC} |"=" {E}
|eof  	{EOF}
|_ 	as char		{raise (Failure("SCANNER:illegal input"^Char.escaped char))}

and comment =parse
| "!)" {token lexbuf}
| _		{comment lexbuf}

and comment1=parse
|"\n" {token lexbuf}
| _ {comment1 lexbuf}
