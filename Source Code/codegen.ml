open Ast
open Printf

let gen_type = function
    "main" -> "int"
  | _ -> "double"

let rec gen_expr = function
    Real(l) -> string_of_float l
  | Id(s) -> s
  | PreUnaop(preop, e) ->
	  "" ^
	  (match preop with
	  Sqrt -> "sqrt" | Sin -> "sin" | Cos -> "cos" | Tan -> "tan" | ASin -> "asin"
	  | ACos -> "acos" | ATan -> "atan" | Log -> "log" | Ln -> "ln" | Not -> "!")
	  ^ "(" ^ gen_expr e ^ ")"
  | Binop(e1, o, e2) ->
      gen_expr e1 ^ " " ^
      (match o with
		Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
	  | And -> "&&" | Or -> "||" | Eq -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
	  
	  |	Pow -> "pow" | Deriv -> "'" | Integ -> "@"				(*Pow and Deriv and Integ to be determined*)
	  
	  ) ^ " " ^
      gen_expr e2
  | Assign(v, e) -> v ^ " = " ^ gen_expr e
  | Call(fname, el) -> fname ^ "(" ^ String.concat ", " (List.map gen_expr el) ^ ")"
  | Noexpr -> ""
  
  
(* wait to be determined *) 
let gen_math (fname, unknowns, formula)= 
	fname ^ "(" ^ String.concat ", " unknowns ^ "){\n\t\t" ^ (gen_expr formula) ^ "\n\t}\n"
  
  
let rec gen_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map gen_stmt stmts) ^ "}\n"
  | Expr(expr) -> gen_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ gen_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ gen_expr e ^ ")\n" ^ gen_stmt s
  | If(e, s1, s2) ->  "if (" ^ gen_expr e ^ ")\n" ^
      gen_stmt s1 ^ "else\n" ^ gen_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ gen_expr e1  ^ " ; " ^ gen_expr e2 ^ " ; " ^
      gen_expr e3  ^ ") " ^ gen_stmt s
  | While(e, s) -> "while (" ^ gen_expr e ^ ") " ^ gen_stmt s
  
  | Output(e) -> "cout<<" ^ gen_expr e ^ ";\n"
  | Math_func(s, sl, e) -> gen_math (s, sl, e)
  
(* needed to work on it *)  
let gen_vdecl (name, expr)= "float " ^ name ^ "=" ^ String.concat "" expr ^";\n"

let gen_fdecl fdecl =
  let ftype = gen_type (fdecl.fname) in
  let fname = fdecl.fname in
  let formal_list = String.concat ", " fdecl.formals in
  let body = String.concat "\n\t" (List.map gen_stmt fdecl.body) in
    match fname with
        "main" -> ftype^" "^fname^"("^formal_list^")\n{\n\tdouble printer;\n\t"^body^"\n\tprintf(\"%lf\\n\",printer);\n\treturn 0;\n}\n"
      | _ -> ftype^" "^fname^"("^formal_list^")\n{\n\t"^body^"\n}\n"

let gen_program prog =
  let header = "#include <cstdio>\n#include \"calcul2.h\"\nusing namespace std;\n" in
    let fdecls = String.concat "\n" (List.map gen_fdecl prog) in
      let _ = print_endline "Code Generation completed successfully.\n" in
        header^"\n"^fdecls
