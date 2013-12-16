open Ast
open Printf


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
	fname ^ "(" ^ String.concat ", " unknowns ^ "){\n" ^ (gen_expr formula) ^ "}\n"
  
  
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
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ "){\n" ^
  String.concat "" (List.map gen_stmt fdecl.body) ^
  "}\n"

let gen_program prog =
  let header = "#include \"calcul2.h\"\n" in
    let body = String.concat "\n" (List.map gen_fdecl prog) in
      let _ = print_endline "Code Generation completed successfully.\n" in
        header^"\n"^body
