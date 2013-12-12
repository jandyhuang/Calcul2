(* binary operators *)
type op = Add | Sub | Mult | Div | Pow | Deriv | Integ | Eq | Neq | Less | Leq |
Greater | Geq | And | Or

(* unary operators *)
type preop = Sqrt | Sin | Cos | Tan | ASin | ACos | ATan | Log | Ln | Not

(* expressions *)
type expr =
    Real of float
  | Id of string
  | Binop of expr * op * expr
  | PreUnaop of preop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

(* statements *)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Output of expr
  | Math_func of string * string list * expr

(* declaration for varibles *)
(*
type var_decl = {
    name : string;
    value : float;
}
*)

(* declaration for math functions *)
(*
type math_func_decl = {
    fname : string;
    unknowns : string list;
    formula : expr;
}
*)

(* declaration for functions *)
type func_decl = {
    fname : string;
    formals : string list;
    body : stmt list;
}

(* definition for type of program *)
type program = string list * func_decl list

let string_of_vdecl (name, expr)= name ^ "=" ^ String.concat "" expr ^";\n"

let string_of_math (fname, unknowns, formula)= fname ^ "(" ^ String.concat ", " 	   unknowns ^ "){\n" ^ String.concat "" formula ^ "}\n"
(*
let rec string_of_expr = function
    Num(l) -> string_of_int l
  | Real(l) -> string_of_float l
  | Id(s) -> s
  | PreUnaop(preop, e) ->
	  "" ^
	  (match preop with
	  Sqrt -> "sqrt" | Sin -> "sin" | Cos -> "cos" | Tan -> "tan" | ASin -> "asin"
	  | ACos -> "acos" | ATan -> "atan" | Log -> "log" | Ln -> "ln" | Not -> "!")
	  ^ "(" ^ string_of_expr e
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
	  |	Pow -> "^" | IntDiv -> "//" | Deriv -> "'" | Integ -> "@"
	  | And -> "&&" | Or -> "||"
      | Eq -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  
  | Output(e) -> ":" ^ string_of_expr e
*)
