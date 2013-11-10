open Ast

let string_of_typespec = function
    IntType     -> "int"
  | SetType     -> "set"
  | FloatType   -> "float"
  | StringType  -> "string"
  | TupleType   -> "tuple"
  | BoolType    -> "bool"
  | NoType      -> "notype"
  | TupleSeq(_) -> "tuple_seq" (* will expand soon *)
  | SetSeq(_)   -> "set_seq"

let string_of_binop = function
    Plus  -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | NDivide -> "//"
  | GrThan  -> ">"
  | LeThan  -> "<"
  | GrThanEq -> ">="
  | LeThanEq -> "<="
  | Equals   -> "=="
  | NotEquals -> "!="
  | LogicalAnd -> "&&"
  | LogicalOr  -> "||"
  | Union -> "union"
  | Cross -> "cross"
  | SetMinus -> "minus"
  | Intersect -> "intersect"

let string_of_unop = function
    Negative -> "-"
  | Not      -> "!"
  | Cardinality -> "#"

let rec string_of_expr = function
  | NullExpr       -> ""
  | Assign(i,e)    -> i ^ " = " ^ string_of_expr e
  | Binop(e1,o,e2) -> string_of_expr e1 ^ string_of_binop o ^ string_of_expr e2
  | Unop(u,e)      -> string_of_unop u ^ string_of_expr e
  | Int(i)         -> string_of_int i 
  | Boolean(i)     -> if (i==true) then "yup" else "nope" 
  | Float(i)       -> i
  | String(i)      -> i
  | Tuple(e)       -> "(" ^ (String.concat ", " (List.map string_of_expr e)) ^ ")"
  | SetLiteral(e)  -> "{" ^ 
                      (String.concat " " (List.map string_of_expr e)) ^
                     "}"
  | SetBuilder(e,s) -> "{" ^ string_of_expr e ^ " | " ^
                      (String.concat ", " (List.map 
                                             (fun f -> (fst f) ^ " in " ^
                                                       string_of_expr (snd f))
                                             s)) ^ "}"
  | SetRange(s,e)   -> "{" ^ string_of_expr s ^ "..." ^ string_of_expr e ^
                       "}"
  | Id(i)           -> i
  | FuncCall(s,t)   -> s ^ "(" ^ (String.concat ", " 
                                    (List.map string_of_expr t)) ^ ")"

let rec string_of_stmt = function
    Block(b)    -> "{\n" ^ (String.concat "" (List.map string_of_stmt b)) ^ "}\n"
  | Expr(e)     -> string_of_expr e ^ ";\n"
  | Decl(t,i,e) -> string_of_typespec t ^ " " ^ i ^
                   if (e=NullExpr) then
                     ";\n"
                   else
                     " = " ^ string_of_expr e ^ ";\n"
  | Return(e)   -> "return " ^ string_of_expr e ^ ";\n"
  | While(e,s)  -> "while(" ^ string_of_expr e ^ ") " ^ string_of_stmt s 
  | If(e,s1,s2) -> "if(" ^ string_of_expr e ^ ") then " ^ string_of_stmt s1 ^
                   "else " ^ string_of_stmt s2
  | Print(e)    -> "print(" ^ string_of_expr e ^ ")"


let string_of_formals formals =
  String.concat ", " (List.map 
                        (fun f -> string_of_typespec (fst f) ^ " " ^ snd f)
                        formals)

let rec string_of_funcdef f =
  "function " ^ f.func_name ^ "[" ^ string_of_formals f.formals ^ "]" ^ " returns " ^
  string_of_typespec f.ret_type ^ 
  "\n" ^ 
  string_of_stmt f.body ^ 
  "\n"

let rec string_of_program p =
  String.concat "" (List.map string_of_stmt p.globals) ^ "\n" ^
  String.concat "\n\n" (List.map string_of_funcdef p.funcs)
