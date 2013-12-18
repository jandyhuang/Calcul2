open Ast
open Printf

let gen_type = function
    "main" -> "int"
  | _ -> "double"


let var_list = ref []
let exist_id id id_list= List.exists (function x -> x = id) !id_list
let gen_var var =
	match (exist_id var var_list) with
	true -> var
  | false -> ignore(var_list := var :: !var_list); "double "^var
  
  
let rec gen_expr = function
    Real(l) -> string_of_float l
  | Id(s) -> gen_var s
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
          
          |        Pow -> "pow" | Deriv -> "'" | Integ -> "@"                                (*Pow and Deriv and Integ to be determined*)
          
          ) ^ " " ^
      gen_expr e2
  | Assign(v, e) -> gen_var v^ " = " ^ gen_expr e
  | Call(fname, el) -> fname ^ "(" ^ String.concat ", " (List.map gen_expr el) ^ ")"
  | Noexpr -> ""
  
(* help to generate f.push_back("unknowns") *)
let rec pad (unks, fname) =
  match unks with
    [] -> []
  | u :: tl -> (fname^".push_back(\""^u^"\");") :: (pad (tl, fname))

let rec get_index unk = function
    [] -> 0
  | h :: t -> if h = unk then 0 else 1 + get_index unk t

let new_node fname decls =
  fname ^ ".AddNode(new FNode(" ^ decls ^ "));\n\t"

let rec gen_tree_op (fname, unknowns) = function
    Real(l) -> new_node fname ("T_VAL,"^(string_of_float l))
  | Id(s) -> new_node fname ("T_VAR,0,"^(string_of_int (get_index s unknowns)))
  | Binop(e1, o, e2) -> 
      gen_tree_op (fname, unknowns) e1 ^
        ( match o with
            Add -> new_node fname "T_OP,0,PLUS" 
          | Sub -> new_node fname "T_OP,0,MINUS"
          | Mult -> new_node fname "T_OP,0,TIMES"
          | Div -> new_node fname "T_OP,0,DIVIDE"
          | And -> new_node fname "T_OP,0,AND"
          | Or -> new_node fname "T_OP,0,OR"
          | Eq -> new_node fname "T_OP,0,EQUAL"
          | Neq -> new_node fname "T_OP,0,NOTEQUAL"
          | Less -> new_node fname "T_OP,0,LESS"
          | Leq -> new_node fname "T_OP,0,LESSEQUAL"
          | Greater -> new_node fname "T_OP,0,GREATER"
          | Geq -> new_node fname "T_OP,0,GREATEREQUAL"
          | Pow -> new_node fname "T_OP,0,POWER"
          | Deriv -> new_node fname "T_OP,0,DERIV"
          | Integ -> new_node fname "T_OP,0,INTEG"
        )
      ^ gen_tree_op (fname, unknowns) e2
  | Noexpr -> ""

let rec construct_tree (fname, unknowns, formula) =
  gen_tree_op (fname, unknowns) formula

let add_fname (fname, unknowns) = 
  let dcl_fname = "vector<string> "^fname^"_var;\n\t" in
  let pbk_fname = String.concat "\n\t" (pad (unknowns, fname)) in
  let dcl_fval = "vector<double> "^fname^"_now;\n\t" in
  let dcl_tree = "FTree "^fname^"("^fname^"_var);\n\t" in 
    dcl_fname ^ pbk_fname ^ "\n\t" ^ dcl_fval ^ dcl_tree ^ "\n\t"

let add_tree (fname, unknowns, formula) =
  let cst_tree = construct_tree (fname, unknowns, formula) in
    cst_tree
  (*create_name^push_back*)

let rec pad_call (el, fname) =
  match el with
    [] -> []
  | u :: tl -> (fname^"_now.push_back(\""^u^"\");") :: (pad_call (tl, fname))

let rec gen_value fname = function
    Real(l) -> fname^"_now.pushback(\""^(string_of_float l)^"\");\n\t"

let rec gen_call_func = function
    Call(fname, el) -> fname ^ "_now.clear();\n\t" ^ String.concat "" (List.map (gen_value fname) el)
      ^ "printer = " ^ fname ^ ".GetValue(" ^ fname ^ "_now);\n\t" 
      ^ "printf(\"%lf\\n\",printer);\n\t"
  | _ -> ""

(* wait to be determined *) 
let gen_math (fname, unknowns, formula)= 
  let addfname = add_fname (fname, unknowns) in
  let addtree = add_tree (fname, unknowns, formula) in
    addfname ^ addtree
        (*fname ^ "(" ^ String.concat ", " unknowns ^ "){\n\t\t" ^ (gen_expr formula) ^ "\n\t}\n"*)

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
  
  | Output(e) -> gen_call_func e
  | Math_func(s, sl, e) -> gen_math (s, sl, e)
  

(*
module StringMap = Map.Make(String)
(* StringMap to store var names for each function, key is func name that in func_map, value is var_list *)
let func_map = StringMap.empty
(* add vars to var_map *)
let add_var (fname, var_list) = 
	List.map (fun(var) -> StringMap.add fname var func_map) var_list

(* needed to work on it *)  
let gen_var (fname, vname)= 
	match StringMap.find fname func_map with
    l when List.mem vname l == true -> vname
  | Not_found -> let func_map = StringMap.add fname ((StringMap.find fname func_map)::vname) func_map in "double "^vname
*)



  

let gen_formals formals = ignore(List.map (fun(x)->var_list := x :: !var_list) formals); List.map (fun(l) -> "double "^l) formals  
	
let gen_fdecl fdecl =
  let ftype = gen_type (fdecl.fname) in
  let fname = fdecl.fname in
  let formal_list = String.concat ", " (gen_formals fdecl.formals) in
  let body = String.concat "\n\t" (List.map gen_stmt fdecl.body) in
    ignore(var_list := []);
    match fname with
        "main" -> ftype^" "^fname^"("^formal_list^")\n{\n\tdouble printer;\n\n\t"^body^"\n\treturn 0;\n}\n"
      | _ -> ftype^" "^fname^"("^formal_list^")\n{\n\t"^body^"\n}\n"

let gen_program prog =
  let header = "#include <cstdio>\n#include \"calcul2.h\"\nusing namespace std;\n" in
    let fdecls = String.concat "\n" (List.map gen_fdecl prog) in
      let _ = print_endline "Code Generation completed successfully.\n" in
        header^"\n"^fdecls
