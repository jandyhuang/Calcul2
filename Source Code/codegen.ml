open Ast
open Printf

(* generate the return type of a function *)
let gen_type = function
    "main" -> "int"
  | _ -> "double"

(* variable list inside a function *)
let var_list = ref []

(* math function list inside a function *)
let mathf_list = ref []

let exist_id id id_list= List.exists (function x -> x = id) !id_list

let gen_var var =
	match (exist_id var var_list) with
	true -> var
  | false -> ignore(var_list := var :: !var_list); "double "^var
  
(* generate the cpp code for expression *)
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
      (match o with 
        Pow -> "pow(" ^ gen_expr e1 ^ ", " ^ gen_expr e2 ^ ")"
      | op -> gen_expr e1 ^ " " ^
      	  (match op with
            Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
          | And -> "&&" | Or -> "||" | Eq -> "==" | Neq -> "!="
      	  | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
          | _ -> ""
          (*| Pow -> "pow" | Deriv -> "'" | Integ -> "@" *)   (*Pow and Deriv and Integ to be determined*)
          
          ) ^ " " ^ gen_expr e2
	     )
  | Assign(v, e) -> gen_var v^ " = " ^ gen_expr e
  | Call(fname, el) -> fname ^ "(" ^ String.concat ", " (List.map gen_expr el) ^ ")"
  | Noexpr -> ""
  
(* help to generate f.push_back("unknowns") *)
let rec pad (unks, fname) =
  match unks with
    [] -> []
  | u :: tl -> (fname^"_var.push_back(\""^u^"\");") :: (pad (tl, fname))

(* get the index of a given unknown *)
let rec get_index unk = function
    [] -> 0
  | h :: t -> if h = unk then 0 else 1 + get_index unk t

(* pad the new node code for a given function with declarations *)
let new_node fname decls =
  fname ^ ".AddNode(new FNode(" ^ decls ^ "));\n\t"

(* pad the add node code for a given function with declarations *)
let add_node fname decls =
  fname ^ ".AddNode(" ^ decls ^ ");\n\t"

(* generate the math arguments *)
let gen_math_args = function
    Id(s) -> s
  | Real(l) -> string_of_float l
  | _ -> ""

(* recursion function to generate the tree operator structures *)
let rec gen_tree_op (fname, unknowns) = function
    Real(l) -> new_node fname ("T_VAL,"^(string_of_float l))
  | Id(s) -> 
    ( match (exist_id s mathf_list) with
        true -> add_node fname (s^".Copy()")
      | false -> new_node fname ("T_VAR,0,"^(string_of_int (get_index s unknowns)))
    )
  | Binop(e1, o, e2) -> 
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
          | Deriv -> add_node fname (gen_math_args e1 ^ "." ^ "Derive(\"" 
                     ^ gen_math_args e2 ^ "\") -> Copy()")
          | _ -> ""
          (*| Integ -> new_node fname "T_OP,0,INTEG"*)
        )
        ^ 
        ( match o with
            Deriv -> ""
          | _ -> gen_tree_op (fname, unknowns) e1 
            ^ gen_tree_op (fname, unknowns) e2
        )
  | PreUnaop(preop, e) ->
        ( match preop with
            Sqrt -> new_node fname "T_OP,0,SQRT" 
          | Sin -> new_node fname "T_OP,0,SIN"
          | Cos -> new_node fname "T_OP,0,COS"
          | Tan -> new_node fname "T_OP,0,TAN"
          | ASin -> new_node fname "T_OP,0,ASIN"
          | ACos -> new_node fname "T_OP,0,ACOS"
          | ATan -> new_node fname "T_OP,0,ATAN"
          | Log -> new_node fname "T_OP,0,LOG"
          | Ln -> new_node fname "T_OP,0,LN"
          | Not -> new_node fname "T_OP,0,NOT"
        )
        ^ gen_tree_op (fname, unknowns) e
  | Noexpr -> ""
  | _ -> ""

(* construct the tree structure *)
let rec construct_tree (fname, unknowns, formula) =
  gen_tree_op (fname, unknowns) formula

(* add the function name and tree declarations *)
let add_fname (fname, unknowns) = 
  let _ = match (exist_id fname mathf_list) with
      false -> ignore(mathf_list := fname :: !mathf_list); "" 
    | _ -> "" in
  let dcl_fname = "vector<string> "^fname^"_var;\n\t" in
  let pbk_fname = String.concat "\n\t" (pad (unknowns, fname)) in
  let dcl_fval = "vector<double> "^fname^"_begin, "^fname^"_end, "^fname^"_now;\n\t" in
  let dcl_tree = "FTree "^fname^"("^fname^"_var);\n\t" in 
    dcl_fname ^ pbk_fname ^ "\n\t" ^ dcl_fval ^ dcl_tree ^ "\n\t"

(* construct the tree *)
let add_tree (fname, unknowns, formula) =
  let cst_tree = construct_tree (fname, unknowns, formula) in
    cst_tree

(* pad the cpp code for push_back *)
let rec pad_call (el, fname) =
  match el with
    [] -> []
  | u :: tl -> (fname^"_now.push_back(\""^u^"\");") :: (pad_call (tl, fname))

(* generate the cpp code for getting values of a function *)
let rec gen_value fname = function
    s -> fname^"_now.push_back("^gen_expr s^");\n\t"
  | _ -> ""
(*
    Real(l) -> fname^"_now.push_back("^(string_of_float l)^");\n\t"
  | Id(s) -> fname^"_now.push_back("^s^");\n\t"
  | _ -> "" 
*)
(* generate the cpp code for setting the integal arguments *)
let rec gen_begin_end (fname, el) =
    fname ^ "_begin.clear();\n\t" ^ fname ^ "_end.clear();\n\t" ^
    (match el with
      a :: b :: _ -> fname ^ "_begin.push_back(" ^ gen_math_args a ^ ");\n\t" ^
                     fname ^ "_end.push_back(" ^ gen_math_args b ^ ");\n\t"
    | _ -> "")

(* generate the cpp code for setting the integal arguments *)
let gen_integ fname = function
      Call(unk, el) -> gen_begin_end (fname, el)
    | _ -> ""

(* generate the cpp code for getting the integal value *)
let get_integ fname =
    fname ^ ".GetIntegal(" ^ fname ^ "_begin, " ^ fname ^ "_end);\n\t"

(* handles a call expression *)
let rec gen_call_func = function
    Call(fname, el) as _func -> if exist_id fname mathf_list then (
	fname ^ "_now.clear();\n\t" ^ String.concat "" (List.map (gen_value fname) el)
      ^ "printer = " ^ fname ^ ".GetValue(" ^ fname ^ "_now);\n\t" 
      ^ "printf(\"%lf\\n\",printer);\n\t" ^ "cout << \"\\n\";\n\t")
				else "cout << " ^ gen_expr _func ^ "<<endl;\n\t"
  | Binop(e1, o, e2) as _expr -> 
        (  match o with
            Deriv -> gen_math_args e1 ^ "_now.clear();\n\t" ^ gen_math_args e1 ^ "." ^
                      "Derive(\"" ^ gen_math_args e2 ^ "\") -> Print();\n\t"
          | Integ -> gen_integ (gen_math_args e1) e2 ^ "cout << " ^ get_integ (gen_math_args e1)
          | _ -> "cout << " ^ gen_expr _expr ^ ";\n\t"
        ) ^ "cout << \"\\n\";\n\t"
  | Id(s) -> 
    (  match (exist_id s mathf_list) with
        true -> s ^ ".Print();\n\t"
      | false -> "cout << " ^ s ^ ";\n\t"
    ) ^ "cout << \"\\n\";\n\t"
  | _ -> "" 


(* generate the math declaration and create tree structure *)
let gen_math (fname, unknowns, formula)= 
  let addfname = add_fname (fname, unknowns) in
  let addtree = add_tree (fname, unknowns, formula) in
    addfname ^ addtree
        (*fname ^ "(" ^ String.concat ", " unknowns ^ "){\n\t\t" ^ (gen_expr formula) ^ "\n\t}\n"*)

(* generate the statement *)
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

(* generate the cpp code for formals, if it apears the first time, then give the type definition *)
let gen_formals formals = ignore(List.map (fun(x)->var_list := x :: !var_list) formals); List.map (fun(l) -> "double "^l) formals  

(* generate the cpp code for function declaration *)
let gen_fdecl fdecl =
  let ftype = gen_type (fdecl.fname) in
  let fname = fdecl.fname in
  let formal_list = String.concat ", " (gen_formals fdecl.formals) in
  let body = String.concat "\n\t" (List.map gen_stmt fdecl.body) in
    ignore(var_list := []);ignore(mathf_list := []);
    match fname with
        "main" -> ftype^" "^fname^"("^formal_list^")\n{\n\tdouble printer;\n\n\t"^body^"\n\treturn 0;\n}\n"
      | _ -> ftype^" "^fname^"("^formal_list^")\n{\n\t"^body^"\n}\n"

(* entrence of codegen, generate the cpp code for our calcul2 code *)
let gen_program prog =
  let header = "#include <cstdio>\n#include \"calcul2.h\"\nusing namespace std;\n" in
    let fdecls = String.concat "\n" (List.map gen_fdecl prog) in
      let _ = print_endline "Codegen completed.\nCompiling..\n" in
        header^"\n"^fdecls
