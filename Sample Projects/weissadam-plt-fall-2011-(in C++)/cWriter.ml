open Ast
open Validation
open Setuptypes

open Printf

(* Authors: Andrew Ingraham, Bill Warner, Adam Weiss *)

exception Unimplemented of string
exception BindingError of string

(* Prevents collisions with library functions *)
let funcname_mangle f = match f with
 | "main" -> f
 | _ -> "_" ^ f


let string_of_typespec = function
   IntType     -> "int"
 | SetType     -> "SetupSet"
 | FloatType   -> "float"
 | StringType  -> "std::string"
 | TupleType   -> "tuple"
 | BoolType    -> "bool"
 | NoType      -> "notype"
 | TupleSeq(_) -> "SetupTuple"
 | SetSeq(_) -> "SetupSet"

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

let empty_ctx =
  let evars = (Hashtbl.create 3) in
  ExecutionContext({evars = evars})


let evar_add var_name var_val cl =
  match cl with
  | ExecutionContext(c) -> Hashtbl.add c.evars var_name var_val
  | ECallers(c, cl) -> Hashtbl.add c.evars var_name var_val


let rec evar_get var_name cl =
  match cl with
  | ExecutionContext(c) -> 
       (try Hashtbl.find c.evars var_name
       with _ -> raise(UndeclaredVariable(var_name)))
  | ECallers(c, cl) ->
	try Hashtbl.find c.evars var_name
	with _ -> evar_get var_name cl

let rec string_of_funccall ?(fc=empty_ctx) s t =
  (funcname_mangle s) ^ "(" ^ (String.concat ", "
                                   (List.map (fun x -> string_of_expr fc x) t)) ^ ")"
and

 string_of_setliteral fc e =
  "SetFactory(" ^
  (String.concat ").add( " (List.map (fun x -> string_of_expr fc x) e)) ^
  ")"

and

 string_of_setbuilder fc expr sourcelist =
  let sc = empty_ctx in
  let ctor = compute_ctor_for_sb sc fc expr sourcelist in
  build_set sc fc ctor expr sourcelist

and

  build_set sc fc ctor expr sourcelist =
    " hello "

and

 string_of_expr fc expr =
    match expr with
 | NullExpr       -> ""
 | Assign(i,e)    -> i ^ " = " ^ string_of_expr fc e
 | Binop(e1, NDivide, e2) -> "static_cast<int>(" ^ (string_of_expr fc e1) ^ " / " ^ (string_of_expr fc e2) ^ ")"
 | Binop(e1, Intersect, e2)     -> "setIntersect (" ^ string_of_expr fc e1 ^ ", " ^ string_of_expr fc e2 ^ ")"
 | Binop(e1, Union, e2)     -> "setUnion (" ^ string_of_expr fc e1 ^ ", " ^ string_of_expr fc e2 ^ ")"
 | Binop(e1, Cross, e2)     -> "setCross (" ^ string_of_expr fc e1 ^ ", " ^ string_of_expr fc e2 ^ ")"
 | Binop(e1, SetMinus, e2)  -> "setMinus (" ^ string_of_expr fc e1 ^ ", " ^ string_of_expr fc e2 ^ ")"
 | Binop(e1, NotEquals, e2)  -> "!(" ^ string_of_expr fc e1 ^ " == " ^ string_of_expr fc e2 ^ ")"
 | Binop(e1, binop, e2) -> string_of_expr fc e1 ^ " " ^ string_of_binop binop ^ " " ^ string_of_expr fc e2
 | Unop(Cardinality,e)      -> "setCardinality (" ^ string_of_expr fc e ^ ")"

 | Unop(u,e)      -> string_of_unop u ^ string_of_expr fc e
 | Int(i)         -> string_of_int i
 | Boolean(i)     -> if (i==true) then "true" else "false"
 | Float(i)       -> i
 | String(i)      -> i
 | Tuple(e)       ->
     "TupleFactory(" ^ (String.concat ".add(" (List.map (fun x -> string_of_expr fc x) e)) ^ ")"
 | SetLiteral(e)  -> string_of_setliteral fc e
 | SetBuilder(e,s) -> string_of_setbuilder fc e s
 | SetRange(s,e)   -> string_of_setrange fc s e
 | Id(i)           -> i
 | FuncCall(s,t)   -> string_of_funccall s t


and 

 string_of_setrange fc s e =
  "SetFactory(" ^ string_of_expr fc s ^ ", " ^ string_of_expr fc e ^ ")"

and

 compute_ctor_for_sb sc fc expr sourcelist =
  List.iter (fun s ->
      match s with
	vname, expr ->
	  let vval =
	    match expr with
	    | SetRange(s,e) -> string_of_setrange fc s e
	    | SetLiteral(e) -> string_of_setliteral fc e
	    | SetBuilder(e,s) ->
		(evar_add vname "" sc;
		 let sc = push_ec sc sc in
		 compute_ctor_for_sb sc fc e s)
	    | Id(i) ->
		(try
		  (evar_get i sc) with _ ->
		    (evar_get i fc))
	    | _ -> raise(BindingError(vname ^ " isn't a set!"))
	  in
	  (evar_add vname vval sc)
      ) sourcelist;
  try (string_of_expr sc expr)
  with _ -> (string_of_expr fc expr)

and 

 string_of_stmt fc stmt =
  match stmt with
   Block(b)    -> "\n" ^ (String.concat "" (List.map (fun x -> string_of_stmt fc x) b)) ^ "\n"
 | Expr(e)     -> string_of_expr fc e ^ ";\n"
 | Decl(t,i,e) -> string_of_typespec t ^ " " ^ i ^
                  if (e=NullExpr) then
                    ";\n"
                  else
                    " = " ^ string_of_expr fc e ^ ";\n"
 | Return(e)   -> "return " ^ string_of_expr fc e ^ ";\n"
 | While(e,s)  -> "while(" ^ string_of_expr fc e ^ ") {" ^ string_of_stmt fc s ^ "}"
 | If(e,s1,s2) -> "if(" ^ string_of_expr fc e ^ ") { " ^ string_of_stmt fc s1 ^
                  "}else{ " ^ string_of_stmt fc s2 ^ "}"
 | Print(p)    -> "std::cout << std::boolalpha << (" ^ string_of_expr fc p ^ ") << std::endl;\n"

let string_of_formals formals =
  String.concat ", " (List.map
			(fun f -> string_of_typespec (fst f) ^ " " ^ snd f)
			formals)

let rec string_of_funcdef fc f =
 (string_of_typespec f.ret_type) ^ " " ^ (funcname_mangle f.func_name) ^ "(" ^ string_of_formals f.formals ^ "){" ^
 "\n" ^
 string_of_stmt fc f.body ^
 "\n}"

let rec string_of_program p =
  "#include <iostream>\n" ^
  "#include <string>\n#include \"setupBase.h\"\n" ^
 (String.concat "" (List.map (fun x -> string_of_stmt empty_ctx x) p.globals)) ^
 (String.concat "\n\n" (List.map (fun x -> string_of_funcdef empty_ctx x)  p.funcs))

let _ =
 let lexbuf = Lexing.from_channel stdin in
 let program = Parser.program Scanner.token lexbuf in
 let _ = validate_program program in
 let result = string_of_program program in
 let file = "test.cpp" in
 let oc = open_out file in
 fprintf oc "%s\n" result;
 close_out oc;

