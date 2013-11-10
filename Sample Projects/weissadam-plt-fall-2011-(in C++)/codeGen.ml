open Ast
open Validation

open Printf

exception Unimplemented of string

(*
let string_of_typespec = function
    IntType     -> "int"
  | SetType     -> "set"
  | FloatType   -> "float"
  | StringType  -> "string"
  | TupleType   -> "tuple"
  | BoolType    -> "bool"
  | NoType      -> "notype"

let string_of_unop = function
    Negative -> "-"
  | Not      -> "!"
  | Cardinality -> "#"

let rec string_of_expr = function
  | NullExpr       -> ""
  | Unop(u,e)      -> string_of_unop u ^ string_of_expr e
  | Tuple(e)       -> "(" ^ (String.concat ", " (List.map string_of_expr e)) ^ ")"
  | SetBuilder(e,s) -> "{" ^ string_of_expr e ^ " | " ^
                      (String.concat ", " (List.map 
                                             (fun f -> (fst f) ^ " in " ^
                                                       string_of_expr (snd f))
                                             s)) ^ "}"
*)

(* FIXME - Must be changed so they take operands' types into account. *)
let string_of_binop = function
	Plus -> " + "
	| Minus -> " - "
	| Times -> " * "
	| Divide -> " / "
	| NDivide -> " // "
	| GrThan  -> ">"
	| LeThan  -> "<"
	| GrThanEq -> ">="
	| LeThanEq -> "<="
	| Equals   -> "="
	| NotEquals -> "<>"
	| LogicalAnd -> "&&"
	| LogicalOr  -> "||"
	|_ -> "TODO_sob"


let rec string_of_default = function
   	IntType     	-> "0"
  	| FloatType   	-> "0.0"
  	| StringType  	-> ""
  	| BoolType    	-> "false"  (* FIXME - Might want to be "nope" but I don't think so. *)
 	(*| SetType     -> *)       (* Should be an empty hashtbl *)
  	(*| TupleType   -> *)
	(*| NoType      -> "notype"*)
	| _		-> raise(Unimplemented("string_of_default function not implemented for this type"))



let rec srHelper acc s e = if (s > e) then acc
                           else srHelper (e::acc) s (e-1)


let rec string_of_expr = function 
	Int(i)                     -> string_of_int i
 	| Boolean(i)               -> string_of_bool i
  	| Float(i)                 -> i
	| Id(i)                    -> "_" ^ i ^ ".contents"

	(* Assignment needs to return value only if being assigned to something else *)
	| Assign(i, e)             -> (match e with 
                                 (*     Id(e) ->  "( _" ^ i ^ " := (_" ^ e ^ ".contents); ); "
	                              | Assign(i2,e2) -> "( _" ^ i ^ " := ((" ^ string_of_expr e ^ "); _" ^ i2 ^ ".contents); ); "*)
                                      Id(e) ->  " _" ^ i ^ " := (_" ^ e ^ ".contents); "
	                              | Assign(i2,e2) -> " _" ^ i ^ " := (" ^ string_of_expr e ^ "; _" ^ i2 ^ ".contents);  "
                                      | _   ->  " _" ^ i ^ " := (" ^ string_of_expr e ^ "); ")

	| Binop(e1, Union, e2)     -> "(union (" ^ string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ "))"
	| Binop(e1, Cross, e2)     -> "(cross (" ^ string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ "))"
	| Binop(e1, SetMinus, e2)  -> "(setminus (" ^ string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ "))"
	| Binop(e1, Intersect, e2) -> "(intersect (" ^ string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ "))"
	| Binop(e1,o,e2)           -> "(" ^ string_of_expr e1 ^ string_of_binop o ^ string_of_expr e2 ^ ")"
	| String(i)                -> i
	| FuncCall(s,t)            -> " (_" ^ s ^ " " ^ (String.concat " " (List.map string_of_expr t)) ^ ")"

	(* FIXME - To handle when e is an empty list.  Also, alter it to never insert duplicates *)
	| SetLiteral(e)            -> "(let temp = Hashtbl.create " ^ string_of_int (List.length e) ^ " in " ^ 
				      (String.concat "\n" (List.map (fun f -> " Hashtbl.add temp " ^ string_of_expr f ^
					     " " ^ (string_of_expr f) ^ "; ") e)) ^ " temp)" 

	  (* SetRange uses SetLiteral to construct the appropriate hashtbl.  The code is much more verbose than it ought to be because of *)
	  (* the necessity to convert between types before performing certain operations. *)
	| SetRange(s,e)            -> let sint = (int_of_string (string_of_expr s)) in 
                                      let eint = (int_of_string (string_of_expr e)) in
		                      if (sint < eint) then 
                                          string_of_expr ( SetLiteral( List.map (fun f -> Int(f) ) (srHelper [] sint eint) ) )
		                      else 
                                          string_of_expr ( SetLiteral( List.map (fun f -> Int(f) ) (srHelper [] eint sint) ) )

	| _                        -> "TODO_soe"


let rec string_of_func_body = function
	Block(b)-> (String.concat "\n" (List.map string_of_func_body b))
  	| Expr(e)     -> string_of_expr e 
(*	| Decl(t,i,e) -> "let _" ^ i ^ " = ref " ^ 
			 if(NullExpr = e) then
				(string_of_default t) ^ " in "
			 else
				(string_of_expr e) ^ " in "
*)

	| Decl(t,i,e) -> (match e with
			 NullExpr	  -> "let _" ^ i ^ " = ref " ^ (string_of_default t) ^ " in "
			 | Id(e)	  -> "let _" ^ i ^ " = ref (_" ^ e ^ ".contents) in "
		 	 | Assign(i2, e2) -> "let _" ^ i ^ " = ref ((" ^ string_of_expr e ^ "); _" ^ i2 ^ ".contents) in "
          		 | _ 		  -> "let _" ^ i ^ " = ref " ^ (string_of_expr e) ^ " in ")

	| Return(e) -> string_of_expr e
  	| If(e,s1,s2) -> "if(" ^ string_of_expr e ^ ") then " ^ string_of_func_body s1 ^
                   " else " ^ string_of_func_body s2
  	| While(e,s)  -> "while(" ^ string_of_expr e ^ ") " ^ " do " ^ string_of_func_body s ^ " done; " 
	(*| _ -> "TODO_sofb"*)

let rec string_of_formals formals = 
String.concat " " (List.map (fun f -> "_" ^ snd f) formals) (*int A, string B => _A _B *)

let rec add_mutable_local formals = 
String.concat " " (List.map (fun f -> "let _" ^ snd f ^ " = ref _" ^ snd f ^ " in \n") formals) 

let rec string_of_funcdef f = 
	"let rec _" ^ f.func_name ^ " " ^ (string_of_formals f.formals) ^ " = \n" 
	^ (add_mutable_local f.formals)  (* Creates mutable local variables equivalent to function args *)
	^ (string_of_func_body f.body)

(* FIXME Ast.program is now a record type.  funcs is the funcdef list you
 * know and love .. p.globals is a list of statements that are restricted
 * to variable declarations *)
let rec string_of_program p = 
  "open Printf\n" ^
  (String.concat "\n\n" (List.map string_of_funcdef p.funcs)) ^
  "\nlet _ = printf \"%d\n\" (_main (int_of_string Sys.argv.(1)))\n"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let _ = validate_program program in
  let result = string_of_program program in
  let file = "test.ml" in
  let oc = open_out file in 
  fprintf oc "%s\n" result;
  close_out oc;

