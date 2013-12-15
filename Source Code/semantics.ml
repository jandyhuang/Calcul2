open Ast

(* is to be modified according to the AST*)
type env = {
	mutable functions : func_decl list;
	variables : string list;
	}
	
(*It is used to test whether a function's name is equal to a string 'name'*)

let func_equal_name name = function
	| func -> func.fname = name
	
(*This function is to check whether a function's name has been defined more than once*)

let fun_exist func env = 
	let name = func.fname in
	   try
			   let _ = List.find (func_equal_name name) env.functions in
				   let e = "Function whose name is "^ name ^" has been defined more than once" in
					     raise (Failure e)
			with Not_found -> false
			
(*This function is to check whether a function's name exist in the env*)
let func_name_exist name env = List.exists (func_equal_name name) env.functions

(*This function will directly give you the function object if you give its name*)
let return_func_given_name name env = 
	try
		   let result = List.find (func_equal_name name) env.functions in
			      result
  with Not_found -> raise(Failure("Function "^ name ^ " has not been declared!"))
  
  (*check whether a 'fpname' is in the formal parameter list of a function*)
let exist_formal_para func fpname = 
	let check func fpname = List.exists (function a -> a = fpname) func.formals in
	  check func fpname
	  
(*check whether a 'vname' is declared in the global variable list*)
		
let exist_global_variable env vname = 
	let check env vname = List.exists (function a -> a= vname) env.variables in
	  check env vname
	  
	  
(*check the first existence*)



(*A function to check whether a function has a parameter called fpname*)
let check_exist_para_in_fun func fpname = List.exists (function a->a = fpname) func.formals


(*The function will check whether a function name "fun_name" has a corresponding function in the environment*)

let find_func func_name env = 
	try 
		 let _ = List.find (func_equal_name func_name) env.functions in
		      true
	with Not_found -> raise(Failure("Function "^ func_name ^" is not found in the function defination list"))
	
	(*This function will check whether a (var_type*string) has a para_name that appears more than once in a function's parameter list*)	
let count_fpara func = function a
  -> let f count b = 
		  if b=a then count+1
			else count
			in
			  let count = List.fold_left f 0 func.formals in
			     if count > 1
					then raise(Failure("Duplicate parameter in function " ^ func.fname))
					else
						count


(*This function will automatically check whether there is parameter duplication in function defination*)
						
let check_fpara_duplicate func = 
	List.map (count_fpara func) func.formals
	
(*This function will automatically check whether there is parameter duplication in function defination*)
						
let check_fpara_duplicate func = 
	List.map (count_fpara func) func.formals

(*This function will check whether a (var_type*string) has a var_name that appears more than once in a function's local variable list*)
(*)
let count_var func = function (_,b)
  -> let f count (_,c) = 
		  if c=b then count+1
			else count
			in
			  let count = List.fold_left f 0 func.locals in
			     if count > 1
					then raise(Failure("Duplicate parameter "^ b ^ " in function " ^ func.fname))
					else
						count
						*)
(*The following function will judge whether an expression is assign or call*)

let is_assign_call func = function
	| Assign(_,_) ->true
	| Call(_,_) ->true
	| _ ->false
	
	
(*The following function will judge whether an expression is assign or call*)

let is_assign_call func = function
	| Assign(_,_) ->true
	| Call(_,_) ->true
	| _ ->false
	
	
(*Checks that a return statement is present in the given function. *)
let has_return_stmt func =
	let stmt_list = func.body in
		 let f id_list stmt = 
			match stmt with
			|Expr(expr) -> match expr with 
	                               |Assign(id,expr2)-> id::id_list
	                               |_ -> id_list  
			|_ -> id_list in
		  let ids_list = List.fold_left f [] stmt_list in
			   ids_list  


(*THis will check each function's validity*)
let check_func f env =
		let _dup_name = fun_exist f env in
                 	let _ = env.functions <- (f) ::env.functions in
		   let _dup_formals = check_fpara_duplicate f in
			   (*let _dup_vlocals = check_var_duplicate f in*)
					(*let _vbody = check_valid_body f env in*)
				   		true


(*check whether there is a main function*)
let exists_main env = 
	if func_name_exist "main" env
	   then true else raise(Failure("No Main Function exist!"))
	   
let equal_variable_name a b = 
	a = b

let exist_v_name vlist vdecl = 
	let new_fun count x = 
		if(equal_variable_name vdecl x) then count+1 else count in
		 let result = List.fold_left new_fun 0 vlist in
		   if result <=1 then true else raise(Failure("Global Variable has been redefined!"))
	
let dup_in_global env = 
	 List.for_all (exist_v_name env.variables) env.variables
	 
let check_program (*(var_list,*)fun_list(*)*) = 
	let env = {functions = [] (*built_in*);variables = [] (*var_list*)} in
         let _global_check = dup_in_global env in
	let _dovalidation = List.map (fun f -> check_func f env) fun_list in
	   let  _mainexist = exists_main env in
		   let _ = print_endline "\nThe semantic check has been finished!\n" in
			true 
