open Ast

(* is to be modified according to the AST*)

type env = {
	mutable functions : func_decl list;
}
	
(* a function to test whether a function's name is equal to a string 'name'*)
let func_equal_name name = function
	| func -> func.fname = name
	
(* a function to check whether a function's name has been defined more than once*)
let fun_exist func env = 
	let name = func.fname in
	   try
			   let _ = List.find (func_equal_name name) env.functions in
				   let e = "Function whose name "^ name ^" has been defined more than once" in
					     raise (Failure e)
			with Not_found -> false
			
(*a function to check whether a function's name exist in the env*)
let exist_func_name name env = List.exists (func_equal_name name) env.functions

(*a function to return the function object if you give its name*)
let get_func_by_name name env = 
	try
		   let result = List.find (func_equal_name name) env.functions in
			      result
  with Not_found -> raise(Failure("Function "^ name ^ " has not been declared!"))
  	  

(*a function to check whether a function has a parameter appears more than once*)  
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
						
let check_fpara_duplicate func = 
	List.map (count_fpara func) func.formals


(* a function to check whether there is a main function*)
let exists_main env = 
	if exist_func_name "main" env
	   then true else raise(Failure("No Main Function exist!"))
	   
(*a function to check whether a id is in a list*)
let exist_id id id_list= List.exists (function x -> x = id) id_list

(*a function to check whether a fname is in a list*)
let exist_mathf fname mathf_list = List.exists (function ( a, _) -> a = fname) mathf_list


(*a function to get math function given the math function name*)
let get_math_fun_by_name fname mathf_list = 
	try
		   let result = List.find (function ( a, _) -> a = fname) mathf_list  in
			      result
        with Not_found -> raise(Failure("Math function "^ fname ^ " has not been declared!"))

(*a function to check whether a math function name is valid*)        
let check_math_func_name_valid mfname mathf_list id_list funcformal env= 
        if (exist_mathf mfname mathf_list)||(exist_id mfname id_list)||(exist_func_name mfname env)||(exist_id mfname funcformal)
        then raise(Failure("Math function name: "^ mfname ^ " has been used!"))
        else true

(*a function to check whether a math function's parameter are valid*)    
let rec check_math_func_para_valid mfname mathf_list id_list  funcformal env = function
        [] -> true
        |hd::tl -> 
           if (exist_mathf hd mathf_list) then raise(Failure("Math function parameter not valid!"))
           else if (exist_id hd id_list) then raise(Failure("Math function parameter not valid!"))
           else if (exist_func_name hd env) then raise(Failure("Math function parameter not valid!"))
            else if (exist_id hd funcformal) then raise(Failure("Math function parameter not valid!"))

           else check_math_func_para_valid mfname mathf_list id_list funcformal env tl

(*a function to check whether a math function has a parameter appears more than once*)           
let count_para paralist = function 
  b -> let f count c = if c=b then count+1 else count
	   in
	      let count = List.fold_left f 0 paralist in
			     if count > 1
					then raise(Failure("Math function has Duplicate parameter!"))
					else
						count

let check_math_para_duplicate paralist = 
	List.map (count_para paralist) paralist   

(*a function to check whether a id name is valid when defined*)   
let check_id_valid id mathf_list funcformal env=
        if (exist_mathf id mathf_list)||(exist_func_name id env)||(exist_id id funcformal) then raise(Failure("Math function name: "^ id ^ " has been used!"))
        else true


(*a function to check whether a expr is valid: uses local variables and functions and math functions that has been defined, functions and math functions have parameter that matches*)
let rec valid_expr funcformal expr id_list mathf_list env=
         match expr with
	     |Assign(id, e1) -> if exist_id id id_list 
	                        then let _ =  valid_expr funcformal e1 id_list mathf_list env in id_list
                                else if (check_id_valid id mathf_list funcformal env) then let _ =  valid_expr funcformal e1 id_list mathf_list env in let id_list= id :: id_list in id_list 
                                else raise (Failure("id has been used!")) 
             |Call(fname,exprlist) -> if exist_func_name fname env

				      then let f1 = get_func_by_name fname env in
                                              if (List.length f1.formals == List.length exprlist)
                                              then let _ = List.map (fun e -> valid_expr funcformal e id_list mathf_list env) exprlist in
                                              id_list
                                              else raise(Failure("function parameter not match!"))   
				      else if exist_mathf fname mathf_list
                                           then let (m1,p1)=get_math_fun_by_name fname mathf_list    in
                                            if (List.length p1 == List.length exprlist)
                                                then  let _ = List.map (fun e -> valid_expr funcformal e id_list mathf_list env) exprlist in						 
                                                id_list
                                                else  raise(Failure("math function parameter not match!"))

                                           else raise(Failure("Undefined function or math function: "^ fname ^ " is used!"))
             |Id(id)  -> if exist_id id id_list 
	                        then id_list
                                else raise(Failure("ID : "^ id ^ " not valid!"))

             |Binop (e1,o,e2)-> 
                             ( match o with 
                             Deriv -> ( match e1 with 
                                        Id(id1)  ->  if exist_mathf id1 mathf_list then 
                                                                    ( match e2 with 
                                                                              Id(id2)  ->  let (m1,p1)= get_math_fun_by_name id1 mathf_list in
                                                                                           if exist_id id2 p1 then id_list
                                                                                           else raise(Failure("Derivation is not properly used!"))
                                                                                           
                                                                             | _ -> raise(Failure("Derivation is not properly used!"))
                                                                    )
                                                    
                                                     else  raise(Failure("Derivation is not properly used!"))

                                       | _ -> raise(Failure("Derivation is not properly used!")) 
                                      )
                            
                             |Integ -> ( match e1 with 
                                        Id(id1)  ->  if exist_mathf id1 mathf_list then 
                                                                    ( match e2 with 
                                                                             Call(a,elist)  ->  let (m1,p1)= get_math_fun_by_name id1 mathf_list in
                                                                                           if  exist_id a p1 &&  List.length elist=2 then id_list
                                                                                           else raise(Failure("Derivation is not properly used!"))
                                                                                           
                                                                             | _ -> raise(Failure("Derivation is not properly used!"))
                                                                             
                                                                             )
                                                    
                                                     else  raise(Failure("Derivation is not properly used!"))

                                       | _ -> raise(Failure("Derivation is not properly used!")) )
                             
                             
                             |_ -> let _ =  valid_expr funcformal e1 id_list mathf_list env in  
                                     let _ =valid_expr funcformal e2 id_list mathf_list env in 
                                         id_list  
                             )
             | PreUnaop(o,e2)-> let _ =  valid_expr funcformal e2 id_list mathf_list env in id_list 

             |_ -> id_list 
        

(*a function to check whether the body of math funtion is valid*)   
let rec check_math_func_body_valid mfname paralist id_list mathf_list env expr=
             match expr with
                 |Id(id)  -> if (exist_id id id_list || exist_id id paralist)
	                        then true
                                else if exist_mathf id mathf_list
                                     then let (m1,p1)= get_math_fun_by_name id mathf_list in
                                             if (p1 = paralist) 
                                             then true
                                             else raise(Failure("Using math function that parameter not match!"))

                                     else raise(Failure("Undefined ID : "^ id ^ "is used!"))

                 |Call(fname,exprlist) -> if exist_func_name fname env

				      then let f1 = get_func_by_name fname env in
                                              if (List.length f1.formals == List.length exprlist)
                                              then let _ = List.map (fun e -> check_math_func_body_valid mfname paralist id_list mathf_list env e) exprlist in
                                              true
                                              else raise(Failure("function parameter not match!"))   
				      else if exist_mathf fname mathf_list
                                           then let (m1,p1)= get_math_fun_by_name fname mathf_list in
                                                   if (List.length p1 == List.length exprlist)
                                                   then  let _ = List.map (fun e ->check_math_func_body_valid mfname paralist id_list mathf_list env e) exprlist in						 
                                                   true
                                                   else  raise(Failure("math function parameter not match!"))

                                           else raise(Failure("Undefined function or math function: "^ fname ^ "is used!"))
            
                 | Binop (e1,_,e2)-> let _ = check_math_func_body_valid mfname paralist id_list mathf_list env e1 in
                                     let _ = check_math_func_body_valid mfname paralist id_list mathf_list env e2 in true 
                 | PreUnaop(e1,e2)-> true 
                 |_ -> true


(*a function to check whether the expr in output is valid*)   
let rec check_output_valid paralist id_list mathf_list env expr=
             match expr with
                 |Id(id)  -> if (exist_id id id_list || exist_id id paralist || exist_mathf id mathf_list)

	                     then true
                             else raise(Failure("Invalid ID : "^ id ^ " in return!"))

                 |Call(fname,exprlist) -> if exist_func_name fname env

				      then let f1 = get_func_by_name fname env in
                                              if (List.length f1.formals == List.length exprlist)
                                              then let _ = List.map (fun e ->  check_output_valid paralist id_list mathf_list env e) exprlist in
                                              true
                                              else raise(Failure("function parameter not match!"))   
				      else if exist_mathf fname mathf_list
                                           then let (m1,p1)= get_math_fun_by_name fname mathf_list in
                                                   if (List.length p1 == List.length exprlist)
                                                   then  let _ = List.map (fun e ->check_output_valid paralist id_list mathf_list env e) exprlist in						 
                                                   true
                                                   else  raise(Failure("math function parameter not match!"))

                                           else raise(Failure("Undefined function or math function: "^ fname ^ "is used!"))
            
                 | Binop (e1,o, e2)->          
                          ( match o with 
                             Deriv -> ( match e1 with 
                                        Id(id1)  ->  if exist_mathf id1 mathf_list then 
                                                                    ( match e2 with 
                                                                              Id(id2)  ->  let (m1,p1)= get_math_fun_by_name id1 mathf_list in
                                                                                           if exist_id id2 p1 then true                                                                                                                                                             else raise(Failure("Derivation is not properly used!"))
                                                                                           
                                                                             | _ -> raise(Failure("Derivation is not properly used!"))
                                                                    )
                                                    
                                                     else  raise(Failure("Derivation is not properly used!"))

                                       | _ -> raise(Failure("Derivation is not properly used!")) 
                                      )
                            
                             |Integ -> ( match e1 with 
                                        Id(id1)  ->  if exist_mathf id1 mathf_list then 
                                                                    ( match e2 with 
                                                                             Call(a,elist)  ->  let (m1,p1)= get_math_fun_by_name id1 mathf_list in
                                                                                           if  exist_id a p1 &&  List.length elist=2 then true
                                                                                           else raise(Failure("Integral is not properly used!"))
                                                                                           
                                                                             | _ -> raise(Failure("Integral is not properly used!"))
                                                                             
                                                                             )
                                                    
                                                     else  raise(Failure("Integral is not properly used!"))

                                       | _ -> raise(Failure("Integral is not properly used!")) )
                             
                             
                             |_ -> let _ = check_output_valid paralist id_list mathf_list env e1 in
                                     let _ = check_output_valid paralist id_list mathf_list env e2 in true 

                             )

                                 
                 | PreUnaop(e1,e2)-> true 
                 |_ -> true



(*a function to check whether the body of a func is valid*)          
let check_func_body_valid func env=
     let stmt_lt = func.body in
       let funcformal= func.formals in 
	  let rec f id_list mathf_list stmt_list =
                 if (List.length stmt_list == 0) then true  
                 else let stmt_tl= List.tl stmt_list in
                         match List.hd stmt_list  with
		          |Expr(expr) -> let idl=valid_expr funcformal expr id_list mathf_list env in f idl mathf_list stmt_tl 

                          |Math_func(mfname, paralist ,expr) ->let _= check_math_func_name_valid mfname mathf_list id_list funcformal env in
                                                                (* let _= check_math_func_para_valid mfname  mathf_list id_list funcformal env paralist in*)
                                                                   let _=check_math_para_duplicate paralist in
                                                                     let _= check_math_func_body_valid mfname paralist id_list mathf_list env expr in 
                                                                        let mathf_list=(mfname, paralist)::mathf_list 
                                                                            in f id_list mathf_list stmt_tl   
                          |Return(expr)  -> let idl=valid_expr funcformal expr id_list mathf_list env in f idl mathf_list stmt_tl 

                          |If(expr,stmt1,stmt2)  -> let idl=valid_expr funcformal expr id_list mathf_list env in 
                                                     (f idl mathf_list [stmt1;stmt1]) && (f idl mathf_list [stmt2;stmt2]) && (f id_list mathf_list stmt_tl)
 
                          |For(expr1,expr2,expr3,stmt) -> let idl1=valid_expr funcformal expr1 id_list mathf_list env in 
                                                          let idl2=valid_expr funcformal expr2 idl1 mathf_list env in  
                                                            let idl3=valid_expr funcformal expr3 idl2 mathf_list env in  
                                                                      (f idl3 mathf_list [stmt;stmt]) && (f id_list mathf_list stmt_tl)

                          |While(expr,stmt)  -> let idl = valid_expr funcformal expr id_list mathf_list env in (f idl mathf_list [stmt;stmt]) && (f id_list mathf_list stmt_tl)

                          |Output(expr)  -> let _=check_output_valid funcformal id_list mathf_list env expr in f id_list mathf_list stmt_tl 

                          |Block(stmtlist) -> (f id_list mathf_list stmtlist) && (f id_list mathf_list stmt_tl)
              
         in f func.formals [] stmt_lt




(*a function to check each function's validity*)
let check_func f env =
		let _dup_name = fun_exist f env in
                   let _ = env.functions <- (f) ::env.functions in
		       let _dup_formals = check_fpara_duplicate f in
				   let _vbody = check_func_body_valid f env in
		                            true

         
(*The final function to check the program*)         
let check_program fun_list = 
        let env = {functions = [];} in
	    let _dovalidation = List.map (fun f -> check_func f env) fun_list in
	         let  _mainexist = exists_main env in
		      let _ = print_endline "\nThe semantic check has been finished!\n" in
			  true 
