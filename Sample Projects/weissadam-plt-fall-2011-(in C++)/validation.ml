open Ast
open Stringofsetups
open Setuptypes

(* Author: Bill Warner whw2108@columbia.edu, Adam Weiss *)

let empty_sc =
  let fsig = {fformals = []; freturn = NoType} in
  let fvars = (Hashtbl.create 3) in
  FunctionContext({ fname = ""; fsig = fsig;  fvars = fvars; has_return = false})

let rec var_collide var_name symbols =
  match symbols with
  | [] -> false
  | hd :: tl -> if Hashtbl.mem hd var_name then true else var_collide var_name tl

let var_add var_name var_type ifcl =
  match ifcl with
  | FunctionContext(fc) -> Hashtbl.add fc.fvars var_name var_type
  | Callers(fc, fcl) -> Hashtbl.add fc.fvars var_name var_type

exception UndeclaredVariable of string

let rec var_get var_name fcl =
  match fcl with
  | FunctionContext(fc) -> 
       (try Hashtbl.find fc.fvars var_name
       with _ -> raise(UndeclaredVariable(var_name)))
  | Callers(c, cl) ->
	try Hashtbl.find c.fvars var_name
	with _ -> var_get var_name cl
	  
exception MultipleDecls of string * string

let validate_formals formals fcl =
  let ctx =
    match fcl with
    | FunctionContext(c) -> c
    | Callers(c, _) -> c
  in 
      List.iter (fun pair -> Hashtbl.add ctx.fvars (snd pair) (fst pair)) formals;

exception Unimplemented of string
exception TypeError of string
exception NotASet of string

let compute_return_type_plus lhs rhs =
    match lhs, rhs with
      FloatType, FloatType   -> FloatType
    | FloatType, IntType     -> FloatType
    | IntType,   FloatType   -> FloatType
    | IntType,   IntType     -> IntType
    | StringType, StringType -> StringType
    | _                    -> raise (TypeError("bad plus args"))

let compute_return_type_minus lhs rhs =
    match lhs, rhs with
    | FloatType, FloatType -> FloatType
    | FloatType, IntType   -> FloatType
    | IntType,   FloatType -> FloatType
    | IntType,   IntType   -> IntType
    | _                    -> raise (TypeError("bad minus args"))

let compute_return_type_times lhs rhs =
    compute_return_type_minus lhs rhs

let compute_return_type_ndivide lhs rhs =
    match lhs, rhs with
    | FloatType, FloatType -> IntType
    | FloatType, IntType   -> IntType
    | IntType,   FloatType -> IntType
    | IntType,   IntType   -> IntType
    | _                    -> raise (TypeError("bad ndivide args"))

let compute_return_type_divide lhs rhs =
    match lhs, rhs with
    | FloatType, FloatType -> FloatType
    | FloatType, IntType   -> FloatType
    | IntType,   FloatType -> FloatType
    | IntType,   IntType   -> FloatType
    | _                    -> raise (TypeError("bad divide args"))

let compute_return_type_comparator lhs o rhs =
    match lhs, rhs with
    | FloatType, FloatType -> BoolType
    | FloatType, IntType   -> BoolType
    | IntType,   FloatType -> BoolType
    | IntType,   IntType   -> BoolType
    | StringType, StringType -> BoolType
    | _                    -> raise (TypeError("bad comparator"))

let compute_return_type_equality lhs rhs =
  match lhs, rhs with
  | FloatType, FloatType   -> BoolType
  | FloatType, IntType     -> BoolType
  | IntType,   FloatType   -> BoolType
  | IntType,   IntType     -> BoolType
  | StringType, StringType -> BoolType
  | TupleType, TupleType   -> BoolType
  | SetType,   SetType     -> BoolType
  | TupleSeq(l), TupleSeq(r) ->
      if l = r then BoolType else raise (TypeError("bad equality args"))
  | SetSeq(l), SetSeq(r) ->
      if l = r then BoolType else raise (TypeError("bad equality args"))
  | _                    -> raise (TypeError("bad equality args"))

let compute_return_type_boolop lhs rhs =
  match lhs, rhs with
  | BoolType, BoolType   -> BoolType
  | _                    -> raise (TypeError("bad args to boolop"))

let compute_return_type_setop lhs rhs =
  match lhs, rhs with
  | SetType, SetType -> lhs
  | SetType, SetSeq(_) -> rhs
  | SetSeq(_), SetType -> lhs
  | SetSeq(_), SetSeq(_) -> lhs
(*  | SetSeq(l), SetSeq(r) ->
      if l = r then lhs else raise (TypeError("bad setop args: " ^
					     (string_of_typespec l)
					     ^ " and " ^
					     (string_of_typespec r))) *)
  | _                -> raise (TypeError("bad setop args: " ^
					     (string_of_typespec lhs)
					     ^ " and " ^
					     (string_of_typespec rhs))) 

let rec compute_return_type_from_binop e1 o e2 fc symbols =
  (* print_endline ("before: " ^ (string_of_expr e1) ^ " " ^ (string_of_expr e2)); *)
  let lt = (compute_return_type_with_context e1 fc symbols) in
  let rt = (compute_return_type_with_context e2 fc symbols) in
  (* print_endline ("after: " ^ (string_of_typespec lt) ^ " " ^ (string_of_typespec rt)); *)
  match o with
    Plus	-> compute_return_type_plus lt rt
  | Minus	-> compute_return_type_minus lt rt
  | Times	-> compute_return_type_times lt rt
  | Divide	-> compute_return_type_divide lt rt
  | NDivide	-> compute_return_type_ndivide lt rt
  | GrThan
  | LeThan
  | GrThanEq
  | LeThanEq	-> compute_return_type_comparator lt o rt
  | Equals
  | NotEquals	-> compute_return_type_equality lt rt
  | LogicalAnd
  | LogicalOr	-> compute_return_type_boolop lt rt
  | Union
  | Cross	-> compute_return_type_setop lt rt
  | SetMinus
  | Intersect	-> compute_return_type_setop lt rt

and

  compute_return_type_from_unnop u e fc symbols =
  let et = (compute_return_type_with_context e fc symbols) in
  match u with
    Negative	-> compute_return_type_neg et
  | Not		-> compute_return_type_negation et
  | Cardinality -> compute_return_type_cardinality et

and

  compute_return_type_neg e =
  match e with
    IntType
  | FloatType	-> e
  | _		->
	raise(TypeError("negative sign (-) can't be applied to type " ^ (string_of_typespec e)))

and

  compute_return_type_negation e =
  match e with
    BoolType	-> e
  | _		->
	raise(TypeError("negation (~) can't be applied to type " ^ (string_of_typespec e)))

and

  compute_return_type_cardinality e =
  match e with
  | SetType
  | TupleType
  | SetSeq(_) -> IntType
  | _		 ->
      raise(TypeError("cardinality (#) can't be applied to type " ^ (string_of_typespec e)))

and

 compute_return_type expr =
  match expr with
    NullExpr		-> NoType
  | Int(i)		-> IntType
  | Boolean(i)		-> BoolType
  | Float(i)		-> FloatType
  | String(i)		-> StringType
  | _			-> raise (Unimplemented("cannot determine type for: " ^ string_of_expr expr))

and

 compute_return_type_of_setrange starte ende fc symbols =
  let startt = compute_return_type_with_context starte fc symbols in
  let endt   = compute_return_type_with_context ende fc symbols in
  match startt, endt with
    IntType, IntType -> SetSeq(IntType)
  | _                ->
      raise(TypeError("ranges are defined by two integers!"))
and

 compute_return_type_with_context expr fc symbols =
  match expr with
  | Id(i)               ->
      var_get i fc
  | FuncCall(s, el)     ->
      let called_ctx = 
	try Hashtbl.find symbols.functions s
	with _ ->
	  raise(TypeError("function " ^ s ^ " is not defined!")) in
      (match called_ctx with
      | FunctionContext(pc)
      | Callers(pc, _) ->
	  pc.fsig.freturn)
  | Tuple(e)		-> compute_return_type_of_tuple e fc symbols
  | SetLiteral(e)	-> compute_and_validate_return_type_of_setliteral e fc symbols
  | Binop(e1,o,e2)	-> compute_return_type_from_binop e1 o e2 fc symbols
  | Unop(u,e)		-> compute_return_type_from_unnop u e fc symbols
  | Assign(i,e)		-> compute_return_type_with_context e fc symbols
  | SetRange(s,e)	-> compute_return_type_of_setrange s e fc symbols
  | SetBuilder(e,s)	-> validate_setbuilder e s fc symbols
  | _			-> compute_return_type expr

 and

 compute_type_of_set expr sl ?(sc=empty_sc) fc symbols =
  List.iter (fun s ->
    match s with
      vname, expr ->
	let st =
	  match expr with
	  | SetRange(s,e) ->
	      (match compute_return_type_of_setrange s e fc symbols with
		SetSeq(IntType) -> SetSeq(IntType)
	      | _          ->
		  raise(TypeError("ranges are defined by two integers!")))
	  | SetLiteral(e) ->
	      ( match 
		(compute_and_validate_return_type_of_setliteral
		   e fc symbols) with
              | SetSeq(t) -> t
	      | _ -> raise(Unimplemented("over my dead body")))
	  | SetBuilder(e,s) ->
	      (var_add vname NoType sc;
	       let sc = push_fc sc sc in
	       compute_type_of_set e s ~sc:sc fc symbols)
	  | Id(i) ->
	      (match (try
		(var_get i sc) with _ ->
		  (var_get i fc)) with
	      | SetSeq(nt) -> nt
	      | _ -> raise(TypeError(vname ^ " must be bound to a set!")))
	  | _ ->
	      raise(NotASet("expression is not a set: " ^ (string_of_expr expr)))
	in
	(var_add vname st sc)
	    ) sl;
  try (compute_return_type_with_context expr sc symbols)
  with _ -> (compute_return_type_with_context expr fc symbols)

and

 validate_setbuilder expr sourcelist fc symbols =
  let st = compute_type_of_set expr sourcelist fc symbols in
  match sourcelist with
  | hd::tl              ->
      (match hd with
      | (_, sexpr) ->
	  let st = compute_return_type_with_context sexpr fc symbols in
	  match st with
	  | SetSeq(_) -> validate_setbuilder expr tl fc symbols
	  | _       ->
	      raise(TypeError("source list expression '" ^ (string_of_expr sexpr)  ^ "' must be a set")))
  | []                  -> SetSeq(st)

and

 compute_return_type_of_tuple ?(acc=TupleSeq([])) e fc symbols =
    match e with
    | fe::el ->
	let elm = compute_return_type_with_context fe fc symbols in
	let nacc =
	  (match acc with
	  | TupleSeq(ts) -> TupleSeq(elm::ts)
	  | _ -> raise(TypeError("Wait, What?"))) in
	compute_return_type_of_tuple ~acc:nacc el fc symbols
    | [] -> acc

and

 compute_and_validate_return_type_of_setliteral el ?(et=NoType) fc symbols =
  
  match el with
    hd::tl ->
      let t = compute_return_type_with_context hd fc symbols in
      let st = SetSeq(t) in
      if et == NoType || st = et (* here, = is deep equality *)
      then compute_and_validate_return_type_of_setliteral tl ~et:st fc symbols
      else raise (TypeError("mismatched types in set literal"))
  | []     -> et

let validate_assignment i et fc =
  let vt = (var_get i fc) in
  if et = vt
  then ()
  else
    match et, vt with
      SetSeq(_), SetType
    | SetType, SetType
    | SetType, SetSeq(_) -> ()
    | SetSeq(e), SetSeq(v) ->
	if e = v
	then ()
	else
	  raise(TypeError("can't assign " ^ string_of_typespec et ^ " to " ^ string_of_typespec vt))
    | _ -> raise(TypeError("can't assign " ^ string_of_typespec et ^ " to " ^ string_of_typespec vt))

let rec validate_funccall s el fc symbols =
  match (try Hashtbl.find symbols.functions s with _ ->
    raise (TypeError(s ^ ": function not defined!"))) with
    _ -> ()

let validate_expr expr fc symbols =
  match expr with
    Assign(i, e)    -> validate_assignment i (compute_return_type_with_context e fc symbols) fc
  | FuncCall(s, el) -> validate_funccall s el fc symbols
  | _               -> ()

let double_decl_check var_name var_type fc =
  match fc with
    FunctionContext(c)
  | Callers(c, _) ->
      if Hashtbl.mem c.fvars var_name
      then raise(TypeError(var_name ^ " was already declared!"))
      else var_add var_name var_type fc

let decl_check_then_add decl_type var_name expr_type fc symbols =
  match decl_type, expr_type with
  | SetSeq(l), SetSeq(r) ->
      (if l = r
      then double_decl_check var_name decl_type fc
      else raise (TypeError("bad equality args")))
  | IntType, IntType
  | FloatType, FloatType
  | StringType, StringType
  | BoolType, BoolType
  | SetType, SetType
  | _, NoType ->
      double_decl_check var_name decl_type fc
  | SetType, SetSeq(r) ->
      double_decl_check var_name expr_type fc
  | TupleSeq(l), TupleSeq(r) ->
      if l = r then
	double_decl_check var_name expr_type fc
  | _, _ ->
      raise (TypeError("type mismatch: var: " ^
		       var_name ^ " declared type: " ^
		       string_of_typespec decl_type ^
		       " expression type: " ^ string_of_typespec expr_type))

let validate_return e fc symbols =
  let et = (compute_return_type_with_context e fc symbols)

  in

  let ft =
    match fc with
    | FunctionContext(c)
    | Callers(c,_) ->
	(c.has_return <- true; c.fsig.freturn)
  in

  match ft, et with
    IntType, IntType
  | FloatType, FloatType
  | StringType, StringType
  | BoolType, BoolType
  | SetType, SetType
  | SetType, SetSeq(_) -> ()
  | SetSeq(l), SetSeq(r) ->
      (if( l = r) then () else
      raise(TypeError("return type mismatch: " ^ string_of_typespec l ^ " " ^ (string_of_typespec r))))
  | _ ->
      raise(TypeError("return type mismatch: " ^ string_of_typespec ft ^ " " ^ (string_of_typespec et)))

let rec validate_stmt stmt fc symbols =
  match stmt with
    Block(b)		->
      validate_stmt_list b fc symbols
  | Decl(t, i, e)	->
      decl_check_then_add t i (compute_return_type_with_context e fc symbols) fc symbols
  | Expr(e)		->
      validate_expr e fc symbols
  | Return(e)		->
      validate_return e fc symbols
  | If(e, t, f)         -> validate_if_stmt e t f fc symbols
  | While(e, b)         -> validate_while_stmt e b fc symbols
  | Print(e)            -> validate_expr e fc symbols

and

 validate_stmt_list b fc symbols =
  List.iter (fun s -> validate_stmt s fc symbols) b;

and

validate_function_block b fc symbols =
  validate_stmt_list b fc symbols;
  match fc with
    FunctionContext(c)
  | Callers(c, _) ->
      if c.has_return
      then ()
      else raise(TypeError("function " ^ c.fname ^ " contains no return statement"))

and

 validate_while_stmt e b fc symbols =
  if (compute_return_type_with_context e fc symbols) != BoolType
  then raise(TypeError("while conditional must be boolean: " ^ (string_of_expr e)))
  else
    validate_stmt b fc symbols
  
and

 validate_if_stmt e t f fc symbols =
  if ((compute_return_type_with_context e fc symbols) != BoolType)
  then raise(TypeError("if conditional must be boolean: " ^ (string_of_expr e)))
  else
    (validate_stmt t fc symbols;
     validate_stmt f fc symbols)

let validate_funcdef symbols fd global_fc =

  let fsig = {fformals = fd.formals; freturn = fd.ret_type} in
  let fvars = (Hashtbl.create 3) in
  let fc = FunctionContext({ fname = fd.func_name;
                             fsig = fsig;
                             fvars =  fvars;
                             has_return = false}) in
  let lfc = push_fc fc global_fc
  in
  Hashtbl.add symbols.functions fd.func_name fc;

  validate_formals fd.formals lfc;
  validate_stmt fd.body lfc symbols;()


let validate_program p =
  let global_fn = "**GLOBAL**" in
  let global_fc = FunctionContext({ fname = global_fn;
                                    fsig={fformals = []; freturn = NoType};
                                    fvars = (Hashtbl.create 3);
                                    has_return = false}) in
  let symbols = {functions = Hashtbl.create 3} in
  validate_stmt_list p.globals global_fc symbols;
  List.iter (fun f -> validate_funcdef symbols f global_fc) p.funcs;
  Hashtbl.add symbols.functions global_fn global_fc;
  symbols
