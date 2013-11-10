open Ast
open Stringofsetups
(* Author Bill Warner *)
exception ContextError of string

type function_signature = { fformals : (typespec * string) list; freturn : typespec }

type function_context = { fname: string; fsig : function_signature; fvars : (string, typespec) Hashtbl.t; mutable has_return : bool}

type function_context_link = FunctionContext of function_context | Callers of function_context * function_context_link

type execution_context = { evars : (string, string) Hashtbl.t; }

type execution_context_link = ExecutionContext of execution_context | ECallers of execution_context * execution_context_link

let push_fc fc fcl =
  match fc with
  | FunctionContext(c) -> Callers(c, fcl)
  | Callers(c, l) -> raise(ContextError("do you really want to push a call stack onto a call stack?"))

let push_ec ec ecl =
  match ec with
  | ExecutionContext(c) -> ECallers(c, ecl)
  | ECallers(c, l) -> raise(ContextError("do you really want to push a call stack onto a call stack?"))

type symbol_table = { functions : (string, function_context_link) Hashtbl.t }

let print_fc ?(indent="") fc =
  print_endline (indent ^ "fname: " ^ fc.fname);
  Hashtbl.iter (fun k v -> print_endline(indent ^ k ^ " " ^ string_of_typespec v))
    fc.fvars

let rec print_fcl ?(indent="") fcl =
  match fcl with
  | FunctionContext(fc) -> print_fc fc ~indent:indent
  | Callers(fc, fcl) -> print_fc fc ~indent:indent; print_fcl fcl ~indent:(indent ^ "  ")

let rec print_typespec ?(indent="") tup =
  match tup with
  | TupleSeq(l) ->
      print_endline("(");
      let ind = indent ^ "  " in 
      List.iter (fun t -> print_typespec ~indent:ind t) l;
      print_endline ")"
  | _ -> print_endline( indent ^ (string_of_typespec tup))

