open Ast
open Stringofsetups
open Validation
open Setuptypes

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let symbols = validate_program program in
  Hashtbl.iter (
  fun fname fcl ->
    (* print_fcl fcl *)
    let fctx =
      match fcl with
      | FunctionContext(c) -> c
      | Callers(c, l) -> c in
    Hashtbl.iter
      (fun k v -> print_endline (k ^ " " ^ (string_of_typespec v)))
      fctx.fvars
      )
    symbols.functions
