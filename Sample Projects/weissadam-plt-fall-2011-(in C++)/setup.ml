open Ast
open Stringofsetups
(* Author Adam Weiss *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let result = string_of_program program in
  print_endline (result)
