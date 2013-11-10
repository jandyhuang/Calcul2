open Printf

	let _ =
	let lexbuf = Lexing.from_channel (open_in "input.alg") in
	let parse_prog=ALG_Parser.program ALG_Scanner.token lexbuf in
	let _=Semantics.check_program parse_prog in
	let prog=CodeGen.gen_program parse_prog in
	let out_channel = open_out "output.c" in
	output_string out_channel prog
