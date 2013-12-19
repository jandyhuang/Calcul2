open Printf
	let _ =
        let lexbuf = Lexing.from_channel stdin in
        let parse_prog = Parser.program Scanner.token lexbuf in
        (* let _ = Semantics.check_program parse_prog in*)
        let prog = Codegen.gen_program parse_prog in
        let output_cpp = open_out "output.cpp" in
        output_string output_cpp prog

