open Printf
	let _ =
        let lexbuf = Lexing.from_channel (open_in "input.cul") in
        let parse_prog = Parser.program Scanner.token lexbuf in
        let _ = Semantics.check_program parse_prog in
        let prog = Codegen.gen_program parse_prog in
        print_endline prog

