open Printf
	let _ =
        let lexbuf = Lexing.from_channel (open_in "input.cul") in
        let parse_prog = Parser.program Scanner.token lexbuf in
        Semantics.check_program parse_prog

