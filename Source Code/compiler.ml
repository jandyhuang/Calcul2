open Printf

        let lexbuf = Lexing.from_channel (open_in "input.cul") in
        parse_prog=Parser.program Scanner.token lexbuf
        (*Semestics.check_program parse_prog*)

