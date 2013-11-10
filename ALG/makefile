all: AST.cmi compiler
compiler: CodeGen.cmo Semantics.cmo ALG_Parser.cmo ALG_Scanner.cmo compiler.cmo 
	ocamlc  -o compiler CodeGen.cmo Semantics.cmo ALG_Parser.cmo ALG_Scanner.cmo  compiler.cmo

compiler.cmo : compiler.ml
	ocamlc -c compiler.ml

ALG_Scanner.cmo : ALG_Scanner.ml
	ocamlc -c ALG_Scanner.ml

ALG_Scanner.ml : ALG_Scanner.mll
	ocamllex ALG_Scanner.mll
ALG_Parser.cmo:	ALG_Parser.ml
	ocamlc -c ALG_Parser.ml
ALG_Parser.ml:	ALG_Parser.mli
	ocamlc -c ALG_Parser.mli
ALG_Parser.mli:  ALG_Parser.mly
	ocamlyacc ALG_Parser.mly

Semantics.cmo: Semantics.ml
	ocamlc -c Semantics.ml
	
CodeGen.cmo:CodeGen.ml
	ocamlc -c CodeGen.ml



AST.cmi: AST.mli
	ocamlc -c AST.mli	
clean: 
	rm -f *.cmo *.cmi ALG_Parser.mli ALG_Parser.ml ALG_Scanner.ml AST.cmi
