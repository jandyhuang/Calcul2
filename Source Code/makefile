all: ast.cmi compiler
compiler: parser.cmo scanner.cmo semantics.cmo codegen.cmo compiler.cmo
	ocamlc -o calcul parser.cmo scanner.cmo semantics.cmo codegen.cmo compiler.cmo

compiler.cmo: compiler.ml
	ocamlc -c compiler.ml

codegen.cmo: codegen.ml
	ocamlc -c codegen.ml

semantics.cmo: semantics.ml
	ocamlc -c semantics.ml

scanner.cmo: scanner.ml
	ocamlc -c scanner.ml

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.cmo: parser.ml
	ocamlc -c parser.ml

parser.ml: parser.mli
	ocamlc -c parser.mli

parser.mli: parser.mly
	ocamlyacc parser.mly

ast.cmi: ast.ml
	ocamlc -c ast.ml

clean:
	rm -f parser.mli parser.ml scanner.ml ast.cmi testall.log calcul \
	output output.cpp *.cmo *.cmi *.out
