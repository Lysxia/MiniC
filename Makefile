# Mini-C Compiler
# Li-yao Xia
#
# 
#

parser_test.out : parser.ml ast.mli
	ocamlc parser_test -o parser_test.out

main.ml : parser.ml lexer.ml ast.cmi

parser.ml : parser.mly ast.cmi
	menhir -v parser.mly

lexer.ml : lexer.mll
	ocamllex lexer.mll

ast.cmi : ast.mli
	ocamlc -c ast.mli
