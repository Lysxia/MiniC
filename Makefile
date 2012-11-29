# Mini-C Compiler
# Li-yao Xia
#
## Append > /dev/zero to shut them up

DEP=error.ml ast.mli parser.mly lexer.mll smap.mli smap.ml typing.ml mipsofast.ml
PARSER_GEN=parser.automaton parser.conflicts
BIN=minic

$(BIN):$(DEP) main.ml
	@ocamlbuild -quiet -use-menhir main.native
	@mv main.native minic

clean:
	@ocamlbuild -clean > /dev/zero
	@rm -rf $(PARSER_GEN) *\~

parser_test: $(DEP) parser_test.ml
	@ocamlbuild -quiet parser_test.native

test: $(BIN)
	@sh test.sh

project:
	@cp Makefile main.ml lexer.mll parser.mly ast.mli error.ml smap.ml smap.mli typing.ml typing.mli report/report_compiler.pdf parser_test.ml test.sh mipsofast.ml xia-liyao/
	@tar -zcf xia-liyao.tgz xia-liyao

