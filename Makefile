# Mini-C Compiler
# Li-yao Xia
#
## Append > /dev/zero to shut them up

DEP=error.ml ast.mli parser.mly lexer.mll smap.ml \
    typing.ml typing.mli ast_printer.ml iselect.ml
PARSER_GEN=parser.automaton parser.conflicts
BIN=minic

$(BIN):$(DEP) main.ml
	@ocamlbuild -quiet -use-menhir main.native
	@mv main.native $(BIN)

clean:
	@ocamlbuild -clean > /dev/zero
	@rm -rf $(PARSER_GEN) *\~

parser_test: $(DEP) parser_test.ml
	@ocamlbuild -quiet parser_test.native

test: $(BIN) tests/
	@sh test.sh

project:
	@cp Makefile main.ml lexer.mll parser.mly ast.mli error.ml smap.ml sset.ml typing.ml typing.mli mipsofast.ml report/report_compiler.pdf parser_test.ml test.sh xia-liyao/
	@tar -zcf xia-liyao.tgz xia-liyao

