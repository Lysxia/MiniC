# Mini-C Compiler
# Li-yao Xia
#
## Append > /dev/zero to shut them up

SRC=smap.ml sset.ml \
		error.ml lexer.mll parser.mly ast.mli typing.mli typing.ml \
		iselect.mli iselect.ml print_ist.ml mips.mli mips.ml \
		ast_printer.ml print_mips.ml main.ml
PARSER_GEN=parser.automaton parser.conflicts
BIN=minic

$(BIN): $(SRC)
	@ocamlbuild -quiet -use-menhir main.native
	@mv main.native $(BIN)

clean:
	@ocamlbuild -clean > /dev/zero
	@rm -rf $(PARSER_GEN) *\~ *.s

parser_test: parser_test.ml
	@ocamlbuild -quiet parser_test.native

test: $(BIN) utest/ktst.c
	./$(BIN) utest/ktst.c

project:
	cp Makefile main.ml lexer.mll parser.mly ast.mli error.ml smap.ml sset.ml \
		typing.ml typing.mli mipsofast.ml report/rapport.txt \
	  test.sh iselect.ml iselect.mli mips.ml mips.mli \
		ast_printer.ml print_ist.ml print_mips.ml print_mips.mli \
		xia-liyao/
	tar -zcf xia-liyao.tgz xia-liyao
