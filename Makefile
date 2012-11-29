# Mini-C Compiler
# Li-yao Xia
#

CMX=error.cmx lexer.cmx parser.cmx typing.cmx
GENERATED=lexer.ml parser.ml parser.mli 
PARSER_GEN=parser.automaton parser.conflicts
BIN=minic

$(BIN):$(CMX) main.ml
	@ocamlopt -o $(BIN) $(CMX) main.ml


%.cmi: %.mli
	@ocamlopt -c $<

%.cmx: %.ml
	@ocamlopt -c $<

%.ml : %.mll
	@ocamllex $<

%.ml : %.mly
	@menhir -v --infer $<

.depend depend:$(GENERATED)
	@rm -f .depend
	@ocamldep *.ml *.mli > .depend

clean:
	@rm -f *.cm[ix] *.o *~ $(GENERATED) $(PARSER_GEN)


parser_test:$(cmx) parser_test.cmx
	@ocamlopt -o parser_test $(cmx) parser_test.ml

parser.ml: ast.cmi

typing.cmx: typing.cmi

test: $(BIN)
	-@sh test.sh

project:
	@cp Makefile main.ml lexer.mll parser.mly ast.mli error.ml typing.ml typing.mli report/report_compiler.pdf parser_test.ml test.sh xia-liyao/
	@tar -zcf xia-liyao.tgz xia-liyao

include .depend
