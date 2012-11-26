# Mini-C Compiler
# Li-yao Xia
#

CMO=error.cmo lexer.cmo parser.cmo typing.cmo
GENERATED=lexer.ml parser.ml parser.mli 
PARSER_GEN=parser.automaton parser.conflicts
BIN=minic

$(BIN):$(CMO) main.cmo
	@ocamlc -o $(BIN) $(CMO) main.cmo


.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	@ocamlc -c $<

.ml.cmo:
	@ocamlc -c $<

.mll.ml:
	@ocamllex $<

.mly.ml:
	@menhir -v --infer $<

.depend depend:$(GENERATED)
	@rm -f .depend
	@ocamldep *.ml *.mli > .depend

clean:
	@rm -f *.cm[io] *.o *~ $(GENERATED) $(PARSER_GEN)

all: $(BIN) parser_test

parser_test:$(CMO) parser_test.cmo
	@ocamlc -o parser_test $(CMO) parser_test.ml

parser.ml: ast.cmi

typing.cmo: typing.cmi

project:
	@cp Makefile main.ml lexer.mll parser.mly ast.mli error.ml typing.ml typing.mli report/report_compiler.pdf parser_test.ml xia-liyao/
	@tar -zcf xia-liyao.tgz xia-liyao

include .depend
