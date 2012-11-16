# Mini-C Compiler
# Li-yao Xia
#

CMO=error.cmo lexer.cmo parser.cmo typing.cmo
GENERATED=lexer.ml parser.ml parser.mli
PARSER_GEN=parser.automaton parser.conflicts
BIN=main

$(BIN):$(CMO) main.cmo
	ocamlc -o $(BIN) $(CMO) $(BIN).cmo


.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc -c $<

.ml.cmo:
	ocamlc -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v --infer $<

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

clean:
	rm -f *.cm[io] *.o *~ $(GENERATED) $(PARSER_GEN)

#
#TESTING
#

parser_test:$(CMO) parser_test.cmo
	ocamlc -o parser_test $(CMO) parser_test.ml

parser.ml: ast.cmi

typing.cmo: typing.cmi

include .depend
