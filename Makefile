# Mini-C Compiler
# Li-yao Xia
#

CMO=error.cmo lexer.cmo parser.cmo typing.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli
PARSER_GEN=parser.automaton parser.conflicts
BIN=main

$(BIN):$(CMO)
	ocamlc -o $(BIN) $(CMO)


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

parser.ml: ast.cmi

typing.cmo: typing.cmi

include .depend
