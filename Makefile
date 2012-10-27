# Mini-C Compiler
# Li-yao Xia
#

CMO=lexer.cmo parser.cmo parser_test.cmo
GENERATED = lexer.ml parser.ml parser.mli
BIN=parser_test

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
	menhir -v $<

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
