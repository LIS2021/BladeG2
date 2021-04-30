.PHONY: clean cleanall

prova: expr.cmx commands.cmx processor.cmx eval.cmx parser.cmx scanner.cmx prova.cmx
	ocamlopt -o prova $^

clean:
	rm -f *.cmx *.mli *.o

cleanall: clean
	rm -f parser.ml scanner.ml *.cmi prova

parser.mli: parser.mly
	ocamlyacc $<

parser.ml: parser.mly
	ocamlyacc $<

scanner.ml: scanner.mll
	ocamllex $<

scanner.cmi: scanner.ml parser.cmi
	ocamlopt -c $<

parser.cmi: parser.mli parser.ml commands.cmi
	ocamlopt -c $<

prova.cmx: prova.ml eval.cmi parser.cmi scanner.cmi expr.cmi
	ocamlopt -c $<

eval.cmi: eval.ml expr.cmi commands.cmi processor.cmi
	ocamlopt -c $<

processor.cmi: processor.ml commands.cmi
	ocamlopt -c $<

%.cmi: %.ml
	ocamlopt -c $<

%.cmx: %.ml %.cmi
	ocamlopt -c $<
