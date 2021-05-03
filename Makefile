.PHONY: clean cleanall

prova: expr.cmx commands.cmx processor.cmx eval.cmx parser.cmx scanner.cmx prova.cmx
	ocamlopt -o prova $^

du_prova: expr.cmx commands.cmx graph.cmx scanner.cmx parser.cmx def_use_graph.cmx
	ocamlopt -o du_prova $^

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

prova.cmx: prova.ml expr.cmi eval.cmi parser.cmi scanner.cmi

eval.cmi: eval.ml expr.cmi commands.cmi processor.cmi

processor.cmi: processor.ml commands.cmi

def_use_graph.cmi: def_use_graph.ml graph.cmi expr.cmi commands.cmi parser.cmi scanner.cmi

%.cmi: %.ml
	ocamlopt -c $<

%.cmx: %.ml %.cmi
	ocamlopt -c $<
