.PHONY: clean cleanall

prova: eval.cmx parser.cmx scanner.cmx prova.cmx
	ocamlopt -o prova $^ 

clean:
	rm -f *.cmx *.cmi *.o

cleanall: clean
	rm -f parser.ml scanner.ml *.mli prova

parser.mli: parser.mly
	ocamlyacc $<

parser.ml: parser.mly
	ocamlyacc $<

scanner.ml: scanner.mll
	ocamllex $<

scanner.cmi: scanner.ml parser.cmi
	ocamlopt -c $<

parser.cmi: parser.mli parser.ml eval.cmi
	ocamlopt -c $<

prova.cmx: prova.ml eval.cmi parser.cmi scanner.cmi
	ocamlopt -c $<

%.cmi: %.ml
	ocamlopt -c $<

%.cmx: %.ml %.cmi
	ocamlopt -c $<
