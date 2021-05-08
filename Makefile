.PHONY: clean cleanall all

all: vm blade

vm: expr.cmx commands.cmx processor.cmx eval.cmx parser.cmx scanner.cmx vm.cmx
	ocamlopt -o vm $^

blade: expr.cmx commands.cmx graph.cmx scanner.cmx parser.cmx flow_network.cmx def_use_graph.cmx blade.cmx
	ocamlopt -o blade $^

# du_prova: expr.cmx commands.cmx graph.cmx scanner.cmx parser.cmx def_use_graph.cmx
# 	ocamlopt -o du_prova $^

clean:
	rm -f *.cmx *.mli *.o

cleanall: clean
	rm -f parser.ml scanner.ml *.cmi vm blade

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

vm.cmx: vm.ml expr.cmi eval.cmi parser.cmi scanner.cmi

eval.cmi: eval.ml expr.cmi commands.cmi processor.cmi

processor.cmi: processor.ml expr.cmi commands.cmi

def_use_graph.cmi: def_use_graph.ml graph.cmi expr.cmi commands.cmi parser.cmi scanner.cmi

blade.cmi: blade.ml expr.cmi commands.cmi graph.cmi def_use_graph.cmi flow_network.cmi

%.cmi: %.ml
	ocamlopt -c $<

%.cmx: %.ml %.cmi
	ocamlopt -c $<
