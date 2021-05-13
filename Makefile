.PHONY: clean cleanall all

all: vm vm_nocache blade

vm: expr.cmx commands.cmx utils.cmx eval.cmx processor.cmx parser.cmx scanner.cmx vm.cmx
	ocamlopt -o vm $^

vm_nocache: expr.cmx commands.cmx utils.cmx eval.cmx processor.cmx parser.cmx scanner.cmx vm_nocache.cmx
	ocamlopt -o vm_nocache $^

blade: expr.cmx commands.cmx graph.cmx scanner.cmx parser.cmx flow_network.cmx def_use_graph.cmx blade.cmx
	ocamlopt -o blade $^

# du_prova: expr.cmx commands.cmx graph.cmx scanner.cmx parser.cmx def_use_graph.cmx
# 	ocamlopt -o du_prova $^

clean:
	rm -f *.cmx *.mli *.o

cleanall: clean
	rm -f parser.ml scanner.ml *.cmi vm vm_nocache blade

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

vm.cmx: vm.ml processor.cmi expr.cmi eval.cmi parser.cmi scanner.cmi
vm-nocache.cmx: vm.ml processor.cmi expr.cmi eval.cmi parser.cmi scanner.cmi

eval.cmi: eval.ml expr.cmi commands.cmi utils.cmi

utils.cmi: utils.ml expr.cmi commands.cmi

processor.cmi: processor.ml expr.cmi commands.cmi utils.cmi eval.cmi

def_use_graph.cmi: def_use_graph.ml graph.cmi expr.cmi commands.cmi parser.cmi scanner.cmi

blade.cmi: blade.ml expr.cmi commands.cmi graph.cmi def_use_graph.cmi flow_network.cmi

%.cmi: %.ml
	ocamlopt -c $<

%.cmx: %.ml %.cmi
	ocamlopt -c $<
