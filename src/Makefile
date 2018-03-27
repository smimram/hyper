SOURCES = stdlib.ml graph.ml hyper.ml
RESULT = hyper
ANNOTATE = true

all: dnc
	./hyper

ci:
	git ci . -m "Hypergraph rewriting."
	git push

include OCamlMakefile
