SOURCES = stdlib.ml UID.ml graph.ml common.ml plot.ml lang.ml hyper.ml
RESULT = hyper
PACKS = unix graphics
ANNOTATE = true
DOC_FILES = $(SOURCES)

all: dnc
	OCAMLRUNPARAM=b ./hyper

ci:
	git ci . -m "Hypergraph rewriting."
	git push

include OCamlMakefile
