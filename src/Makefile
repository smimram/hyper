SOURCES = stdlib.ml UID.ml graph.ml hyper.ml
RESULT = hyper
ANNOTATE = true
DOC_FILES = $(SOURCES)

all: dnc
	OCAMLRUNPARAM=b ./hyper

ci:
	git ci . -m "Hypergraph rewriting."
	git push

include OCamlMakefile
