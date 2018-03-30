SOURCES = webPlot.ml web.ml
RESULT = hyper
PACKS = unix,graphics,js_of_ocaml,js_of_ocaml.syntax
CMO = $(filter-out ../hyper.cmo, $(wildcard ../*.cmo))

all: hyper
	ocamlfind ocamlc -annot -I .. $(CMO) -package $(PACKS) -syntax camlp4o -linkpkg -o $(RESULT) $(SOURCES)
	js_of_ocaml $(RESULT)

hyper:
	$(MAKE) -C .. dcl

.PHONY: hyper
