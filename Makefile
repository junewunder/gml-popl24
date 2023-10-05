MAKEFLAGS += --silent

PAPER_EXAMPLES = paper-examples/blellochproduce paper-examples/list_pi paper-examples/map paper-examples/pipeline_pi paper-examples/pipeline_pi_full paper-examples/qsort paper-examples/tree

.PHONY: build
build: _build

_build: _opam
	echo Building project...
	dune build

	echo Done.
	echo Run "eval \$$(opam env --switch=. --set-switch)" to set up your current shell environment
	echo Then run "dune exec -- gml <file>" to run an example

.PHONY: init
init: _opam

_opam:
	echo Creating gml switch...
	rm -rf _opam
	opam switch create . 4.14.0 -y
	eval $(opam env --switch=. --set-switch)

	echo Installing dependencies...
	opam install --deps-only -d -t . -y --switch .
	eval $(opam env --switch=. --set-switch)

test: _build
	./tests.sh

paper-examples/%: _build
	echo "Running $@.ml"
	dune exec -- gml -nt -z $@.dot $@.ml
	dot -Tpng $@.dot > $@.pdf || (echo "dot failed. Make sure GraphViz is installed"; exit 1)
	rm $@.dot

vis: $(PAPER_EXAMPLES)
	echo "PDF visualizations output to paper-examples/"

clean:
	rm -rf ./_build
	rm -rf ./_opam

