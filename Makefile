MAKEFLAGS += --silent

PAPER_EXAMPLES = paper-examples/blellochproduce paper-examples/list_pi paper-examples/pipeline_pi paper-examples/pipeline_pi_full paper-examples/tree-rev paper-examples/tree

.PHONY: build
build: _build

_build: _opam
	echo Building project...
	./run-in-opam-switch "dune build"

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
	./run-in-opam-switch ./tests.sh

paper-examples/%: _build
	echo "Running $@.ml"
	./run-in-opam-switch 'dune exec -- gml -nt -u 3 -z $@-3.dot $@.ml'
	./run-in-opam-switch 'dune exec -- gml -nt -u 6 -z $@-6.dot $@.ml'
	dot -Tpng $@-3.dot > $@-3.png || (echo "dot failed. Make sure GraphViz is installed"; exit 1)
	dot -Tpng $@-6.dot > $@-6.png || (echo "dot failed. Make sure GraphViz is installed"; exit 1)
	rm $@-3.dot
	rm $@-6.dot

vis: $(PAPER_EXAMPLES)
	echo "PNG visualizations output to paper-examples/"

clean:
	rm -rf ./_build
	rm -rf ./_opam
	rm -f paper-examples/*.out
	rm -f paper-examples/*.png
	rm -f testcases/*.out
	rm -f testcases/*.png
