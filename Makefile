MAKEFLAGS += --silent


init:
	echo Creating gml switch...
	rm -rf _opam
	opam switch create . 4.14.0 -y
	eval $(opam env --switch=. --set-switch)

	echo Installing dependencies...
	opam install --deps-only -d -t . -y --switch .
	eval $(opam env --switch=. --set-switch)

	echo Building project...
	dune build

	echo Running example...
	dune exec -- gml paper-examples/map.ml

	echo Done.
	echo Run "eval \$$(opam env --switch=. --set-switch)" to set up your current shell environment
	echo Then run "dune exec -- gml <file>" to run an example

clean:
	rm -rf ./_build
	rm -rf ./_opam
	rm gml.install

