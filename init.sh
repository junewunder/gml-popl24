echo Creating gml switch...
opam switch create gml 4.14.2+trunk
eval $(opam env --switch=gml --set-switch)

echo Installing dependencies...
opam install --deps-only -d -t . -y
eval $(opam env)

echo Building project...
dune build

echo Running example...
dune exec -- gml examples/fib.ml

echo Done.
echo Run `eval $(opam env --switch=gml --set-switch)` to set up your current shell environment
