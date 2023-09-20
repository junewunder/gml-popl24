opam switch create gml 4.14.2+trunk
eval $(opam env --switch=gml)
opam install --deps-only -d -t . -y
opam build
eval $(opam env)
dune build
cp _build/default/src/main.exe ./gml
dune exec -- gml examples/fib.ml
