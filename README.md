# gml checker (graph meta language)



## install dependencies
before trying to run examples, install opam.

next run `init.sh`, this creates an opam switch called "gml", installs all dependencies, updates your current env, builds the project, and runs the `fib.ml` examples

## running examples
run your example with the following command
```
dune exec -- gml examples/fib.ml
```

in the event of an error, enable stack traces using:
```
OCAMLRUNPARAM=b dune exec -- gml examples/fib.ml
```

to run all tests, run `tests.sh`