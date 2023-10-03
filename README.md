# gml checker (graph meta language)

To run the artifact, follow these steps.

## Install and set up OPAM 

Follow the instructions on [the OPAM website][opam].

## Install dependencies

Run `init.sh`.  This script creates an OPAM switch called "gml", installs all
dependencies, updates your environment, builds the project, and runs the
`fib.ml` examples.

## Running examples

Run your example with the following command
```
dune exec -- gml examples/fib.ml
```

In the event of an error, enable stack traces using:
```
OCAMLRUNPARAM=b dune exec -- gml examples/fib.ml
```

To run all tests, run `tests.sh`

  [opam]: https://opam.ocaml.org/doc/install.html
