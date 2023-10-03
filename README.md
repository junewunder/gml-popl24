# gml checker (graph meta language)

To run the artifact, follow these steps.

## Install and set up OPAM

Follow the instructions on [the OPAM website][opam].

## Install dependencies

Run `make`.  This creates an OPAM switch called "gml", installs all
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

To silently run all paper examples and testcases, run `tests-silent.sh`

To sequentially run each example and see the graph type outputted, run `tests.sh`. This will wait for user input after each example is ran.

To output the graph visualization for every paper-example, run `make vis`

## Command line options

```
gml [OPTIONS] file
  --time Output timing information
  -t Output timing information
  -i File to output inferred types
  --intf File to output inferred types
  -s Print AST sizes of graph types
  --sizes Print AST sizes of graph types
  -d Run deadlock detection
  --deadlock Run deadlock detection
  -z Output DOT visualization
  --dump-dot Output DOT visualization
  -f Top-level binding to analyze; if unspecified, analyze whole program
  --func Top-level binding to analyze; if unspecified, analyze whole program
  -v Print debugging output
  --verbose Print debugging output
  -nt Supress output of type information
  --no-types Supress output of type information
  --dl-ex Process hardcoded deadlock example #
  -help  Display this list of options
  --help  Display this list of options
```

  [opam]: https://opam.ocaml.org/doc/install.html
