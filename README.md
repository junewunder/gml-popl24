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
dune exec -- gml paper-examples/map.ml
```

In the event of an error with the checker, enable ocaml stack traces using:
```
OCAMLRUNPARAM=b dune exec -- gml paper-examples/map.ml
```

To silently run all paper examples and testcases, run `tests.sh`.
what to expect:
 - runs every file in the `paper-examples` and `testcases` folder
 - for every file, `running <file>... success`
 - if "success", then the checker passed the test without erroring -- note that this does not indicate correctness of the inferred graph type
 - if "failure", the checker encountered an error somewhere during checking

To interactively run each example, use `tests-interactive.sh`. This will wait for user input after each example is ran.
what to expect:
 - for every file in the `paper-examples` and `testcases` folder:
 - print the name of the file being ran
 - print the program to the terminal
 - runs the checker, printing the outputted graph types from the program
 - waits for the user to press the enter button

To output the graph visualization for every paper-example, run `make vis`
what to expect:
 - for every file in the `paper-examples` and `testcases` folder:
 - run the graph checker silently
 - output the programs' .dot files
 - compile these to pdf with `dot`

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
