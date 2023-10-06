# gml checker (graph meta language)

To run the artifact, follow these steps.

## Install and set up OPAM

Follow the instructions on [the OPAM website][opam].

## Install dependencies

Run `make`.  This creates a local opam switch, installs all
dependencies, updates your environment, and builds the project.

After running this command, run `eval $(opam env --switch=. --set-switch)` to
finish setting up the environment.
**NOTE:** This command sets up the environment only in the current shell;
you will need to re-run this command any time you open a new shell.

## Running examples

### Run test cases

To silently run all paper examples and testcases, run `make test`
(or alternatively `tests.sh`).

What to expect:
 - runs every file in the `paper-examples` and `testcases` folder
 - for every file, prints `running <file>... success`
 - if "success", then the checker passed the test without erroring -- note that this does not indicate correctness of the inferred graph type
 - if "failure", the checker encountered an error somewhere during checking

### Output visualizations for paper examples

*Note:* Visualizing the examples requires installing [GraphViz][graphviz]

The `paper-examples` folder contains code for the examples contained in the
paper. To output the graph visualization for every paper example,
run `make vis`.

What to expect:
 - silently runs `gml` on every file in the `paper-examples` and `testcases` folder
 - for `paper-examples/EXAMPLE.ml`, outputs a visualization of the main program's
   graph type in `paper-examples/EXAMPLE.dot`
 - runs GraphViz (if installed) to output the visualization as
   `paper-examples/EXAMPLE.pdf`
 - deletes `paper-examples/EXAMPLE.dot`

| figure # | paper-examples file        |
|----------|----------------------------|
| Fig 1.   | list_pi.ml, pipeline_pi.ml |
| Fig 28.  | blellochproduce.ml         |
| Fig 29.  | tree.ml                    |
|          |                            |
|          |                            |
|          |                            |
|          |                            |

### Run test cases interactively (optional)

To interactively run each example, use `tests-interactive.sh`.
This will wait for user input after each example is run.

What to expect:
 - for every file in the `paper-examples` and `testcases` folder:
 - print the name of the file being run
 - print the program to the terminal
 - runs the checker, printing the outputted graph types from the program
 - waits for the user to press the enter button

### Run individual examples

Run your example with the following command
```
dune exec -- gml <gml file>
```

See "Command line options" below for more usage information on the command.

In the event of an error with the checker, enable OCaml stack traces using:
```
OCAMLRUNPARAM=b dune exec -- gml <gml file>
```

## Visualizing your own programs

If you would like to write your own GML program and output the visualization for it then run the following commands:
```
dune exec -- gml -nt -z program.dot program.ml
dot -Tpng program.dot > program.png
open program.png
```

## Command line options

```
gml [OPTIONS] file
  --time Output timing information
  -t Output timing information
  -i File to output inferred types
  --intf File to output inferred types
  -s Print AST sizes of graph types
  --sizes Print AST sizes of graph types
  -z Output DOT visualization
  --dump-dot Output DOT visualization
  -f Top-level binding to analyze; if unspecified, analyze whole program
  --func Top-level binding to analyze; if unspecified, analyze whole program
  -v Print debugging output
  --verbose Print debugging output
  -nt Supress output of type information
  --no-types Supress output of type information
  -help  Display this list of options
  --help  Display this list of options
```

  [opam]: https://opam.ocaml.org/doc/install.html
  [graphviz]: https://graphviz.org/