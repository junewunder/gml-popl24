# GMLμ checker (Graph Meta Language)

This artifact accompanies the paper "Pipelines and Beyond: Graph Types for ADTs
with Futures", submitted to POPL 2024.  It implements an engine to infer vertex
structure annotations and graph types for a subset of OCaml, following the
language and the type system presented in the paper.

## Claims made in paper

TODO

This artifact is known to run on the following environments:

- Ubuntu 20.04.6 LTS running on an Intel i7 with 15.2 GiB of RAM
- macOS 13.4.1 on M1 macbook pro with 16GB or RAM

## Download and installation instructions

**Option 1:** Download the VM image located [here][vm]. The image already has
all dependencies installed, and the code pre-built. You can skip to "Evaluation
instructions" below (unless you wish to rebuild the artifact code, in which case
you can proceed with "Install remaining dependencies and build artifact" below).

The VM image has been tested on VirtualBox 7.0, but should be widely compatible.

**Option 2:** Build the source code yourself. Navigate to the `gml-popl24`
directory and follow these steps:

DOWNLOAD instructions

### Necessary dependencies

The following need to be installed before building the artifact:

* [The OPAM package manager][opam] >= 2.1.
* [GraphViz][graphviz] (only needed for reproducing the figures from the paper)

All other dependencies will be installed locally by our build script.

Suitable versions of these are pre-installed on the VM image.

### Install remaining dependencies and build artifact

Run `make`.  This creates a local opam switch, installs all dependencies, sets
up your shell environment, and compiles the GMLμ checker.

After running this command, run `eval $(opam env --switch=. --set-switch)` to
finish setting up the shell environment.
**NOTE:** This command sets up the environment only in the current shell;
you will need to re-run this command any time you open a new shell.

To reproduce the figures from the paper, you will also need to install
[GraphViz][graphviz] (if testing on your own machine; GraphViz is already
installed on the VM image).

The test cases under "Run test cases" below serve as a good sanity check that
the artifact has built correctly.

## Evaluation instructions

All commands should be run from within the `gml-popl24` directory.
In the VM image, this is `~/gml-popl24`.

### Run test cases

To silently run all paper examples and testcases, run `make test`
(or alternatively `tests.sh`).

#### Expected behavior

The code should:
 - run the GMLμ checker on every file in the `paper-examples` and `testcases` folder
 - for every file, print `running <file>... success`
 - if the test prints "success", this means that the checker passed the test without erroring -- note that this does not indicate correctness of the inferred graph type
 - if the test prints "failure", this means that the checker encountered an error somewhere during checking

### Output visualizations for paper examples

*Note:* Visualizing the examples requires installing [GraphViz][graphviz]

The `paper-examples` folder contains code for the examples contained in the
paper. To output the graph visualization for every paper example,
run `make vis`.

#### Expected behavior

The code should:
 - silently run `gml` on every file in the `paper-examples` and `testcases` folders
 - for `paper-examples/EXAMPLE.ml`, output a visualization of the main program's
   graph type in `paper-examples/EXAMPLE.dot`
 - run GraphViz (if installed) to output the visualization as
   `paper-examples/EXAMPLE.pdf`
 - delete `paper-examples/EXAMPLE.dot`

| figure # | paper-examples file            |
|----------|--------------------------------|
| Fig 1.   | `list_pi.ml`, `pipeline_pi.ml` |
| Fig 28.  | `blellochproduce.ml`           |
| Fig 29.  | `tree.ml`                      |

TODO: Maybe move some of this to "Additional information"

### Run test cases interactively (optional)

To interactively run each example, use `tests-interactive.sh`.
This will wait for user input after each example is run.

#### Expected behavior

The code should, for every file in the `paper-examples` and `testcases` folder:
  - print the name of the file being checked
  - print the program to the terminal
  - run the checker, printing the output graph types from the program
  - wait for the user to press the enter button

### Run individual examples

You can check individual examples with the following command
```
dune exec -- gml <gml file>
```

See "Command line options" below for more usage information on the command.

In the event of an error with the checker, enable OCaml stack traces using:
```
OCAMLRUNPARAM=b dune exec -- gml <gml file>
```
## Additional information

### File structure

TODO

### Visualizing your own programs

If you would like to write your own GML program and output the visualization for it then run the following commands:
```
dune exec -- gml -nt -z program.dot program.ml
dot -Tpng program.dot > program.png
open program.png
```

### Command line options

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
  [vm]: MISSING LINK
