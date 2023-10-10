# GMLμ checker (Graph Meta Language)

This artifact accompanies the paper "Pipelines and Beyond: Graph Types for ADTs
with Futures", submitted to POPL 2024.  It implements an engine to infer vertex
structure annotations and graph types for a subset of OCaml, following the
language and the type system presented in the paper.

## Claims made in paper

All of the claims concerning the artifact are in Section 6 of the paper,
"Implementation and Examples". In this section, we claim that the
implementation, called GMLμ, accepts ordinary, unannotated programs in a
subset of OCaml extended with keywords `future` and `touch` for spawning and
forcing futures, and annotates the program with graph types according to the
type system of Section 3. GMLμ can output the inferred types of the program
to the console, and can also produce a visualization of the graph type.

We claim that GMLμ can handle, and produce visualizations for,

* The pipelining example of Figures 1 and 2 (in Section 1)
* The producer-consumer example of [Blelloch and Reid-Miller 1997] (Fig. 28)
* Sum of a pipelined tree, also reminiscent of [Blelloch and Reid-Miller 1997]
  (Fig. 29)
* Reversal of the pipelined tree.


## Download and installation instructions

The artifact is intended to be built and run on a Unix system or VM, but
will also likely work on [WSL][wsl].
This artifact is known to run on the following environments:

- Ubuntu 20.04.6 LTS running on an Intel i7 with 15.2 GiB of RAM
- macOS 13.4.1 on M1 macbook pro with 16GB of RAM

Download the tarball from Zenodo. Untar it using, e.g.

    tar -xvf gml-popl24

This will extract the artifact to a new directory called `gml-popl24`.
Navigate to this directory and follow these steps:

### Necessary dependencies

The following need to be installed before building the artifact:

* [The OPAM package manager][opam] >= 2.1.
* [GraphViz][graphviz] (only needed for reproducing the figures from the paper)

All other dependencies will be installed locally by our build script.

### Install remaining dependencies and build artifact

Run `make`.  This creates a local opam switch, installs all dependencies, sets
up your shell environment, and compiles the GMLμ checker. (**NOTE:** The
compilation process might issue some warnings when compiling the GMLμ
parser. You can safely ignore those.) The first time you run it, this process
will take several minutes.

After running this command, run `eval $(opam env --switch=. --set-switch)` to
finish setting up the shell environment.
**NOTE:** This command sets up the environment only in the current shell;
you will need to re-run this command any time you open a new shell.

The test cases under "Run test cases" below serve as a good sanity check that
the artifact has built correctly.

## Evaluation instructions

All commands should be run from within the `gml-popl24` directory.

### Run test cases

To silently run all paper examples and testcases, run `make test`
(or alternatively `tests.sh`).

#### Expected behavior

The code should:
 - run the GMLμ checker on every file in the `paper-examples` and `testcases` folder
 - for every file, print `running <file>... success`
 - if the test prints "success", this means that the checker ran on the test
   without errors. The inferred types are stored in `<file>.out`. For reference,
   the expected outputs for all of the `paper-examples` and `testcases` are
   provided in the respective subfolders of `expected_out`.
 - if the test prints "failure", this means that the checker encountered an error somewhere during checking

### Output visualizations for paper examples

*Note:* Visualizing the examples requires installing [GraphViz][graphviz]

The `paper-examples` folder contains code for the examples contained in the
paper. To output the graph visualization for every paper example,
run `make vis`.

The visualization tool comes with a parameter, which is the number of times to
unroll the graph type before visualizing. A higher number generates more of
the graph, which can help in discerning the pattern more clearly, but also
results in a larger, more cluttered visual. Our test script runs each
visualization with two different values for this parameter, 3 and 6.

#### Expected behavior

The code should:
 - silently run `gml` on every file in the `paper-examples` and `testcases` folders
 - for `paper-examples/EXAMPLE.ml`, output a visualization of the main program's
   graph type in `paper-examples/EXAMPLE-{3, 6}.dot`
 - run GraphViz (if installed) to output the visualization as
   `paper-examples/EXAMPLE-{3, 6}.png`
 - delete `paper-examples/EXAMPLE-{3, 6}.dot`

Four of the visualizations generated are included in the paper. The following
table gives the visualization generated and where it appears in the paper.

| Figure/section # | `paper-examples` visual              |
|------------------|--------------------------------------|
| Fig 1.           | `list_pi-6.png`, `pipeline_pi-6.png` |
| Fig 28.          | `blellochproduce-6.png`              |
| Fig 29.          | `tree-3.png`                         |

In addition, `tree-rev.ml` and its visualizations are mentioned in Section 6
(under "Tree Reverse") but the visualizations are not included in the body
of the paper.

### Run test cases interactively (optional)

To interactively run each example, use `tests-interactive.sh`.
This will wait for user input after each example is run.

#### Expected behavior

The code should, for every file in the `paper-examples` and `testcases` folder:
  - print the name of the file being checked
  - print the program to the terminal
  - run the checker, printing the output graph types from the program
  - wait for the user to press the enter button


## Additional information

### File structure

The files and subdirectories of the artifact are listed below:

| File/Directory         | Purpose                                          |
|------------------------|--------------------------------------------------|
| `expected_out`         | Expected outputs of `make test`                  |
| `paper-examples`       | Source code for examples included in the paper   |
| `src`                  | Source code for GMLμ                             |
| `testcases`            | Code for additional examples used for testing    |
| `README.md`            | This file                                        |
| `tests.sh`             | Run all test cases (also invoked by `make test`) |
| `tests-interactive.sh` | Run test cases interactively                     |

Important source code files are described below:

* `ast.ml`: Abstract syntax tree definitions
* `grUtil.ml`: Utility functions including context management,
  substitution, and inference of vertex structure types (Section 5)
* `inferGr.ml`: Main code for graph type inference
* `inferTy.ml`: OCaml type inference; this runs as a separate pass before
  graph type inference
* `main.ml`: Parsing of command line arguments and driver functions

### Run individual examples

You can check individual examples (either those provided or your own)
with the following command
```
dune exec -- gml <gml file>
```

If you would like to output the visualization, you can run
```
dune exec -- gml -nt -z <output>.dot <gml file>.ml
dot -Tpng <output>.dot > <output>.png
```

See "Command line options" below for more usage information on the command.

In the event of an error with the checker, enable OCaml stack traces using:
```
OCAMLRUNPARAM=b dune exec -- gml <gml file>
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
  -u Number of times to normalize for visualization. Default: 3
  --unroll Number of times to normalize for visualization. Default: 3
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
  [wsl]: https://learn.microsoft.com/en-us/windows/wsl/install