# Artifact for submitted article "Tail modulo Async/Await"

This artifact implements the full "Tail mod Async/Await" transformation, i.e~
it extends tradtional tail-calls to be available under constructors (similarly
to Tail-modulo-Cons) *and* under awaits in asynchronous functions.
It is implemented in OCaml using a PPX syntax extension.

The artifact requires basic knowledge of OCaml to evaluate usability, but
doesn't require any knowledge to evaluate reproducability of benchmarks.

See also the [Readme](Readme.md).

Concretely, it implements the code transformation from section 3
with both extensions from section 4.
The implementation is described in section 5.1.

The artifact demonstrates:
- The usability of the transformation (through various example and tests)
- That it does reduce space overhead (via memory measurement). This measurements require OCaml's memprof.
- Performance measurements, following the benchmark in section 5.2

Current limitation of our prototype implementation:
- OCaml's unboxed data-types are unsupported.
- Location in error messages might be imprecise.
- Mutually recursive functions have not been rigourously tested.

## Hardware dependencies

For basic usage of the artifact: None.

For memory benchmark: Need Linux.

## Software dependencies

- Opam
- OCaml 5.3

For benchmarks:

- jq >= 1.6
- python3
- matplotlib
- kiwix (particularly 'kiwix-serve', usually from a 'kiwix-tools' package)

## Getting Started

### VM specific instructions

The sources for the artifact are available in the VM image in `~/tail_mod_await`.
All paths are given relative to this directory, and scripts are to be run from it.
All dependencies (system-wide as well as opam dependencies) are installed.
The ZIM file for the webcrawler evaluation is provided in `~/wikipedia_en_top_mini_2025-12.zim`

Benchmarks are configured for a 4-core machine.

### Installation from scratch

With opam installed (takes around 10 minutes):

```
opam switch create ocaml.5.3.0 # Create a fresh OCaml switch
eval $(opam env) # enable the new switch
opam install --deps --with-test path/to/artifact # Install all the dependencies
```

Exact dependencies can be found in [ppx_tmaa.opam](ppx_tmaa.opam).

### Basic Usage

Build everything with `make`.

Run tests with `make test`.

## Step by Step Instructions

### Running and Playing with Examples

Examples are in [examples/](examples/).
In particular, [examples/tree_map_annotated.ml](examples/tree_map_annotated.ml) contains a fully
annotated example with detail explanations and comparison of our transformation.

To run one specific examples:

```
dune exec examples/THEEXAMPLE.exe
```

To observe the output of the ppx transformation directly:

```
dune describe pp examples/THEEXAMPLE.ml
```

You can modify the examples and add new examples freely. If you add new
examples, you should add this to [examples/dune](examples/dune):

```
(executable
 (name my_example)
 (preprocess (pps ppx_tmaa)) # Add more ppx if needed
 (libraries picos_mux.fifo) # Add more dependencies if needed
 (modules my_example)
)
```

### Observing tests

[test/expect_ppx_tmaa.ml](test/expect_ppx_tmaa.ml) contains a series of
expect test of the following format:

```ocaml
"name of the test",
[%expr SOURCE CODE IN ASYNC FUNCTION]
[%expect EXPECTED TRANSFORMED CODE]
```

This allow to observe in a more isolated fashion the code and its transformation, along with various
corner cases.

### Running benchmarks

**Most parameters have been halved compared to the benchmarks in the paper, in order to run in a
more timely manner on "normal" machines.**

#### Performance measurements

This correspond to the paragraph "Time measurements" in Section 5.2.

Benchmarks are available in the [bench/](bench/) directory.
The script [bench/runbench.sh](bench/runbench.sh) runs various
`Tree.map` implementations in various contexts. See the paper for a description
of all the functions. It is run by

```
sh bench/runbench.sh
```

**The number of elements and busy loop iterations has been halved compared to the paper, in order to
run in a more timely manner on "normal" machine.**

These benchmarks are currently configured to run in the provided VM, with 4 cores and 4GB of RAM,
and are expected to take about 1h30 to run. The number of parallel domains and size of the test
cases can be adapted at the top of each `.ml` file of the different benchmarks
By default, they are configured with half as many as the real benchmarks
from the paper.

After the benchmarks are done, the results can be compiled into graphs using
[bench/processdata.sh](bench/processdata.sh), outputing three graphs:
- `synchro.png` corresponding to figure 29,
- `elemscaling.png` corresponding to figure 30.
- `parscaling.png` corresponding to figure 31,

```
sh bench/processdata.sh
```

Full explanations of these results can be found in the paper

#### Webcrawler benchmark

This corresponds to paragraph 5.2.2 "A case study: the webcrawler** in Section 5.2


The script [run_webcrawler_bench.sh](run_webcrawler_bench.sh) runs several variants of a webcrawler
on the provided ZIM file [../wikipedia_en_top_mini_2025-12.zim](../wikipedia_en_top_mini_2025-12.zim) used for our
evaluation. The script is to be ran from the present directory and accepts the following arguments:

```
sh run_webcrwaler_bench.sh <number of cpus> <repeats> <server address>
```

where:
- `<number of cpus>` denotes the maximum number of parallel domains for the parallel webcrawler
  (default 4)
- `<repeats>` the number of repeats per run (default 2)
- `<server address>` the address of a running kiwix server listening on port 8080, serving the
  provided ZIM file

If ran without arguments, the script will run the necessary kiwix server (assuming the ZIM file is
in the parent of the current directory), then run benchmarks with default values.

**This will fetch 5000 pages rather than the 10000 of the paper**

For accurate benchmarks, it is recommended to run the crawler and kiwix-serve
on different machines in the same local network.

#### Memory measurements

This correspond to the paragraph "Memory consumption" in Section 5.2.

The goal of this section is to reproduce the data in Figure 28, which indicates
*memory* measurements of various runs of `map` on trees.

These results are produced by running

```
make membench
```

and printed on Stdout
