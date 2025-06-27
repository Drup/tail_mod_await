# Artifact for submitted article "Tail modulo Async/Await"

This artifact implements the full "Tail mod Async/Await" transformation, i.e~
it extends tradtional tail-calls to be available under constructors (similarly
to Tail-modulo-Cons) *and* under awaits in asynchronous functions.
It is implemented in OCaml using a PPX syntax extension.

The artifact requires basic knowledge of OCaml to evaluate usability, but
doesn't require any knowledge to evaluate reproducability of benchmarks.

See also the [Readme](Readme.md).

Concretely, it implements the code transformation describe in section 4.1
which subsumes both transformation presented in section 2 and section 3.
The implementation is described in section 4.2.

The artifact demonstrates:
- The usability of the transformation (through various example and tests)
- That it does reduce space overhead (via memory measurement). This measurements require OCaml's memprof.
- Early performance measurements

Current limitation of our prototype implementation:
- OCaml's unboxed data-types are unsupported.
- Location in error messages might be imprecise.
- Mutually recursive functions have not been rigourously tested.

Note that the paper is in Major revision, and that most of the requirements for revision relates to Experimental Evaluation, which is likely to change significantly in the next version. The core implementation is unlikely to change.

## Hardware dependencies

For basic usage of the artifact: None.

For memory benchmark: Need Linux.

## Software dependencies

- Opam
- OCaml 5.3

For performance benchmark:

- jq
- python
- mathplotlib

For memory benchmark:

- An alternative switch with ocaml 5.1
- Linux
- A web browser
- gmp

## Getting Started

### Installation from scratch (Without docker)

With opam installed (takes around 10 minutes):

```
opam switch create ocaml.5.3.0 # Create a fresh OCaml switch
eval $(opam env) # enable the new switch
opam install --deps --with-test path/to/artifact # Install all the dependencies
opam switch create --no-switch memtrace_viewer --packages ocaml.5.1.1,memtrace_viewer.v0.17.0 # Alternative switch for memory viewer
```

Exact dependencies can be found in [ppx_tmaa.opam](ppx_tmaa.opam).

### Docker-specific instructions

This artifact is shipped with a docker image [tail-mod-await-image.tar](tail-mod-await-image.tar).

To run it:

```
docker load < tail-mod-await-image.tar.gz
docker run -p 8080:8080 -it tail-mod-await-image:latest
```

### Basic Usage

Build everything with `make`.

Run tests with `make test`.

## Step by Step Instructions

### Running and Playing with Examples

Examples are in [examples/](examples/).
In particular, [examples/tree_map_annotated.ml](examples/tree_map_annotated.ml) contains a fully annotated example with detail explanations and comparison of our transformation.

To run one specific examples:

```
dune exec examples/THEEXAMPLE.exe
```

The example [examples/icfp_example.ml](examples/icf_example.ml) contains
detailed step by step instructions on how the transformation works, and
how to use it.

To observe the output of the ppx transformation directly:

```
dune describe pp examples/THEEXAMPLE.exe
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

This allow to observe in a more isolated fashion the code and its transformation, along with various corner cases.

### Performance measurements

This correspond to the paragraph "What is the effect of Tail-Modulo-Cons-Await on performances and parallelism?" in Section 4.3.

Benchmarks are available in the [bench/](bench/) directory.
The script [bench/bench_tree.ml](bench/bench_tree.ml) runs various
`Tree.map` implementations in various contexts. See the paper for a description
of all the functions.
One can adjust the degree of parallelism by modifying the `n_domains` variable
at the beginning of the file. It is 4 by default.

You can run it with:

```
make bench
```

It is expected to take between 5 and 10 minutes.

It outputs two graphs:
- `short.png` corresponding to figure 32.a.
  It maps a quick function. This allow to test the influence of synchronisation.
- `long.png` corresponding to figure 32.b.
  It maps a long function. This allows to observe parallelism.

One should observe that:
- Dashed lines (with our transformation) are equivalent or below plain lines.
- The black `map_tmc` function, which is the non-async implementation of map
  is much faster in the `short` case (as there is no synchronisation).
- The various `_par` benefits from parallelism: they run faster by a factor
of `n_domains` (4 by default).

### Memory measurements

This correspond to the paragraph "What is the effect of Tail-Modulo-Cons-Await on memory consumption?" in Section 4.3.

The goal of this section is to reproduce Figure 31, which indicates
*memory* measurements of various runs of `map` on lists.

First run the benchmark with memory tracing to produce a file `trace.ctf`

```
make benchmem
```

Unfortunately, the viewer for such trace files requires an old OCaml version.
We strongly recommend you use the provided Docker.
Assuming the correct switch with `memtrace_viewer` is installed (either manually via the setup above, or thanks to our Docker), run the following command:

```
opam exec --switch memtrace_viewer -- memtrace-viewer trace.ctf
```

It will open a web-based UI, accessible at [localhost:8080](localhost:8080) on your host machine, where you can observe the graph on the left.
It should look exactly like figure 31. In particular, all peaks should not be above the first peak, indicating they don't consume extra memory than
the Tail-Modulo-Cons variant.
