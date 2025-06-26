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

If not running under Docker: Need Opam and OCaml 5.3

## Getting Started

### Installation from scratch (Without docker)

With opam installed (takes around 10 minutes): 

```
opam switch create  ocaml.5.3.0 # Create a fresh OCaml switch
opam install --deps -- with-test path/to/artifact # Install all the dependencies
```

Exact dependencies can be found in [ppx_tmaa.opam](ppx_tmaa.opam).

### Basic Usage

Build everything with `make`.

Run tests with `make test`.

## Step by Step Instructions

### Running and Playing with Examples

Examples are in [examples/](examples/). To run one specific examples: 

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
