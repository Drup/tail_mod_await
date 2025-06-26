# Tail mod Async/Await

Implement the "Tail mod Async/Await" transformation as a PPX.
Consider the following piece of code:

```ocaml
let%async rec async_list_map (f : 'a -> 'b Fut.t) = function
  | [] -> []
  | h :: t ->
    await (f h) :: await (async_list_map f t)
```

Where f is an asynchronous function.
With the "Tail mod Async/Await" (or TMAA for short), this function
behaves in constant stack space *and* bounded scheduler queue size.

Furthermore, it can turn tree-traversing function concurrent, yet 
running in constant stack (but not bounded scheduler queue), as the following
example:

```ocaml
type 'a tree = N of 'a * 'a tree * 'a tree | L

let%async rec map f t : 'a tree = match t with
  | L -> L
  | N (v, tl, tr) ->
    N (f v, await (map f tl), await (map f tr))
```

Currently, it relies on [picos](https://github.com/ocaml-multicore/picos)
for a portable implementation of Aync/Await, but the concept is largely
library-independent.

## Running Examples and Tests

Examples are in [examples/](examples/). To run them, first install the dependencies:

```
opam install --deps --with-test . 
```

Then run:

```
dune exec examples/THEEXAMPLE.exe
```

To run all the tests:

```
make test
```

To see the output of the ppx transformation

```
dune describe pp SOMEFILE.ml
```
