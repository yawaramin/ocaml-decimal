## decimal

Arbitrary-precision floating-point decimal type implemented in OCaml. Ported
from Python
[`decimal` module](https://github.com/python/cpython/blob/23831a7a90956e38b7d70304bb6afe30d37936de/Lib/_pydecimal.py).
It uses Zarith to do biginteger arithmetic.

Opam: https://ocaml.org/p/decimal

## License

This package is licensed under the
[Python Software Foundation License v2](https://github.com/python/cpython/blob/23831a7a90956e38b7d70304bb6afe30d37936de/LICENSE#L73),
for the sake of simplicity, as it is a derived work of the Python `decimal` module.

## Examples

See next section (below) for `dune` instructions.

    $ opam install decimal # if trying out
    # (* if trying out: *)
      #require "decimal";;
      #require "ppx_decimal";;
    # (* tell the REPL how to display decimals *)
      #install_printer Decimal.pp;;
    # (* PPX gives convenience literal syntax *)
      Decimal.(0.1m + 0.2m);;
    - : Decimal.t = 0.3
    # (* default precision is 32 *)
      Decimal.(of_int 1 / of_int 3);;
    - : Decimal.t = 0.33333333333333333333333333333333
    # (* round to decimal places: *)
      Decimal.(round ~n:2 (of_int 22 / of_int 7));;
    - : Decimal.t = 3.14

## Use with dune

In your `dune` file's `executable` or similar stanza:

```
 (libraries decimal)
 (preprocess (pps ppx_decimal))
```

## Dev

Build:

    dune build

Try in REPL:

    dune utop

## Tests

The test runner source is in `test/decimal_test.ml`. It parses and runs the test
cases in `test/data/`. I am adding test case data files from the
[Python snapshot](https://github.com/python/cpython/tree/23831a7a90956e38b7d70304bb6afe30d37936de/Lib/test/decimaltestdata)
as I go.

Run current tests:

    dune test

Note that, some of the tests don't make sense for the OCaml port and have thus
been deleted. If you ever need to update to new versions of the test files, you
can apply the changes as patches after re-dowloading the relevant `*.decTest`
files:

    git show 07074859567e936b8d170aba5ef58889a4d9d467 | git apply
    git show ae0196377fb7a99db7f198f2fb242e6a2fe4541e | git apply
