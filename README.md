## decimal

Arbitrary-precision floating-point decimal type implemented in OCaml. Ported
from Python
[`decimal` module](https://github.com/python/cpython/blob/23831a7a90956e38b7d70304bb6afe30d37936de/Lib/_pydecimal.py#L1157).
It uses Zarith to do biginteger arithmetic.

## License

This package is licensed under the
[Python Software Foundation License v2](https://github.com/python/cpython/blob/23831a7a90956e38b7d70304bb6afe30d37936de/LICENSE#L73),
for the sake of simplicity, as it is a derived work of the Python `decimal` module.

## Examples

    # (* for convenience *)
      module D = Decimal
      let i = D.of_int
      let s = D.of_string;;
    #
      #install_printer D.pp;;
    #
      D.(s "0.1" + s "0.2");;
    - : D.t = 0.3
