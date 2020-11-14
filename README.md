## decimal

Arbitrary-precision floating-point decimal type implemented in OCaml. Ported
from Python
[`decimal` module](https://github.com/python/cpython/blob/23831a7a90956e38b7d70304bb6afe30d37936de/Lib/_pydecimal.py#L1157).
Using Zarith to do biginteger arithmetic.

## Examples

    # (* for convenience *)
      module D = Decimal
      let d = D.of_string;;
    #
      #install_printer D.pp;;
    #
      D.(d "0.1" + d "0.2");;
    - : D.t = 0.3
