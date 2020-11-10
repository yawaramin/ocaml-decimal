type rounding_mode =
| Down
| Half_up
| Half_even
| Ceiling
| Floor
| Half_down
| Zero_five_up

type flag =
| Clamped
| Invalid_operation
| Conversion_syntax
| Division_impossible
| Division_undefined
| Inexact
| Rounded
| Subnormal
| Overflow
| Underflow

type t = {
  prec : int;
  rounding_mode : rounding_mode;
  emax : int;
  emin : int;
  capitals : bool;
  clamp : bool;
}

let default = ref {
  prec = 32;
  rounding_mode = Half_even;
  emax = 999_999;
  emin = -999_999;
  capitals = true;
  clamp = false;
}

let set_default = (:=) default
let default () = !default

let etiny { prec; emin; _ } = emin - prec + 1
let etop { prec; emax; _ } = emax - prec + 1
